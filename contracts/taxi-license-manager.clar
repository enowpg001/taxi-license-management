;; taxi-license-manager
;; Manages taxi licenses with driver certification and vehicle inspection tracking

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-input (err u103))
(define-constant err-expired (err u105))
(define-constant err-suspended (err u106))

;; Data Variables
(define-data-var license-counter uint u0)
(define-data-var driver-counter uint u0)
(define-data-var vehicle-counter uint u0)
(define-data-var inspection-counter uint u0)

;; Data Maps
(define-map taxi-licenses
  uint
  {
    license-number: (string-ascii 50),
    driver-id: uint,
    vehicle-id: uint,
    issue-date: uint,
    expiry-date: uint,
    status: (string-ascii 20),
    issued-by: principal
  }
)

(define-map drivers
  uint
  {
    name: (string-ascii 100),
    license-number: (string-ascii 50),
    certification-date: uint,
    certification-expiry: uint,
    background-check: bool,
    active: bool,
    registered-at: uint
  }
)

(define-map vehicles
  uint
  {
    plate-number: (string-ascii 20),
    make: (string-ascii 50),
    model: (string-ascii 50),
    year: uint,
    vin: (string-ascii 50),
    insurance-expiry: uint,
    registered-at: uint
  }
)

(define-map vehicle-inspections
  uint
  {
    vehicle-id: uint,
    inspector: principal,
    inspection-date: uint,
    passed: bool,
    next-inspection-due: uint,
    notes: (optional (string-utf8 500))
  }
)

(define-map driver-licenses
  uint
  (list 10 uint)
)

(define-map vehicle-inspection-history
  uint
  (list 50 uint)
)

(define-map authorized-inspectors principal bool)

;; Authorization Functions
(define-public (add-inspector (inspector principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-set authorized-inspectors inspector true))
  )
)

(define-public (remove-inspector (inspector principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-delete authorized-inspectors inspector))
  )
)

(define-read-only (is-inspector (user principal))
  (default-to false (map-get? authorized-inspectors user))
)

;; Driver Management
(define-public (register-driver (name (string-ascii 100))
                                 (license-number (string-ascii 50))
                                 (certification-expiry uint)
                                 (background-check bool))
  (let
    ((driver-id (+ (var-get driver-counter) u1)))
    (asserts! (or (is-eq tx-sender contract-owner) (is-inspector tx-sender)) err-unauthorized)
    (asserts! (> (len name) u0) err-invalid-input)
    (asserts! (> certification-expiry block-height) err-invalid-input)
    
    (map-set drivers driver-id {
      name: name,
      license-number: license-number,
      certification-date: block-height,
      certification-expiry: certification-expiry,
      background-check: background-check,
      active: true,
      registered-at: block-height
    })
    
    (var-set driver-counter driver-id)
    (ok driver-id)
  )
)

(define-public (update-driver-status (driver-id uint) (active bool))
  (let
    ((driver (unwrap! (map-get? drivers driver-id) err-not-found)))
    (asserts! (or (is-eq tx-sender contract-owner) (is-inspector tx-sender)) err-unauthorized)
    
    (ok (map-set drivers driver-id (merge driver {active: active})))
  )
)

(define-read-only (get-driver (driver-id uint))
  (map-get? drivers driver-id)
)

;; Vehicle Management
(define-public (register-vehicle (plate-number (string-ascii 20))
                                  (make (string-ascii 50))
                                  (model (string-ascii 50))
                                  (year uint)
                                  (vin (string-ascii 50))
                                  (insurance-expiry uint))
  (let
    ((vehicle-id (+ (var-get vehicle-counter) u1)))
    (asserts! (or (is-eq tx-sender contract-owner) (is-inspector tx-sender)) err-unauthorized)
    (asserts! (> (len plate-number) u0) err-invalid-input)
    (asserts! (> insurance-expiry block-height) err-invalid-input)
    
    (map-set vehicles vehicle-id {
      plate-number: plate-number,
      make: make,
      model: model,
      year: year,
      vin: vin,
      insurance-expiry: insurance-expiry,
      registered-at: block-height
    })
    
    (var-set vehicle-counter vehicle-id)
    (ok vehicle-id)
  )
)

(define-read-only (get-vehicle (vehicle-id uint))
  (map-get? vehicles vehicle-id)
)

;; Vehicle Inspection
(define-public (conduct-vehicle-inspection (vehicle-id uint)
                                            (passed bool)
                                            (next-due uint)
                                            (notes (optional (string-utf8 500))))
  (let
    (
      (inspection-id (+ (var-get inspection-counter) u1))
      (vehicle (unwrap! (map-get? vehicles vehicle-id) err-not-found))
      (history (default-to (list) (map-get? vehicle-inspection-history vehicle-id)))
    )
    (asserts! (or (is-eq tx-sender contract-owner) (is-inspector tx-sender)) err-unauthorized)
    (asserts! (> next-due block-height) err-invalid-input)
    
    (map-set vehicle-inspections inspection-id {
      vehicle-id: vehicle-id,
      inspector: tx-sender,
      inspection-date: block-height,
      passed: passed,
      next-inspection-due: next-due,
      notes: notes
    })
    
    (map-set vehicle-inspection-history vehicle-id
      (unwrap! (as-max-len? (append history inspection-id) u50) err-invalid-input))
    
    (var-set inspection-counter inspection-id)
    (ok inspection-id)
  )
)

(define-read-only (get-inspection (inspection-id uint))
  (map-get? vehicle-inspections inspection-id)
)

(define-read-only (get-vehicle-inspection-history (vehicle-id uint))
  (map-get? vehicle-inspection-history vehicle-id)
)

;; License Management
(define-public (issue-license (license-number (string-ascii 50))
                               (driver-id uint)
                               (vehicle-id uint)
                               (expiry-date uint))
  (let
    (
      (license-id (+ (var-get license-counter) u1))
      (driver (unwrap! (map-get? drivers driver-id) err-not-found))
      (vehicle (unwrap! (map-get? vehicles vehicle-id) err-not-found))
      (driver-history (default-to (list) (map-get? driver-licenses driver-id)))
    )
    (asserts! (or (is-eq tx-sender contract-owner) (is-inspector tx-sender)) err-unauthorized)
    (asserts! (get active driver) err-suspended)
    (asserts! (> expiry-date block-height) err-invalid-input)
    (asserts! (> (get certification-expiry driver) block-height) err-expired)
    
    (map-set taxi-licenses license-id {
      license-number: license-number,
      driver-id: driver-id,
      vehicle-id: vehicle-id,
      issue-date: block-height,
      expiry-date: expiry-date,
      status: "active",
      issued-by: tx-sender
    })
    
    (map-set driver-licenses driver-id
      (unwrap! (as-max-len? (append driver-history license-id) u10) err-invalid-input))
    
    (var-set license-counter license-id)
    (ok license-id)
  )
)

(define-public (update-license-status (license-id uint) (new-status (string-ascii 20)))
  (let
    ((license (unwrap! (map-get? taxi-licenses license-id) err-not-found)))
    (asserts! (or (is-eq tx-sender contract-owner) (is-inspector tx-sender)) err-unauthorized)
    
    (ok (map-set taxi-licenses license-id (merge license {status: new-status})))
  )
)

(define-public (renew-license (license-id uint) (new-expiry uint))
  (let
    ((license (unwrap! (map-get? taxi-licenses license-id) err-not-found)))
    (asserts! (or (is-eq tx-sender contract-owner) (is-inspector tx-sender)) err-unauthorized)
    (asserts! (> new-expiry block-height) err-invalid-input)
    
    (ok (map-set taxi-licenses license-id 
      (merge license {expiry-date: new-expiry, status: "active"})))
  )
)

(define-read-only (get-license (license-id uint))
  (map-get? taxi-licenses license-id)
)

(define-read-only (get-driver-licenses (driver-id uint))
  (map-get? driver-licenses driver-id)
)

;; Counter Functions
(define-read-only (get-license-counter)
  (ok (var-get license-counter))
)

(define-read-only (get-driver-counter)
  (ok (var-get driver-counter))
)

(define-read-only (get-vehicle-counter)
  (ok (var-get vehicle-counter))
)

(define-read-only (get-inspection-counter)
  (ok (var-get inspection-counter))
)
