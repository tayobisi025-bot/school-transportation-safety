;; School Transportation Safety Contract
;; Student transport platform with route monitoring, driver certification, vehicle maintenance, and emergency communication

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-data (err u103))
(define-constant err-expired-certification (err u104))
(define-constant err-vehicle-not-active (err u105))

;; Data Maps
(define-map drivers
  { driver-id: uint }
  {
    driver-principal: principal,
    name: (string-ascii 100),
    license-number: (string-ascii 50),
    certification-type: (string-ascii 50),
    certification-expiry: uint,
    background-check-date: uint,
    status: (string-ascii 20),
    total-violations: uint
  }
)

(define-map vehicles
  { vehicle-id: uint }
  {
    bus-number: (string-ascii 20),
    capacity: uint,
    last-inspection: uint,
    next-maintenance: uint,
    mileage: uint,
    fuel-efficiency: uint,
    status: (string-ascii 20),
    assigned-routes: uint
  }
)

(define-map routes
  { route-id: uint }
  {
    route-name: (string-ascii 100),
    driver-id: uint,
    vehicle-id: uint,
    start-location: (string-ascii 200),
    end-location: (string-ascii 200),
    estimated-duration: uint,
    student-count: uint,
    safety-rating: uint,
    active-status: bool
  }
)

(define-map students
  { student-id: uint }
  {
    name: (string-ascii 100),
    parent-contact: (string-ascii 100),
    emergency-contact: (string-ascii 100),
    route-id: uint,
    pickup-location: (string-ascii 200),
    grade-level: uint,
    special-needs: bool,
    status: (string-ascii 20)
  }
)

(define-map trip-logs
  { route-id: uint, trip-id: uint }
  {
    driver-id: uint,
    vehicle-id: uint,
    start-time: uint,
    end-time: uint,
    students-picked: uint,
    incidents-reported: uint,
    fuel-consumed: uint,
    trip-status: (string-ascii 20)
  }
)

(define-map emergency-alerts
  { alert-id: uint }
  {
    route-id: uint,
    alert-type: (string-ascii 50),
    severity: uint,
    description: (string-ascii 500),
    reported-by: principal,
    timestamp: uint,
    resolved: bool
  }
)

;; Data Variables
(define-data-var next-driver-id uint u1)
(define-data-var next-vehicle-id uint u1)
(define-data-var next-route-id uint u1)
(define-data-var next-student-id uint u1)
(define-data-var next-trip-id uint u1)
(define-data-var next-alert-id uint u1)
(define-data-var total-active-routes uint u0)
(define-data-var total-students uint u0)

;; Driver Management Functions
(define-public (register-driver (driver-principal principal) (name (string-ascii 100)) 
                               (license-number (string-ascii 50)) (certification-type (string-ascii 50)) 
                               (certification-expiry uint))
  (let ((driver-id (var-get next-driver-id)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> certification-expiry stacks-block-height) err-invalid-data)
    
    (map-set drivers
      { driver-id: driver-id }
      {
        driver-principal: driver-principal,
        name: name,
        license-number: license-number,
        certification-type: certification-type,
        certification-expiry: certification-expiry,
        background-check-date: stacks-block-height,
        status: "active",
        total-violations: u0
      }
    )
    
    (var-set next-driver-id (+ driver-id u1))
    (ok driver-id)
  )
)

(define-public (update-driver-certification (driver-id uint) (new-expiry uint))
  (let ((driver-data (unwrap! (map-get? drivers { driver-id: driver-id }) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> new-expiry stacks-block-height) err-invalid-data)
    
    (map-set drivers
      { driver-id: driver-id }
      (merge driver-data {
        certification-expiry: new-expiry,
        background-check-date: stacks-block-height
      })
    )
    (ok true)
  )
)

;; Vehicle Management Functions
(define-public (register-vehicle (bus-number (string-ascii 20)) (capacity uint) 
                                (fuel-efficiency uint))
  (let ((vehicle-id (var-get next-vehicle-id)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (> capacity u0) err-invalid-data)
    (asserts! (> fuel-efficiency u0) err-invalid-data)
    
    (map-set vehicles
      { vehicle-id: vehicle-id }
      {
        bus-number: bus-number,
        capacity: capacity,
        last-inspection: stacks-block-height,
        next-maintenance: (+ stacks-block-height u10000),
        mileage: u0,
        fuel-efficiency: fuel-efficiency,
        status: "active",
        assigned-routes: u0
      }
    )
    
    (var-set next-vehicle-id (+ vehicle-id u1))
    (ok vehicle-id)
  )
)

(define-public (update-vehicle-maintenance (vehicle-id uint) (new-mileage uint))
  (let ((vehicle-data (unwrap! (map-get? vehicles { vehicle-id: vehicle-id }) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (>= new-mileage (get mileage vehicle-data)) err-invalid-data)
    
    (map-set vehicles
      { vehicle-id: vehicle-id }
      (merge vehicle-data {
        mileage: new-mileage,
        last-inspection: stacks-block-height,
        next-maintenance: (+ stacks-block-height u10000)
      })
    )
    (ok true)
  )
)

;; Route Management Functions
(define-public (create-route (route-name (string-ascii 100)) (driver-id uint) (vehicle-id uint)
                            (start-location (string-ascii 200)) (end-location (string-ascii 200))
                            (estimated-duration uint))
  (let ((route-id (var-get next-route-id))
        (driver-data (unwrap! (map-get? drivers { driver-id: driver-id }) err-not-found))
        (vehicle-data (unwrap! (map-get? vehicles { vehicle-id: vehicle-id }) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status driver-data) "active") err-unauthorized)
    (asserts! (is-eq (get status vehicle-data) "active") err-vehicle-not-active)
    (asserts! (> (get certification-expiry driver-data) stacks-block-height) err-expired-certification)
    (asserts! (> estimated-duration u0) err-invalid-data)
    
    (map-set routes
      { route-id: route-id }
      {
        route-name: route-name,
        driver-id: driver-id,
        vehicle-id: vehicle-id,
        start-location: start-location,
        end-location: end-location,
        estimated-duration: estimated-duration,
        student-count: u0,
        safety-rating: u100,
        active-status: true
      }
    )
    
    (var-set next-route-id (+ route-id u1))
    (var-set total-active-routes (+ (var-get total-active-routes) u1))
    (ok route-id)
  )
)

;; Student Management Functions
(define-public (register-student (name (string-ascii 100)) (parent-contact (string-ascii 100))
                                (emergency-contact (string-ascii 100)) (route-id uint)
                                (pickup-location (string-ascii 200)) (grade-level uint)
                                (special-needs bool))
  (let ((student-id (var-get next-student-id)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-some (map-get? routes { route-id: route-id })) err-not-found)
    (asserts! (> grade-level u0) err-invalid-data)
    
    (map-set students
      { student-id: student-id }
      {
        name: name,
        parent-contact: parent-contact,
        emergency-contact: emergency-contact,
        route-id: route-id,
        pickup-location: pickup-location,
        grade-level: grade-level,
        special-needs: special-needs,
        status: "active"
      }
    )
    
    ;; Update route student count
    (let ((route-data (unwrap! (map-get? routes { route-id: route-id }) err-not-found)))
      (map-set routes
        { route-id: route-id }
        (merge route-data {
          student-count: (+ (get student-count route-data) u1)
        })
      )
    )
    
    (var-set next-student-id (+ student-id u1))
    (var-set total-students (+ (var-get total-students) u1))
    (ok student-id)
  )
)

;; Trip Logging Functions
(define-public (log-trip (route-id uint) (students-picked uint) (fuel-consumed uint))
  (let ((route-data (unwrap! (map-get? routes { route-id: route-id }) err-not-found))
        (trip-id (var-get next-trip-id)))
    (asserts! (or (is-eq tx-sender contract-owner)
                  (is-eq tx-sender (get driver-principal (unwrap! (map-get? drivers { driver-id: (get driver-id route-data) }) err-not-found)))) err-unauthorized)
    (asserts! (get active-status route-data) err-not-found)
    
    (map-set trip-logs
      { route-id: route-id, trip-id: trip-id }
      {
        driver-id: (get driver-id route-data),
        vehicle-id: (get vehicle-id route-data),
        start-time: stacks-block-height,
        end-time: (+ stacks-block-height (get estimated-duration route-data)),
        students-picked: students-picked,
        incidents-reported: u0,
        fuel-consumed: fuel-consumed,
        trip-status: "completed"
      }
    )
    
    (var-set next-trip-id (+ trip-id u1))
    (ok trip-id)
  )
)

;; Emergency Communication Functions
(define-public (report-emergency (route-id uint) (alert-type (string-ascii 50)) 
                                (severity uint) (description (string-ascii 500)))
  (let ((alert-id (var-get next-alert-id))
        (route-data (unwrap! (map-get? routes { route-id: route-id }) err-not-found)))
    (asserts! (or (is-eq tx-sender contract-owner)
                  (is-eq tx-sender (get driver-principal (unwrap! (map-get? drivers { driver-id: (get driver-id route-data) }) err-not-found)))) err-unauthorized)
    (asserts! (<= severity u5) err-invalid-data)
    
    (map-set emergency-alerts
      { alert-id: alert-id }
      {
        route-id: route-id,
        alert-type: alert-type,
        severity: severity,
        description: description,
        reported-by: tx-sender,
        timestamp: stacks-block-height,
        resolved: false
      }
    )
    
    (var-set next-alert-id (+ alert-id u1))
    (ok alert-id)
  )
)

(define-public (resolve-emergency (alert-id uint))
  (let ((alert-data (unwrap! (map-get? emergency-alerts { alert-id: alert-id }) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set emergency-alerts
      { alert-id: alert-id }
      (merge alert-data { resolved: true })
    )
    (ok true)
  )
)

;; Read-only Functions
(define-read-only (get-driver (driver-id uint))
  (map-get? drivers { driver-id: driver-id })
)

(define-read-only (get-vehicle (vehicle-id uint))
  (map-get? vehicles { vehicle-id: vehicle-id })
)

(define-read-only (get-route (route-id uint))
  (map-get? routes { route-id: route-id })
)

(define-read-only (get-student (student-id uint))
  (map-get? students { student-id: student-id })
)

(define-read-only (get-trip-log (route-id uint) (trip-id uint))
  (map-get? trip-logs { route-id: route-id, trip-id: trip-id })
)

(define-read-only (get-emergency-alert (alert-id uint))
  (map-get? emergency-alerts { alert-id: alert-id })
)

(define-read-only (get-system-stats)
  {
    total-routes: (var-get total-active-routes),
    total-students: (var-get total-students),
    next-driver-id: (var-get next-driver-id),
    next-vehicle-id: (var-get next-vehicle-id)
  }
)

(define-read-only (check-driver-certification (driver-id uint))
  (let ((driver-data (unwrap! (map-get? drivers { driver-id: driver-id }) err-not-found)))
    (ok (> (get certification-expiry driver-data) stacks-block-height))
  )
)


;; title: transport-monitor
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

