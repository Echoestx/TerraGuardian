;; Basic Decentralized Conservation Funding

;; Constants
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INITIATIVE_NOT_FOUND (err u101))
(define-constant ERR_DUPLICATE_INITIATIVE (err u102))
(define-constant ERR_FUNDING_TOO_LOW (err u103))
(define-constant ERR_TIME_EXPIRED (err u104))
(define-constant ERR_TARGET_NOT_ACHIEVED (err u105))
(define-constant ERR_INVALID_PARAMETERS (err u106))

;; Data Maps
(define-map conservation-initiatives 
  { initiative-id: uint } 
  { 
    title: (string-ascii 50), 
    steward: principal, 
    target-funding: uint, 
    end-date: uint, 
    funds-collected: uint, 
    status-active: bool
  }
)

(define-map backed-initiatives 
  { initiative-id: uint, backer: principal } 
  { funding: uint }
)

;; Variables
(define-data-var initiative-counter uint u0)

;; Helper function to check if initiative exists
(define-private (initiative-exists (initiative-id uint))
  (is-some (map-get? conservation-initiatives { initiative-id: initiative-id }))
)

;; Functions

;; Register a new conservation initiative
(define-public (register-initiative (title (string-ascii 50)) (target-funding uint) (end-date uint))
  (let (
    (initiative-id (+ (var-get initiative-counter) u1))
    (title-length (len title))
  )
    (asserts! (> end-date block-height) ERR_TIME_EXPIRED)
    (asserts! (> target-funding u0) ERR_FUNDING_TOO_LOW)
    (asserts! (and (> title-length u0) (<= title-length u50)) ERR_INVALID_PARAMETERS)
    (asserts! (is-none (map-get? conservation-initiatives { initiative-id: initiative-id })) ERR_DUPLICATE_INITIATIVE)
    (map-set conservation-initiatives
      { initiative-id: initiative-id }
      { 
        title: title, 
        steward: tx-sender, 
        target-funding: target-funding, 
        end-date: end-date, 
        funds-collected: u0, 
        status-active: true
      }
    )
    (var-set initiative-counter initiative-id)
    (ok initiative-id)
  )
)

;; Back a conservation initiative
(define-public (back-initiative (initiative-id uint) (funding uint))
  (let (
    (initiative (unwrap! (map-get? conservation-initiatives { initiative-id: initiative-id }) ERR_INITIATIVE_NOT_FOUND))
    (existing-backing (default-to { funding: u0 } (map-get? backed-initiatives { initiative-id: initiative-id, backer: tx-sender })))
  )
    (asserts! (initiative-exists initiative-id) ERR_INITIATIVE_NOT_FOUND)
    (asserts! (> funding u0) ERR_FUNDING_TOO_LOW)
    (asserts! (get status-active initiative) ERR_NOT_AUTHORIZED)
    (asserts! (<= block-height (get end-date initiative)) ERR_TIME_EXPIRED)
    (try! (stx-transfer? funding tx-sender (as-contract tx-sender)))
    (map-set backed-initiatives
      { initiative-id: initiative-id, backer: tx-sender }
      { funding: (+ (get funding existing-backing) funding) }
    )
    (map-set conservation-initiatives
      { initiative-id: initiative-id }
      (merge initiative { funds-collected: (+ (get funds-collected initiative) funding) })
    )
    (ok true)
  )
)

;; Release funds (for initiative stewards)
(define-public (release-funds (initiative-id uint))
  (let (
    (initiative (unwrap! (map-get? conservation-initiatives { initiative-id: initiative-id }) ERR_INITIATIVE_NOT_FOUND))
  )
    (asserts! (initiative-exists initiative-id) ERR_INITIATIVE_NOT_FOUND)
    (asserts! (is-eq tx-sender (get steward initiative)) ERR_NOT_AUTHORIZED)
    (asserts! (>= (get funds-collected initiative) (get target-funding initiative)) ERR_TARGET_NOT_ACHIEVED)
    (asserts! (> block-height (get end-date initiative)) ERR_TIME_EXPIRED)
    (try! (as-contract (stx-transfer? (get funds-collected initiative) tx-sender (get steward initiative))))
    (map-set conservation-initiatives
      { initiative-id: initiative-id }
      (merge initiative { status-active: false })
    )
    (ok true)
  )
)

;; Retrieve contribution (for backers if initiative fails)
(define-public (retrieve-funding (initiative-id uint))
  (let (
    (initiative (unwrap! (map-get? conservation-initiatives { initiative-id: initiative-id }) ERR_INITIATIVE_NOT_FOUND))
    (backer-data (unwrap! (map-get? backed-initiatives { initiative-id: initiative-id, backer: tx-sender }) ERR_INITIATIVE_NOT_FOUND))
  )
    (asserts! (initiative-exists initiative-id) ERR_INITIATIVE_NOT_FOUND)
    (asserts! (> block-height (get end-date initiative)) ERR_TIME_EXPIRED)
    (asserts! (< (get funds-collected initiative) (get target-funding initiative)) ERR_NOT_AUTHORIZED)
    (try! (as-contract (stx-transfer? (get funding backer-data) tx-sender tx-sender)))
    (map-delete backed-initiatives { initiative-id: initiative-id, backer: tx-sender })
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-initiative-details (initiative-id uint))
  (map-get? conservation-initiatives { initiative-id: initiative-id })
)

(define-read-only (get-backer-contribution (initiative-id uint) (backer principal))
  (map-get? backed-initiatives { initiative-id: initiative-id, backer: backer })
)
