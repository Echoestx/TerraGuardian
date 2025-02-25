;; TerraGuardian: Decentralized Funding for Conservation Projects

;; Constants
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INITIATIVE_NOT_FOUND (err u101))
(define-constant ERR_DUPLICATE_INITIATIVE (err u102))
(define-constant ERR_FUNDING_TOO_LOW (err u103))
(define-constant ERR_TIME_EXPIRED (err u104))
(define-constant ERR_TARGET_NOT_ACHIEVED (err u105))
(define-constant ERR_INVALID_PARAMETERS (err u106))
(define-constant ERR_EXISTING_BACKERS (err u107))
(define-constant ERR_TIMEFRAME_EXTENSION_BLOCKED (err u108))
(define-constant ERR_REVIEW_ONGOING (err u109))
(define-constant ERR_APPROVAL_CRITERIA_UNMET (err u110))

;; Configuration
(define-constant EXTENSION_FUNDING_REQUIREMENT u75) ;; 75% of the target
(define-constant MAX_EXTENSION_LENGTH u30)
(define-constant COMMUNITY_REVIEW_DAYS u7)
(define-constant MIN_APPROVAL_PERCENTAGE u60) ;; 60% approvals required
(define-constant MIN_REVIEWER_COUNT u10) ;; At least 10 reviews needed

;; Data Maps
(define-map conservation-initiatives 
  { initiative-id: uint } 
  { 
    title: (string-ascii 50), 
    steward: principal, 
    target-funding: uint, 
    end-date: uint, 
    funds-collected: uint, 
    status-active: bool,
    extension-count: uint,
    review-end-time: uint,
    total-reviews: uint,
    positive-reviews: uint
  }
)

(define-map backed-initiatives 
  { initiative-id: uint, backer: principal } 
  { funding: uint }
)

(define-map initiative-reviews
  { initiative-id: uint, reviewer: principal }
  { approved: bool }
)

;; Variables
(define-data-var initiative-counter uint u0)

;; Helper function to check if initiative exists
(define-private (initiative-exists (initiative-id uint))
  (is-some (map-get? conservation-initiatives { initiative-id: initiative-id }))
)

;; Helper function to calculate approval rate
(define-private (calculate-approval-rate (positive-count uint) (total-count uint))
  (if (is-eq total-count u0)
    u0
    (/ (* positive-count u100) total-count)
  )
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
        status-active: true,
        extension-count: u0,
        review-end-time: (+ end-date (* COMMUNITY_REVIEW_DAYS u144)), ;; Assuming 144 blocks per day
        total-reviews: u0,
        positive-reviews: u0
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

;; Submit a review for a conservation initiative
(define-public (submit-review (initiative-id uint) (approval bool))
  (let (
    (initiative (unwrap! (map-get? conservation-initiatives { initiative-id: initiative-id }) ERR_INITIATIVE_NOT_FOUND))
    (backer-data (unwrap! (map-get? backed-initiatives { initiative-id: initiative-id, backer: tx-sender }) ERR_NOT_AUTHORIZED))
  )
    (asserts! (initiative-exists initiative-id) ERR_INITIATIVE_NOT_FOUND)
    (asserts! (get status-active initiative) ERR_NOT_AUTHORIZED)
    (asserts! (<= block-height (get review-end-time initiative)) ERR_TIME_EXPIRED)
    (asserts! (is-none (map-get? initiative-reviews { initiative-id: initiative-id, reviewer: tx-sender })) ERR_DUPLICATE_INITIATIVE)
    (map-set initiative-reviews
      { initiative-id: initiative-id, reviewer: tx-sender }
      { approved: approval }
    )
    (map-set conservation-initiatives
      { initiative-id: initiative-id }
      (merge initiative {
        total-reviews: (+ (get total-reviews initiative) u1),
        positive-reviews: (if approval (+ (get positive-reviews initiative) u1) (get positive-reviews initiative))
      })
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
    (asserts! (> block-height (get review-end-time initiative)) ERR_REVIEW_ONGOING)
    (asserts! (>= (get total-reviews initiative) MIN_REVIEWER_COUNT) ERR_APPROVAL_CRITERIA_UNMET)
    (asserts! (>= (calculate-approval-rate (get positive-reviews initiative) (get total-reviews initiative)) MIN_APPROVAL_PERCENTAGE) ERR_APPROVAL_CRITERIA_UNMET)
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
    (asserts! (> block-height (get review-end-time initiative)) ERR_REVIEW_ONGOING)
    (asserts! (or
      (< (get funds-collected initiative) (get target-funding initiative))
      (< (get total-reviews initiative) MIN_REVIEWER_COUNT)
      (< (calculate-approval-rate (get positive-reviews initiative) (get total-reviews initiative)) MIN_APPROVAL_PERCENTAGE)
    ) ERR_NOT_AUTHORIZED)
    (try! (as-contract (stx-transfer? (get funding backer-data) tx-sender tx-sender)))
    (map-delete backed-initiatives { initiative-id: initiative-id, backer: tx-sender })
    (ok true)
  )
)

;; Terminate initiative (for stewards)
(define-public (terminate-initiative (initiative-id uint))
  (let (
    (initiative (unwrap! (map-get? conservation-initiatives { initiative-id: initiative-id }) ERR_INITIATIVE_NOT_FOUND))
  )
    (asserts! (initiative-exists initiative-id) ERR_INITIATIVE_NOT_FOUND)
    (asserts! (is-eq tx-sender (get steward initiative)) ERR_NOT_AUTHORIZED)
    (asserts! (get status-active initiative) ERR_NOT_AUTHORIZED)
    (asserts! (<= block-height (get end-date initiative)) ERR_TIME_EXPIRED)
    (asserts! (is-eq (get funds-collected initiative) u0) ERR_EXISTING_BACKERS)
    (map-set conservation-initiatives
      { initiative-id: initiative-id }
      (merge initiative { status-active: false })
    )
    (ok true)
  )
)

;; Extend initiative timeframe
(define-public (extend-timeframe (initiative-id uint) (new-end-date uint))
  (let (
    (initiative (unwrap! (map-get? conservation-initiatives { initiative-id: initiative-id }) ERR_INITIATIVE_NOT_FOUND))
    (current-end-date (get end-date initiative))
    (extension-days (/ (- new-end-date current-end-date) u144)) ;; Assuming 144 blocks per day
  )
    (asserts! (initiative-exists initiative-id) ERR_INITIATIVE_NOT_FOUND)
    (asserts! (is-eq tx-sender (get steward initiative)) ERR_NOT_AUTHORIZED)
    (asserts! (get status-active initiative) ERR_NOT_AUTHORIZED)
    (asserts! (<= block-height current-end-date) ERR_TIME_EXPIRED)
    (asserts! (<= extension-days MAX_EXTENSION_LENGTH) ERR_INVALID_PARAMETERS)
    (asserts! (>= (* (get funds-collected initiative) u100) (* (get target-funding initiative) EXTENSION_FUNDING_REQUIREMENT)) ERR_TIMEFRAME_EXTENSION_BLOCKED)
    (asserts! (< (get extension-count initiative) u3) ERR_TIMEFRAME_EXTENSION_BLOCKED)
    (map-set conservation-initiatives
      { initiative-id: initiative-id }
      (merge initiative { 
        end-date: new-end-date,
        review-end-time: (+ new-end-date (* COMMUNITY_REVIEW_DAYS u144)),
        extension-count: (+ (get extension-count initiative) u1)
      })
    )
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

(define-read-only (get-reviewer-feedback (initiative-id uint) (reviewer principal))
  (map-get? initiative-reviews { initiative-id: initiative-id, reviewer: reviewer })
)

(define-read-only (get-community-feedback (initiative-id uint))
  (match (map-get? conservation-initiatives { initiative-id: initiative-id })
    initiative (ok {
      total-reviews: (get total-reviews initiative),
      positive-reviews: (get positive-reviews initiative),
      approval-percentage: (calculate-approval-rate (get positive-reviews initiative) (get total-reviews initiative)),
      criteria-met: (and
        (>= (get total-reviews initiative) MIN_REVIEWER_COUNT)
        (>= (calculate-approval-rate (get positive-reviews initiative) (get total-reviews initiative)) MIN_APPROVAL_PERCENTAGE)
      )
    })
    ERR_INITIATIVE_NOT_FOUND
  )
)