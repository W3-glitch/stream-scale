;; StreamScale - Privacy-Preserving Reputation System

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-score (err u103))
(define-constant err-unauthorized (err u104))

;; Data Variables
(define-data-var min-reputation-score uint u0)
(define-data-var max-reputation-score uint u1000)

;; Data Maps
;; Store reputation scores (commitment hash -> score)
(define-map reputation-scores
    principal
    {
        score: uint,
        total-interactions: uint,
        last-update: uint,
        active: bool
    }
)

;; Store proof commitments (for privacy preservation)
(define-map proof-commitments
    { user: principal, commitment-id: uint }
    {
        commitment-hash: (buff 32),
        timestamp: uint,
        verified: bool
    }
)

;; Oracle validators (simplified decentralized oracle)
(define-map authorized-oracles
    principal
    bool
)

;; Reputation milestones for rewards
(define-map milestone-rewards
    uint
    uint
)

;; User interaction history count
(define-map interaction-count
    principal
    uint
)

;; Read-only functions

(define-read-only (get-reputation (user principal))
    (ok (default-to 
        { score: u0, total-interactions: u0, last-update: u0, active: false }
        (map-get? reputation-scores user)
    ))
)

(define-read-only (get-reputation-score (user principal))
    (ok (get score (default-to 
        { score: u0, total-interactions: u0, last-update: u0, active: false }
        (map-get? reputation-scores user)
    )))
)

(define-read-only (is-oracle-authorized (oracle principal))
    (default-to false (map-get? authorized-oracles oracle))
)

(define-read-only (get-proof-commitment (user principal) (commitment-id uint))
    (ok (map-get? proof-commitments { user: user, commitment-id: commitment-id }))
)

(define-read-only (get-interaction-count (user principal))
    (ok (default-to u0 (map-get? interaction-count user)))
)

(define-read-only (meets-reputation-threshold (user principal) (threshold uint))
    (let
        (
            (user-score (get score (default-to 
                { score: u0, total-interactions: u0, last-update: u0, active: false }
                (map-get? reputation-scores user)
            )))
        )
        (ok (>= user-score threshold))
    )
)

;; Public functions

;; Initialize user reputation
(define-public (initialize-reputation)
    (let
        (
            (existing (map-get? reputation-scores tx-sender))
        )
        (if (is-some existing)
            err-already-exists
            (ok (map-set reputation-scores tx-sender {
                score: u100,
                total-interactions: u0,
                last-update: block-height,
                active: true
            }))
        )
    )
)

;; Submit a proof commitment (zero-knowledge proof placeholder)
(define-public (submit-proof-commitment (commitment-id uint) (commitment-hash (buff 32)))
    (let
        (
            (current-count (default-to u0 (map-get? interaction-count tx-sender)))
        )
        (begin
            (map-set proof-commitments 
                { user: tx-sender, commitment-id: commitment-id }
                {
                    commitment-hash: commitment-hash,
                    timestamp: block-height,
                    verified: false
                }
            )
            (map-set interaction-count tx-sender (+ current-count u1))
            (ok true)
        )
    )
)

;; Verify proof and update reputation (oracle function)
(define-public (verify-and-update-reputation (user principal) (commitment-id uint) (score-delta int))
    (let
        (
            (caller-authorized (is-oracle-authorized tx-sender))
            (current-rep (default-to 
                { score: u100, total-interactions: u0, last-update: u0, active: true }
                (map-get? reputation-scores user)
            ))
            (current-score (get score current-rep))
            (new-score (if (< score-delta 0)
                (if (> (to-uint (- 0 score-delta)) current-score)
                    u0
                    (- current-score (to-uint (- 0 score-delta)))
                )
                (+ current-score (to-uint score-delta))
            ))
        )
        (if (not caller-authorized)
            err-unauthorized
            (begin
                ;; Mark proof as verified
                (map-set proof-commitments 
                    { user: user, commitment-id: commitment-id }
                    (merge 
                        (default-to 
                            { commitment-hash: 0x00, timestamp: u0, verified: false }
                            (map-get? proof-commitments { user: user, commitment-id: commitment-id })
                        )
                        { verified: true }
                    )
                )
                ;; Update reputation score
                (map-set reputation-scores user {
                    score: new-score,
                    total-interactions: (+ (get total-interactions current-rep) u1),
                    last-update: block-height,
                    active: true
                })
                (ok new-score)
            )
        )
    )
)

;; Update reputation based on interaction (simplified)
(define-public (record-interaction (positive bool))
    (let
        (
            (current-rep (default-to 
                { score: u100, total-interactions: u0, last-update: u0, active: true }
                (map-get? reputation-scores tx-sender)
            ))
            (score-change (if positive 10 -5))
            (current-score (get score current-rep))
            (new-score (if positive
                (+ current-score (to-uint score-change))
                (if (> (to-uint (- 0 score-change)) current-score)
                    u0
                    (- current-score (to-uint (- 0 score-change)))
                )
            ))
        )
        (begin
            (map-set reputation-scores tx-sender {
                score: new-score,
                total-interactions: (+ (get total-interactions current-rep) u1),
                last-update: block-height,
                active: true
            })
            (ok new-score)
        )
    )
)

;; Admin functions

;; Authorize oracle
(define-public (authorize-oracle (oracle principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set authorized-oracles oracle true))
    )
)

;; Revoke oracle
(define-public (revoke-oracle (oracle principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set authorized-oracles oracle false))
    )
)

;; Set milestone reward
(define-public (set-milestone-reward (score-threshold uint) (reward-amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (map-set milestone-rewards score-threshold reward-amount))
    )
)

;; Deactivate user reputation (emergency function)
(define-public (deactivate-reputation (user principal))
    (let
        (
            (current-rep (default-to 
                { score: u0, total-interactions: u0, last-update: u0, active: false }
                (map-get? reputation-scores user)
            ))
        )
        (begin
            (asserts! (is-eq tx-sender contract-owner) err-owner-only)
            (ok (map-set reputation-scores user 
                (merge current-rep { active: false })
            ))
        )
    )
)

;; Initialize contract
(begin
    (map-set authorized-oracles contract-owner true)
)
