;; Ridge Decentralized Notification Infrastructure Protocol
;; Comprehensive smart contract for cross-chain notification routing with privacy preservation

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-INPUT (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-NODE-NOT-FOUND (err u103))
(define-constant ERR-SUBSCRIPTION-NOT-FOUND (err u104))
(define-constant ERR-NOTIFICATION-EXPIRED (err u105))
(define-constant ERR-INVALID-REPUTATION (err u106))
(define-constant ERR-ESCROW-NOT-FOUND (err u107))
(define-constant ERR-DELIVERY-FAILED (err u108))
(define-constant ERR-SPAM-DETECTED (err u109))
(define-constant ERR-INSUFFICIENT-STAKE (err u110))
(define-constant ERR-TEMPLATE-NOT-FOUND (err u111))
(define-constant ERR-BRIDGE-DISABLED (err u112))
(define-constant ERR-QUEUE-FULL (err u113))

;; Contract constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MINIMUM-NODE-STAKE u1000000) ;; 1 RIDGE token
(define-constant BASE-NOTIFICATION-FEE u100)
(define-constant REPUTATION-THRESHOLD u80)
(define-constant MAX-QUEUE-SIZE u1000)
(define-constant DELIVERY-TIMEOUT u144) ;; blocks

;; Data variables
(define-data-var contract-owner principal CONTRACT-OWNER)
(define-data-var protocol-fee-rate uint u250) ;; 2.5%
(define-data-var total-nodes uint u0)
(define-data-var total-notifications uint u0)
(define-data-var notification-counter uint u0)
(define-data-var escrow-counter uint u0)
(define-data-var bridge-enabled bool true)
(define-data-var emergency-pause bool false)

;; Data maps
(define-map notification-nodes
    principal
    {
        stake: uint,
        reputation-score: uint,
        total-delivered: uint,
        last-active: uint,
        earnings: uint,
        is-active: bool
    }
)

(define-map user-subscriptions
    principal
    {
        subscription-price: uint,
        allowed-senders: (list 50 principal),
        blocked-categories: (list 20 (string-ascii 32)),
        auto-renewal: bool,
        spending-limit: uint,
        total-earned: uint
    }
)

(define-map notification-queue
    uint
    {
        sender: principal,
        recipient: principal,
        message-hash: (buff 32),
        priority: uint,
        gas-price: uint,
        expiry-block: uint,
        template-id: uint,
        cross-chain-target: (optional (string-ascii 20)),
        delivery-nodes: (list 3 principal),
        is-delivered: bool,
        escrow-amount: uint
    }
)

(define-map notification-templates
    uint
    {
        creator: principal,
        template-hash: (buff 32),
        conditions: (list 10 (string-ascii 100)),
        is-active: bool,
        usage-count: uint,
        fee-per-use: uint
    }
)

(define-map delivery-proofs
    {notification-id: uint, node: principal}
    {
        proof-hash: (buff 32),
        timestamp: uint,
        gas-used: uint,
        attestation-signature: (buff 65)
    }
)

(define-map escrow-contracts
    uint
    {
        depositor: principal,
        amount: uint,
        notification-id: uint,
        release-condition: (string-ascii 50),
        expiry-block: uint,
        is-released: bool
    }
)

(define-map node-performance
    principal
    {
        success-rate: uint,
        average-delivery-time: uint,
        spam-reports: uint,
        last-penalty: uint,
        consecutive-successes: uint
    }
)

(define-map cross-chain-bridges
    (string-ascii 20)
    {
        is-enabled: bool,
        bridge-contract: principal,
        minimum-fee: uint,
        success-rate: uint
    }
)

;; Authorization check
(define-private (is-contract-owner)
    (is-eq tx-sender (var-get contract-owner))
)

;; Input validation functions
(define-private (validate-stake-amount (amount uint))
    (>= amount MINIMUM-NODE-STAKE)
)

(define-private (validate-reputation (score uint))
    (<= score u100)
)

(define-private (validate-priority (priority uint))
    (and (>= priority u1) (<= priority u10))
)

;; Admin functions
(define-public (set-protocol-fee-rate (new-rate uint))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (asserts! (<= new-rate u1000) ERR-INVALID-INPUT)
        (ok (var-set protocol-fee-rate new-rate))
    )
)

(define-public (toggle-emergency-pause)
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (ok (var-set emergency-pause (not (var-get emergency-pause))))
    )
)

(define-public (update-bridge-status (chain-name (string-ascii 20)) (enabled bool))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (match (map-get? cross-chain-bridges chain-name)
            bridge-info (ok (map-set cross-chain-bridges chain-name
                (merge bridge-info {is-enabled: enabled})))
            ERR-INVALID-INPUT
        )
    )
)

;; Node management functions
(define-public (register-notification-node (stake-amount uint))
    (let ((current-block block-height))
        (begin
            (asserts! (not (var-get emergency-pause)) ERR-NOT-AUTHORIZED)
            (asserts! (validate-stake-amount stake-amount) ERR-INSUFFICIENT-STAKE)
            (asserts! (is-none (map-get? notification-nodes tx-sender)) ERR-INVALID-INPUT)
            
            (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
            
            (map-set notification-nodes tx-sender {
                stake: stake-amount,
                reputation-score: u100,
                total-delivered: u0,
                last-active: current-block,
                earnings: u0,
                is-active: true
            })
            
            (map-set node-performance tx-sender {
                success-rate: u100,
                average-delivery-time: u0,
                spam-reports: u0,
                last-penalty: u0,
                consecutive-successes: u0
            })
            
            (var-set total-nodes (+ (var-get total-nodes) u1))
            (ok true)
        )
    )
)

(define-public (stake-additional-tokens (amount uint))
    (match (map-get? notification-nodes tx-sender)
        node-info
        (begin
            (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
            (ok (map-set notification-nodes tx-sender
                (merge node-info {stake: (+ (get stake node-info) amount)})))
        )
        ERR-NODE-NOT-FOUND
    )
)

(define-public (update-user-subscription 
    (price uint) 
    (senders (list 50 principal))
    (blocked-cats (list 20 (string-ascii 32)))
    (auto-renew bool)
    (limit uint))
    (begin
        (asserts! (not (var-get emergency-pause)) ERR-NOT-AUTHORIZED)
        (asserts! (<= price u100000) ERR-INVALID-INPUT)
        
        (ok (map-set user-subscriptions tx-sender {
            subscription-price: price,
            allowed-senders: senders,
            blocked-categories: blocked-cats,
            auto-renewal: auto-renew,
            spending-limit: limit,
            total-earned: (default-to u0 (get total-earned 
                (map-get? user-subscriptions tx-sender)))
        }))
    )
)

(define-public (queue-notification
    (recipient principal)
    (message-hash (buff 32))
    (priority uint)
    (template-id uint)
    (cross-chain (optional (string-ascii 20)))
    (escrow-amount uint))
    (let ((notification-id (+ (var-get notification-counter) u1))
          (expiry-block (+ block-height DELIVERY-TIMEOUT))
          (selected-nodes (select-delivery-nodes priority)))
        (begin
            (asserts! (not (var-get emergency-pause)) ERR-NOT-AUTHORIZED)
            (asserts! (validate-priority priority) ERR-INVALID-INPUT)
            (asserts! (< (var-get notification-counter) MAX-QUEUE-SIZE) ERR-QUEUE-FULL)
            
            ;; Handle escrow if amount > 0
            (if (> escrow-amount u0)
                (try! (stx-transfer? escrow-amount tx-sender (as-contract tx-sender)))
                true
            )
            
            (map-set notification-queue notification-id {
                sender: tx-sender,
                recipient: recipient,
                message-hash: message-hash,
                priority: priority,
                gas-price: (get-current-gas-price),
                expiry-block: expiry-block,
                template-id: template-id,
                cross-chain-target: cross-chain,
                delivery-nodes: selected-nodes,
                is-delivered: false,
                escrow-amount: escrow-amount
            })
            
            (var-set notification-counter notification-id)
            (var-set total-notifications (+ (var-get total-notifications) u1))
            (ok notification-id)
        )
    )
)

(define-public (submit-delivery-proof
    (notification-id uint)
    (proof-hash (buff 32))
    (gas-used uint)
    (signature (buff 65)))
    (match (map-get? notification-queue notification-id)
        notification
        (begin
            (asserts! (is-delivery-node tx-sender (get delivery-nodes notification)) ERR-NOT-AUTHORIZED)
            (asserts! (not (get is-delivered notification)) ERR-DELIVERY-FAILED)
            (asserts! (<= block-height (get expiry-block notification)) ERR-NOTIFICATION-EXPIRED)
            
            (map-set delivery-proofs {notification-id: notification-id, node: tx-sender} {
                proof-hash: proof-hash,
                timestamp: block-height,
                gas-used: gas-used,
                attestation-signature: signature
            })
            
            ;; Update node statistics
            (update-node-performance tx-sender true)
            
            ;; Mark notification as delivered
            (map-set notification-queue notification-id
                (merge notification {is-delivered: true}))
            
            ;; Release escrow if applicable
            (if (> (get escrow-amount notification) u0)
                (try! (as-contract (stx-transfer? 
                    (get escrow-amount notification)
                    tx-sender
                    (get recipient notification))))
                true
            )
            
            (ok true)
        )
        ERR-NOTIFICATION-NOT-FOUND
    )
)