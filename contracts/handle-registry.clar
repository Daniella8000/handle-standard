;; handle-registry contract
;; This contract creates a robust system for registering, managing, and verifying unique blockchain handles.
;; It allows users to claim and verify handles through a decentralized, community-driven process.
;; The contract maintains an immutable record of verified handles, supports handle ownership transfers,
;; and generates identity credentials for blockchain participants.

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-HANDLE-NOT-FOUND (err u101))
(define-constant ERR-HANDLE-CLAIM-NOT-FOUND (err u102))
(define-constant ERR-HANDLE-ALREADY-PROCESSED (err u103))
(define-constant ERR-INVALID-HANDLE-FORMAT (err u104))
(define-constant ERR-HANDLE-ALREADY-EXISTS (err u105))
(define-constant ERR-NOT-ENOUGH-VERIFICATIONS (err u106))
(define-constant ERR-ALREADY-VERIFIED (err u107))
(define-constant ERR-NOT-VALIDATOR (err u108))
(define-constant ERR-VERIFICATION-PERIOD-ENDED (err u109))

;; Data space definitions

;; Track contract administrator
(define-data-var contract-admin principal tx-sender)

;; Handle metadata and validation requirements
(define-map handle-configs
  { config-id: uint }
  {
    max-length: uint,
    min-length: uint,
    allowed-chars: (string-ascii 50),
    verification-threshold: uint,
    registration-fee: uint
  }
)

;; Registered handles
(define-map handles
  { handle: (string-ascii 50) }
  {
    owner: principal,
    created-at: uint,
    verified: bool,
    verification-status: (string-ascii 12),
    verification-expiry: uint
  }
)

;; Handle validators - principals authorized to verify handle claims
(define-map handle-validators
  { handle: (string-ascii 50), validator: principal }
  { 
    verified-at: uint,
    verification-proof: (string-utf8 200)
  }
)

;; Handle claim process - for initial handle registrations requiring verification
(define-map handle-claims
  { claim-id: uint }
  {
    handle: (string-ascii 50),
    claimant: principal,
    submitted-at: uint,
    status: (string-ascii 12),     ;; "pending", "verified", "rejected"
    verification-expiry: uint,     ;; time after which the verification period ends
    verifications-required: uint,  ;; number of verifications needed
    verifications-received: uint   ;; number of verifications received
  }
)

;; Detailed verifications for handle claims
(define-map handle-claim-verifications
  { claim-id: uint, validator: principal }
  {
    verified-at: uint,
    verification-proof: (string-utf8 500),
    is-valid: bool
  }
)

;; Track handle credentials and proof of identity
(define-map handle-credentials
  { principal: principal, credential-id: uint }
  {
    handle: (string-ascii 50),
    verified-at: uint,
    credential-type: (string-ascii 24),
    metadata: (string-utf8 200)
  }
)

;; Counter variables
(define-data-var next-handle-claim-id uint u1)
(define-data-var next-credential-id uint u1)
(define-data-var total-verified-handles uint u0)

;; Private functions

;; Validate handle format
(define-private (is-valid-handle (handle (string-ascii 50)) (config-id uint))
  (let (
    (config (unwrap! (map-get? handle-configs { config-id: config-id }) false))
    (handle-length (len handle))
  )
    (and 
      (<= handle-length (get max-length config))
      (>= handle-length (get min-length config))
      (fold 
        (lambda (char result)
          (and result 
               (is-some 
                 (index-of (get allowed-chars config) char)
               )
          )
        )
        (list-to-iter (string-to-list handle))
        true
      )
    )
  )
)

;; Register a new handle claim
(define-public (claim-handle
    (handle (string-ascii 50))
    (config-id uint)
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (claim-id (var-get next-handle-claim-id))
    (now-block-height block-height)
  )
    ;; Validate handle
    (asserts! (is-valid-handle handle config-id) ERR-INVALID-HANDLE-FORMAT)
    (asserts! (is-none (map-get? handles { handle: handle })) ERR-HANDLE-ALREADY-EXISTS)
    (asserts! (> verification-expiry now-block-height) (err u110))
    (asserts! (> verifications-required u0) (err u111))
    
    ;; Create handle claim
    (map-set handle-claims
      { claim-id: claim-id }
      {
        handle: handle,
        claimant: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0
      }
    )
    
    (var-set next-handle-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify a handle claim
(define-public (verify-handle-claim
    (claim-id uint)
    (is-valid bool)
    (verification-proof (string-utf8 500)))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (handle (get handle claim))
    (now-block-height block-height)
  )
    ;; Validate verification
    (asserts! (is-handle-validator handle tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none 
      (map-get? handle-claim-verifications 
        { claim-id: claim-id, validator: tx-sender }
      )
    ) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set handle-claim-verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        verification-proof: verification-proof,
        is-valid: is-valid
      }
    )
    
    ;; Update claim
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
    )
      (map-set handle-claims
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-handle-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Transfer handle ownership
(define-public (transfer-handle-ownership
    (handle (string-ascii 50))
    (new-owner principal))
  (let (
    (current-handle-info (unwrap! (map-get? handles { handle: handle }) ERR-HANDLE-NOT-FOUND))
  )
    ;; Authorization check
    (asserts! (is-eq tx-sender (get owner current-handle-info)) ERR-NOT-AUTHORIZED)
    
    ;; Update handle ownership
    (map-set handles
      { handle: handle }
      (merge current-handle-info { owner: new-owner })
    )
    
    (ok true)
  )
)

;; Finalize handle claim with expired verification period
(define-public (finalize-expired-handle-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u112))
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-handle-claim-status claim-id)
          (ok true))
        (begin
          (map-set handle-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)

;; Check if a principal is a handle validator
(define-private (is-handle-validator (handle (string-ascii 50)) (validator principal))
  (is-some 
    (map-get? handle-validators 
      { handle: handle, validator: validator }
    )
  )
)

;; Register a new handle claim
(define-public (claim-handle
    (handle (string-ascii 50))
    (config-id uint)
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (claim-id (var-get next-handle-claim-id))
    (now-block-height block-height)
  )
    ;; Validate handle
    (asserts! (is-valid-handle handle config-id) ERR-INVALID-HANDLE-FORMAT)
    (asserts! (is-none (map-get? handles { handle: handle })) ERR-HANDLE-ALREADY-EXISTS)
    (asserts! (> verification-expiry now-block-height) (err u110))
    (asserts! (> verifications-required u0) (err u111))
    
    ;; Create handle claim
    (map-set handle-claims
      { claim-id: claim-id }
      {
        handle: handle,
        claimant: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0
      }
    )
    
    (var-set next-handle-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify a handle claim
(define-public (verify-handle-claim
    (claim-id uint)
    (is-valid bool)
    (verification-proof (string-utf8 500)))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (handle (get handle claim))
    (now-block-height block-height)
  )
    ;; Validate verification
    (asserts! (is-handle-validator handle tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none 
      (map-get? handle-claim-verifications 
        { claim-id: claim-id, validator: tx-sender }
      )
    ) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set handle-claim-verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        verification-proof: verification-proof,
        is-valid: is-valid
      }
    )
    
    ;; Update claim
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
    )
      (map-set handle-claims
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-handle-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Transfer handle ownership
(define-public (transfer-handle-ownership
    (handle (string-ascii 50))
    (new-owner principal))
  (let (
    (current-handle-info (unwrap! (map-get? handles { handle: handle }) ERR-HANDLE-NOT-FOUND))
  )
    ;; Authorization check
    (asserts! (is-eq tx-sender (get owner current-handle-info)) ERR-NOT-AUTHORIZED)
    
    ;; Update handle ownership
    (map-set handles
      { handle: handle }
      (merge current-handle-info { owner: new-owner })
    )
    
    (ok true)
  )
)

;; Finalize handle claim with expired verification period
(define-public (finalize-expired-handle-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u112))
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-handle-claim-status claim-id)
          (ok true))
        (begin
          (map-set handle-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)

;; Update handle claim status based on verifications
(define-private (update-handle-claim-status (claim-id uint))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) false))
    (current-verifications (get verifications-received claim))
    (required-verifications (get verifications-required claim))
    (handle (get handle claim))
    (now-block-height block-height)
  )
    (if (>= current-verifications required-verifications)
      ;; Enough verifications received, mark handle as verified
      (begin
        (map-set handles 
          { handle: handle }
          {
            owner: (get claimant claim),
            created-at: now-block-height,
            verified: true,
            verification-status: "verified",
            verification-expiry: now-block-height
          }
        )
        
        (map-set handle-claims 
          { claim-id: claim-id }
          (merge claim { status: "verified" })
        )
        
        ;; Issue handle credential
        (let (
          (credential-id (var-get next-credential-id))
        )
          (map-set handle-credentials
            { principal: (get claimant claim), credential-id: credential-id }
            {
              handle: handle,
              verified-at: now-block-height,
              credential-type: "handle-verified",
              metadata: "On-chain handle verification"
            }
          )
          
          ;; Increment counters
          (var-set next-credential-id (+ credential-id u1))
          (var-set total-verified-handles (+ (var-get total-verified-handles) u1))
          true
        )
      )
      false
    )
  )
)

;; Register a new handle claim
(define-public (claim-handle
    (handle (string-ascii 50))
    (config-id uint)
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (claim-id (var-get next-handle-claim-id))
    (now-block-height block-height)
  )
    ;; Validate handle
    (asserts! (is-valid-handle handle config-id) ERR-INVALID-HANDLE-FORMAT)
    (asserts! (is-none (map-get? handles { handle: handle })) ERR-HANDLE-ALREADY-EXISTS)
    (asserts! (> verification-expiry now-block-height) (err u110))
    (asserts! (> verifications-required u0) (err u111))
    
    ;; Create handle claim
    (map-set handle-claims
      { claim-id: claim-id }
      {
        handle: handle,
        claimant: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0
      }
    )
    
    (var-set next-handle-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify a handle claim
(define-public (verify-handle-claim
    (claim-id uint)
    (is-valid bool)
    (verification-proof (string-utf8 500)))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (handle (get handle claim))
    (now-block-height block-height)
  )
    ;; Validate verification
    (asserts! (is-handle-validator handle tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none 
      (map-get? handle-claim-verifications 
        { claim-id: claim-id, validator: tx-sender }
      )
    ) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set handle-claim-verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        verification-proof: verification-proof,
        is-valid: is-valid
      }
    )
    
    ;; Update claim
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
    )
      (map-set handle-claims
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-handle-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Transfer handle ownership
(define-public (transfer-handle-ownership
    (handle (string-ascii 50))
    (new-owner principal))
  (let (
    (current-handle-info (unwrap! (map-get? handles { handle: handle }) ERR-HANDLE-NOT-FOUND))
  )
    ;; Authorization check
    (asserts! (is-eq tx-sender (get owner current-handle-info)) ERR-NOT-AUTHORIZED)
    
    ;; Update handle ownership
    (map-set handles
      { handle: handle }
      (merge current-handle-info { owner: new-owner })
    )
    
    (ok true)
  )
)

;; Finalize handle claim with expired verification period
(define-public (finalize-expired-handle-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u112))
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-handle-claim-status claim-id)
          (ok true))
        (begin
          (map-set handle-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)

;; Read-only functions

;; Get handle configuration details
(define-read-only (get-handle-config (config-id uint))
  (map-get? handle-configs { config-id: config-id })
)

;; Get handle details
(define-read-only (get-handle (handle (string-ascii 50)))
  (map-get? handles { handle: handle })
)

;; Check if a principal is a handle validator
(define-read-only (is-validator (handle (string-ascii 50)) (validator principal))
  (is-some (map-get? handle-validators { handle: handle, validator: validator }))
)

;; Get handle claim details
(define-read-only (get-handle-claim (claim-id uint))
  (map-get? handle-claims { claim-id: claim-id })
)

;; Get handle verification details
(define-read-only (get-handle-verification (claim-id uint) (validator principal))
  (map-get? handle-claim-verifications { claim-id: claim-id, validator: validator })
)

;; Get handle credential for a participant
(define-read-only (get-handle-credential (principal principal) (credential-id uint))
  (map-get? handle-credentials { principal: principal, credential-id: credential-id })
)

;; Get total number of verified handles
(define-read-only (get-total-verified-handles)
  (var-get total-verified-handles)
)

;; Public functions

;; Set admin (only current admin can change)
(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
    (var-set contract-admin new-admin)
    (ok true)
  )
)

;; Register a new handle claim
(define-public (claim-handle
    (handle (string-ascii 50))
    (config-id uint)
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (claim-id (var-get next-handle-claim-id))
    (now-block-height block-height)
  )
    ;; Validate handle
    (asserts! (is-valid-handle handle config-id) ERR-INVALID-HANDLE-FORMAT)
    (asserts! (is-none (map-get? handles { handle: handle })) ERR-HANDLE-ALREADY-EXISTS)
    (asserts! (> verification-expiry now-block-height) (err u110))
    (asserts! (> verifications-required u0) (err u111))
    
    ;; Create handle claim
    (map-set handle-claims
      { claim-id: claim-id }
      {
        handle: handle,
        claimant: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0
      }
    )
    
    (var-set next-handle-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify a handle claim
(define-public (verify-handle-claim
    (claim-id uint)
    (is-valid bool)
    (verification-proof (string-utf8 500)))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (handle (get handle claim))
    (now-block-height block-height)
  )
    ;; Validate verification
    (asserts! (is-handle-validator handle tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none 
      (map-get? handle-claim-verifications 
        { claim-id: claim-id, validator: tx-sender }
      )
    ) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set handle-claim-verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        verification-proof: verification-proof,
        is-valid: is-valid
      }
    )
    
    ;; Update claim
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
    )
      (map-set handle-claims
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-handle-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Transfer handle ownership
(define-public (transfer-handle-ownership
    (handle (string-ascii 50))
    (new-owner principal))
  (let (
    (current-handle-info (unwrap! (map-get? handles { handle: handle }) ERR-HANDLE-NOT-FOUND))
  )
    ;; Authorization check
    (asserts! (is-eq tx-sender (get owner current-handle-info)) ERR-NOT-AUTHORIZED)
    
    ;; Update handle ownership
    (map-set handles
      { handle: handle }
      (merge current-handle-info { owner: new-owner })
    )
    
    (ok true)
  )
)

;; Finalize handle claim with expired verification period
(define-public (finalize-expired-handle-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u112))
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-handle-claim-status claim-id)
          (ok true))
        (begin
          (map-set handle-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)

;; Register or update handle configuration (admin only)
(define-public (register-handle-config
    (config-id uint)
    (max-length uint)
    (min-length uint)
    (allowed-chars (string-ascii 50))
    (verification-threshold uint)
    (registration-fee uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
    (map-set handle-configs
      { config-id: config-id }
      {
        max-length: max-length,
        min-length: min-length,
        allowed-chars: allowed-chars,
        verification-threshold: verification-threshold,
        registration-fee: registration-fee
      }
    )
    (ok true)
  )
)

;; Register a new handle claim
(define-public (claim-handle
    (handle (string-ascii 50))
    (config-id uint)
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (claim-id (var-get next-handle-claim-id))
    (now-block-height block-height)
  )
    ;; Validate handle
    (asserts! (is-valid-handle handle config-id) ERR-INVALID-HANDLE-FORMAT)
    (asserts! (is-none (map-get? handles { handle: handle })) ERR-HANDLE-ALREADY-EXISTS)
    (asserts! (> verification-expiry now-block-height) (err u110))
    (asserts! (> verifications-required u0) (err u111))
    
    ;; Create handle claim
    (map-set handle-claims
      { claim-id: claim-id }
      {
        handle: handle,
        claimant: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0
      }
    )
    
    (var-set next-handle-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify a handle claim
(define-public (verify-handle-claim
    (claim-id uint)
    (is-valid bool)
    (verification-proof (string-utf8 500)))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (handle (get handle claim))
    (now-block-height block-height)
  )
    ;; Validate verification
    (asserts! (is-handle-validator handle tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none 
      (map-get? handle-claim-verifications 
        { claim-id: claim-id, validator: tx-sender }
      )
    ) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set handle-claim-verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        verification-proof: verification-proof,
        is-valid: is-valid
      }
    )
    
    ;; Update claim
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
    )
      (map-set handle-claims
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-handle-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Transfer handle ownership
(define-public (transfer-handle-ownership
    (handle (string-ascii 50))
    (new-owner principal))
  (let (
    (current-handle-info (unwrap! (map-get? handles { handle: handle }) ERR-HANDLE-NOT-FOUND))
  )
    ;; Authorization check
    (asserts! (is-eq tx-sender (get owner current-handle-info)) ERR-NOT-AUTHORIZED)
    
    ;; Update handle ownership
    (map-set handles
      { handle: handle }
      (merge current-handle-info { owner: new-owner })
    )
    
    (ok true)
  )
)

;; Finalize handle claim with expired verification period
(define-public (finalize-expired-handle-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u112))
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-handle-claim-status claim-id)
          (ok true))
        (begin
          (map-set handle-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)

;; Deactivate impact type (admin only)
(define-public (deactivate-impact-type (impact-type (string-ascii 24)))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
    (let ((impact-info (unwrap! (map-get? impact-types { impact-type: impact-type }) ERR-INVALID-IMPACT-TYPE)))
      (map-set impact-types 
        { impact-type: impact-type }
        (merge impact-info { active: false })
      )
      (ok true)
    )
  )
)

;; Register a new handle claim
(define-public (claim-handle
    (handle (string-ascii 50))
    (config-id uint)
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (claim-id (var-get next-handle-claim-id))
    (now-block-height block-height)
  )
    ;; Validate handle
    (asserts! (is-valid-handle handle config-id) ERR-INVALID-HANDLE-FORMAT)
    (asserts! (is-none (map-get? handles { handle: handle })) ERR-HANDLE-ALREADY-EXISTS)
    (asserts! (> verification-expiry now-block-height) (err u110))
    (asserts! (> verifications-required u0) (err u111))
    
    ;; Create handle claim
    (map-set handle-claims
      { claim-id: claim-id }
      {
        handle: handle,
        claimant: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0
      }
    )
    
    (var-set next-handle-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify a handle claim
(define-public (verify-handle-claim
    (claim-id uint)
    (is-valid bool)
    (verification-proof (string-utf8 500)))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (handle (get handle claim))
    (now-block-height block-height)
  )
    ;; Validate verification
    (asserts! (is-handle-validator handle tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none 
      (map-get? handle-claim-verifications 
        { claim-id: claim-id, validator: tx-sender }
      )
    ) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set handle-claim-verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        verification-proof: verification-proof,
        is-valid: is-valid
      }
    )
    
    ;; Update claim
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
    )
      (map-set handle-claims
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-handle-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Transfer handle ownership
(define-public (transfer-handle-ownership
    (handle (string-ascii 50))
    (new-owner principal))
  (let (
    (current-handle-info (unwrap! (map-get? handles { handle: handle }) ERR-HANDLE-NOT-FOUND))
  )
    ;; Authorization check
    (asserts! (is-eq tx-sender (get owner current-handle-info)) ERR-NOT-AUTHORIZED)
    
    ;; Update handle ownership
    (map-set handles
      { handle: handle }
      (merge current-handle-info { owner: new-owner })
    )
    
    (ok true)
  )
)

;; Finalize handle claim with expired verification period
(define-public (finalize-expired-handle-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u112))
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-handle-claim-status claim-id)
          (ok true))
        (begin
          (map-set handle-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)

;; Register a new project
(define-public (register-project 
    (name (string-utf8 50)) 
    (description (string-utf8 500)) 
    (location (string-utf8 100)))
  (let (
    (project-id (var-get next-project-id))
    (now-block-height block-height)
  )
    (map-set projects 
      { project-id: project-id }
      {
        owner: tx-sender,
        name: name,
        description: description,
        location: location,
        created-at: now-block-height,
        total-verified-impact: u0,
        status: "active"
      }
    )
    ;; Add project owner as a validator
    (map-set project-validators
      { project-id: project-id, validator: tx-sender }
      {
        authorized-at: now-block-height,
        authorized-by: tx-sender
      }
    )
    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

;; Register a new handle claim
(define-public (claim-handle
    (handle (string-ascii 50))
    (config-id uint)
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (claim-id (var-get next-handle-claim-id))
    (now-block-height block-height)
  )
    ;; Validate handle
    (asserts! (is-valid-handle handle config-id) ERR-INVALID-HANDLE-FORMAT)
    (asserts! (is-none (map-get? handles { handle: handle })) ERR-HANDLE-ALREADY-EXISTS)
    (asserts! (> verification-expiry now-block-height) (err u110))
    (asserts! (> verifications-required u0) (err u111))
    
    ;; Create handle claim
    (map-set handle-claims
      { claim-id: claim-id }
      {
        handle: handle,
        claimant: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0
      }
    )
    
    (var-set next-handle-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify a handle claim
(define-public (verify-handle-claim
    (claim-id uint)
    (is-valid bool)
    (verification-proof (string-utf8 500)))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (handle (get handle claim))
    (now-block-height block-height)
  )
    ;; Validate verification
    (asserts! (is-handle-validator handle tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none 
      (map-get? handle-claim-verifications 
        { claim-id: claim-id, validator: tx-sender }
      )
    ) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set handle-claim-verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        verification-proof: verification-proof,
        is-valid: is-valid
      }
    )
    
    ;; Update claim
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
    )
      (map-set handle-claims
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-handle-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Transfer handle ownership
(define-public (transfer-handle-ownership
    (handle (string-ascii 50))
    (new-owner principal))
  (let (
    (current-handle-info (unwrap! (map-get? handles { handle: handle }) ERR-HANDLE-NOT-FOUND))
  )
    ;; Authorization check
    (asserts! (is-eq tx-sender (get owner current-handle-info)) ERR-NOT-AUTHORIZED)
    
    ;; Update handle ownership
    (map-set handles
      { handle: handle }
      (merge current-handle-info { owner: new-owner })
    )
    
    (ok true)
  )
)

;; Finalize handle claim with expired verification period
(define-public (finalize-expired-handle-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u112))
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-handle-claim-status claim-id)
          (ok true))
        (begin
          (map-set handle-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)

;; Add a validator to a project
(define-public (add-project-validator (project-id uint) (validator principal))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    (now-block-height block-height)
  )
    (asserts! (is-eq tx-sender (get owner project)) ERR-NOT-AUTHORIZED)
    (map-set project-validators
      { project-id: project-id, validator: validator }
      {
        authorized-at: now-block-height,
        authorized-by: tx-sender
      }
    )
    (ok true)
  )
)

;; Register a new handle claim
(define-public (claim-handle
    (handle (string-ascii 50))
    (config-id uint)
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (claim-id (var-get next-handle-claim-id))
    (now-block-height block-height)
  )
    ;; Validate handle
    (asserts! (is-valid-handle handle config-id) ERR-INVALID-HANDLE-FORMAT)
    (asserts! (is-none (map-get? handles { handle: handle })) ERR-HANDLE-ALREADY-EXISTS)
    (asserts! (> verification-expiry now-block-height) (err u110))
    (asserts! (> verifications-required u0) (err u111))
    
    ;; Create handle claim
    (map-set handle-claims
      { claim-id: claim-id }
      {
        handle: handle,
        claimant: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0
      }
    )
    
    (var-set next-handle-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify a handle claim
(define-public (verify-handle-claim
    (claim-id uint)
    (is-valid bool)
    (verification-proof (string-utf8 500)))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (handle (get handle claim))
    (now-block-height block-height)
  )
    ;; Validate verification
    (asserts! (is-handle-validator handle tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none 
      (map-get? handle-claim-verifications 
        { claim-id: claim-id, validator: tx-sender }
      )
    ) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set handle-claim-verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        verification-proof: verification-proof,
        is-valid: is-valid
      }
    )
    
    ;; Update claim
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
    )
      (map-set handle-claims
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-handle-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Transfer handle ownership
(define-public (transfer-handle-ownership
    (handle (string-ascii 50))
    (new-owner principal))
  (let (
    (current-handle-info (unwrap! (map-get? handles { handle: handle }) ERR-HANDLE-NOT-FOUND))
  )
    ;; Authorization check
    (asserts! (is-eq tx-sender (get owner current-handle-info)) ERR-NOT-AUTHORIZED)
    
    ;; Update handle ownership
    (map-set handles
      { handle: handle }
      (merge current-handle-info { owner: new-owner })
    )
    
    (ok true)
  )
)

;; Finalize handle claim with expired verification period
(define-public (finalize-expired-handle-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u112))
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-handle-claim-status claim-id)
          (ok true))
        (begin
          (map-set handle-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)

;; Remove a validator from a project
(define-public (remove-project-validator (project-id uint) (validator principal))
  (let ((project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner project)) ERR-NOT-AUTHORIZED)
    (map-delete project-validators { project-id: project-id, validator: validator })
    (ok true)
  )
)

;; Register a new handle claim
(define-public (claim-handle
    (handle (string-ascii 50))
    (config-id uint)
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (claim-id (var-get next-handle-claim-id))
    (now-block-height block-height)
  )
    ;; Validate handle
    (asserts! (is-valid-handle handle config-id) ERR-INVALID-HANDLE-FORMAT)
    (asserts! (is-none (map-get? handles { handle: handle })) ERR-HANDLE-ALREADY-EXISTS)
    (asserts! (> verification-expiry now-block-height) (err u110))
    (asserts! (> verifications-required u0) (err u111))
    
    ;; Create handle claim
    (map-set handle-claims
      { claim-id: claim-id }
      {
        handle: handle,
        claimant: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0
      }
    )
    
    (var-set next-handle-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify a handle claim
(define-public (verify-handle-claim
    (claim-id uint)
    (is-valid bool)
    (verification-proof (string-utf8 500)))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (handle (get handle claim))
    (now-block-height block-height)
  )
    ;; Validate verification
    (asserts! (is-handle-validator handle tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none 
      (map-get? handle-claim-verifications 
        { claim-id: claim-id, validator: tx-sender }
      )
    ) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set handle-claim-verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        verification-proof: verification-proof,
        is-valid: is-valid
      }
    )
    
    ;; Update claim
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
    )
      (map-set handle-claims
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-handle-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Transfer handle ownership
(define-public (transfer-handle-ownership
    (handle (string-ascii 50))
    (new-owner principal))
  (let (
    (current-handle-info (unwrap! (map-get? handles { handle: handle }) ERR-HANDLE-NOT-FOUND))
  )
    ;; Authorization check
    (asserts! (is-eq tx-sender (get owner current-handle-info)) ERR-NOT-AUTHORIZED)
    
    ;; Update handle ownership
    (map-set handles
      { handle: handle }
      (merge current-handle-info { owner: new-owner })
    )
    
    (ok true)
  )
)

;; Finalize handle claim with expired verification period
(define-public (finalize-expired-handle-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u112))
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-handle-claim-status claim-id)
          (ok true))
        (begin
          (map-set handle-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)

;; Update project status
(define-public (update-project-status (project-id uint) (status (string-ascii 10)))
  (let ((project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get owner project)) ERR-NOT-AUTHORIZED)
    (asserts! (or (is-eq status "active") (is-eq status "completed")) (err u111))
    (map-set projects 
      { project-id: project-id }
      (merge project { status: status })
    )
    (ok true)
  )
)

;; Register a new handle claim
(define-public (claim-handle
    (handle (string-ascii 50))
    (config-id uint)
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (claim-id (var-get next-handle-claim-id))
    (now-block-height block-height)
  )
    ;; Validate handle
    (asserts! (is-valid-handle handle config-id) ERR-INVALID-HANDLE-FORMAT)
    (asserts! (is-none (map-get? handles { handle: handle })) ERR-HANDLE-ALREADY-EXISTS)
    (asserts! (> verification-expiry now-block-height) (err u110))
    (asserts! (> verifications-required u0) (err u111))
    
    ;; Create handle claim
    (map-set handle-claims
      { claim-id: claim-id }
      {
        handle: handle,
        claimant: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0
      }
    )
    
    (var-set next-handle-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify a handle claim
(define-public (verify-handle-claim
    (claim-id uint)
    (is-valid bool)
    (verification-proof (string-utf8 500)))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (handle (get handle claim))
    (now-block-height block-height)
  )
    ;; Validate verification
    (asserts! (is-handle-validator handle tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none 
      (map-get? handle-claim-verifications 
        { claim-id: claim-id, validator: tx-sender }
      )
    ) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set handle-claim-verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        verification-proof: verification-proof,
        is-valid: is-valid
      }
    )
    
    ;; Update claim
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
    )
      (map-set handle-claims
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-handle-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Transfer handle ownership
(define-public (transfer-handle-ownership
    (handle (string-ascii 50))
    (new-owner principal))
  (let (
    (current-handle-info (unwrap! (map-get? handles { handle: handle }) ERR-HANDLE-NOT-FOUND))
  )
    ;; Authorization check
    (asserts! (is-eq tx-sender (get owner current-handle-info)) ERR-NOT-AUTHORIZED)
    
    ;; Update handle ownership
    (map-set handles
      { handle: handle }
      (merge current-handle-info { owner: new-owner })
    )
    
    (ok true)
  )
)

;; Finalize handle claim with expired verification period
(define-public (finalize-expired-handle-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u112))
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-handle-claim-status claim-id)
          (ok true))
        (begin
          (map-set handle-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)

;; Submit an impact claim
(define-public (submit-impact-claim
    (project-id uint)
    (impact-type (string-ascii 24))
    (amount uint)
    (evidence-url (string-utf8 200))
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (project (unwrap! (map-get? projects { project-id: project-id }) ERR-PROJECT-NOT-FOUND))
    (impact-info (unwrap! (map-get? impact-types { impact-type: impact-type }) ERR-INVALID-IMPACT-TYPE))
    (claim-id (var-get next-claim-id))
    (now-block-height block-height)
  )
    ;; Validate inputs
    (asserts! (is-eq tx-sender (get owner project)) ERR-NOT-AUTHORIZED)
    (asserts! (get active impact-info) ERR-INVALID-IMPACT-TYPE)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (> verifications-required u0) (err u112))
    (asserts! (> verification-expiry now-block-height) (err u113))
    
    ;; Create impact claim
    (map-set impact-claims
      { claim-id: claim-id }
      {
        project-id: project-id,
        impact-type: impact-type,
        amount: amount,
        evidence-url: evidence-url,
        submitted-by: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0,
        verified-amount: u0
      }
    )
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Register a new handle claim
(define-public (claim-handle
    (handle (string-ascii 50))
    (config-id uint)
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (claim-id (var-get next-handle-claim-id))
    (now-block-height block-height)
  )
    ;; Validate handle
    (asserts! (is-valid-handle handle config-id) ERR-INVALID-HANDLE-FORMAT)
    (asserts! (is-none (map-get? handles { handle: handle })) ERR-HANDLE-ALREADY-EXISTS)
    (asserts! (> verification-expiry now-block-height) (err u110))
    (asserts! (> verifications-required u0) (err u111))
    
    ;; Create handle claim
    (map-set handle-claims
      { claim-id: claim-id }
      {
        handle: handle,
        claimant: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0
      }
    )
    
    (var-set next-handle-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify a handle claim
(define-public (verify-handle-claim
    (claim-id uint)
    (is-valid bool)
    (verification-proof (string-utf8 500)))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (handle (get handle claim))
    (now-block-height block-height)
  )
    ;; Validate verification
    (asserts! (is-handle-validator handle tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none 
      (map-get? handle-claim-verifications 
        { claim-id: claim-id, validator: tx-sender }
      )
    ) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set handle-claim-verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        verification-proof: verification-proof,
        is-valid: is-valid
      }
    )
    
    ;; Update claim
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
    )
      (map-set handle-claims
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-handle-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Transfer handle ownership
(define-public (transfer-handle-ownership
    (handle (string-ascii 50))
    (new-owner principal))
  (let (
    (current-handle-info (unwrap! (map-get? handles { handle: handle }) ERR-HANDLE-NOT-FOUND))
  )
    ;; Authorization check
    (asserts! (is-eq tx-sender (get owner current-handle-info)) ERR-NOT-AUTHORIZED)
    
    ;; Update handle ownership
    (map-set handles
      { handle: handle }
      (merge current-handle-info { owner: new-owner })
    )
    
    (ok true)
  )
)

;; Finalize handle claim with expired verification period
(define-public (finalize-expired-handle-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u112))
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-handle-claim-status claim-id)
          (ok true))
        (begin
          (map-set handle-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)

;; Verify an impact claim as a validator
(define-public (verify-impact-claim
    (claim-id uint)
    (approved bool)
    (verified-amount uint)
    (comments (string-utf8 200)))
  (let (
    (claim (unwrap! (map-get? impact-claims { claim-id: claim-id }) ERR-CLAIM-NOT-FOUND))
    (project-id (get project-id claim))
    (now-block-height block-height)
  )
    ;; Validate
    (asserts! (is-project-validator project-id tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-CLAIM-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none (map-get? verifications { claim-id: claim-id, validator: tx-sender })) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        approved: approved,
        verified-amount: verified-amount,
        comments: comments
      }
    )
    
    ;; Update the claim with this verification
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
      (new-verified-amount (if approved 
                              (if (> (get verified-amount claim) u0)
                                  ;; Average with existing verified amount
                                  (/ (+ (get verified-amount claim) verified-amount) u2)
                                  ;; First verification
                                  verified-amount)
                              ;; Not approved
                              (get verified-amount claim)))
    )
      (map-set impact-claims 
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received,
          verified-amount: new-verified-amount
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Register a new handle claim
(define-public (claim-handle
    (handle (string-ascii 50))
    (config-id uint)
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (claim-id (var-get next-handle-claim-id))
    (now-block-height block-height)
  )
    ;; Validate handle
    (asserts! (is-valid-handle handle config-id) ERR-INVALID-HANDLE-FORMAT)
    (asserts! (is-none (map-get? handles { handle: handle })) ERR-HANDLE-ALREADY-EXISTS)
    (asserts! (> verification-expiry now-block-height) (err u110))
    (asserts! (> verifications-required u0) (err u111))
    
    ;; Create handle claim
    (map-set handle-claims
      { claim-id: claim-id }
      {
        handle: handle,
        claimant: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0
      }
    )
    
    (var-set next-handle-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify a handle claim
(define-public (verify-handle-claim
    (claim-id uint)
    (is-valid bool)
    (verification-proof (string-utf8 500)))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (handle (get handle claim))
    (now-block-height block-height)
  )
    ;; Validate verification
    (asserts! (is-handle-validator handle tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none 
      (map-get? handle-claim-verifications 
        { claim-id: claim-id, validator: tx-sender }
      )
    ) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set handle-claim-verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        verification-proof: verification-proof,
        is-valid: is-valid
      }
    )
    
    ;; Update claim
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
    )
      (map-set handle-claims
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-handle-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Transfer handle ownership
(define-public (transfer-handle-ownership
    (handle (string-ascii 50))
    (new-owner principal))
  (let (
    (current-handle-info (unwrap! (map-get? handles { handle: handle }) ERR-HANDLE-NOT-FOUND))
  )
    ;; Authorization check
    (asserts! (is-eq tx-sender (get owner current-handle-info)) ERR-NOT-AUTHORIZED)
    
    ;; Update handle ownership
    (map-set handles
      { handle: handle }
      (merge current-handle-info { owner: new-owner })
    )
    
    (ok true)
  )
)

;; Finalize handle claim with expired verification period
(define-public (finalize-expired-handle-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u112))
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-handle-claim-status claim-id)
          (ok true))
        (begin
          (map-set handle-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)

;; Register an external data source (admin only)
(define-public (register-data-source
    (source-id (string-ascii 24))
    (name (string-utf8 100))
    (description (string-utf8 200))
    (interface-principal principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) ERR-NOT-AUTHORIZED)
    (map-set authorized-data-sources
      { source-id: source-id }
      {
        name: name,
        description: description,
        interface-principal: interface-principal,
        authorized-at: block-height
      }
    )
    (ok true)
  )
)

;; Register a new handle claim
(define-public (claim-handle
    (handle (string-ascii 50))
    (config-id uint)
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (claim-id (var-get next-handle-claim-id))
    (now-block-height block-height)
  )
    ;; Validate handle
    (asserts! (is-valid-handle handle config-id) ERR-INVALID-HANDLE-FORMAT)
    (asserts! (is-none (map-get? handles { handle: handle })) ERR-HANDLE-ALREADY-EXISTS)
    (asserts! (> verification-expiry now-block-height) (err u110))
    (asserts! (> verifications-required u0) (err u111))
    
    ;; Create handle claim
    (map-set handle-claims
      { claim-id: claim-id }
      {
        handle: handle,
        claimant: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0
      }
    )
    
    (var-set next-handle-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify a handle claim
(define-public (verify-handle-claim
    (claim-id uint)
    (is-valid bool)
    (verification-proof (string-utf8 500)))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (handle (get handle claim))
    (now-block-height block-height)
  )
    ;; Validate verification
    (asserts! (is-handle-validator handle tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none 
      (map-get? handle-claim-verifications 
        { claim-id: claim-id, validator: tx-sender }
      )
    ) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set handle-claim-verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        verification-proof: verification-proof,
        is-valid: is-valid
      }
    )
    
    ;; Update claim
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
    )
      (map-set handle-claims
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-handle-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Transfer handle ownership
(define-public (transfer-handle-ownership
    (handle (string-ascii 50))
    (new-owner principal))
  (let (
    (current-handle-info (unwrap! (map-get? handles { handle: handle }) ERR-HANDLE-NOT-FOUND))
  )
    ;; Authorization check
    (asserts! (is-eq tx-sender (get owner current-handle-info)) ERR-NOT-AUTHORIZED)
    
    ;; Update handle ownership
    (map-set handles
      { handle: handle }
      (merge current-handle-info { owner: new-owner })
    )
    
    (ok true)
  )
)

;; Finalize handle claim with expired verification period
(define-public (finalize-expired-handle-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u112))
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-handle-claim-status claim-id)
          (ok true))
        (begin
          (map-set handle-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)

;; Submit external verification (must be from registered data source)
(define-public (submit-external-verification
    (claim-id uint)
    (source-id (string-ascii 24))
    (approved bool)
    (verified-amount uint)
    (verification-data (string-utf8 500)))
  (let (
    (data-source (unwrap! (map-get? authorized-data-sources { source-id: source-id }) ERR-DATA-SOURCE-NOT-AUTHORIZED))
    (claim (unwrap! (map-get? impact-claims { claim-id: claim-id }) ERR-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Validate
    (asserts! (is-eq tx-sender (get interface-principal data-source)) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status claim) "pending") ERR-CLAIM-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    
    ;; Save external verification
    (map-set external-verifications
      { claim-id: claim-id, source-id: source-id }
      {
        verified-at: now-block-height,
        approved: approved,
        verified-amount: verified-amount,
        verification-data: verification-data
      }
    )
    
    ;; Update the claim with this verification (counts as 2 regular verifications)
    (let (
      (new-verifications-received (+ (get verifications-received claim) u2))
      (new-verified-amount (if approved 
                             (if (> (get verified-amount claim) u0)
                                 ;; Average with existing verified amount but give more weight to external verification
                                 (/ (+ (+ (get verified-amount claim) verified-amount) verified-amount) u3)
                                 ;; First verification
                                 verified-amount)
                             ;; Not approved
                             (get verified-amount claim)))
    )
      (map-set impact-claims 
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received,
          verified-amount: new-verified-amount
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Register a new handle claim
(define-public (claim-handle
    (handle (string-ascii 50))
    (config-id uint)
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (claim-id (var-get next-handle-claim-id))
    (now-block-height block-height)
  )
    ;; Validate handle
    (asserts! (is-valid-handle handle config-id) ERR-INVALID-HANDLE-FORMAT)
    (asserts! (is-none (map-get? handles { handle: handle })) ERR-HANDLE-ALREADY-EXISTS)
    (asserts! (> verification-expiry now-block-height) (err u110))
    (asserts! (> verifications-required u0) (err u111))
    
    ;; Create handle claim
    (map-set handle-claims
      { claim-id: claim-id }
      {
        handle: handle,
        claimant: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0
      }
    )
    
    (var-set next-handle-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify a handle claim
(define-public (verify-handle-claim
    (claim-id uint)
    (is-valid bool)
    (verification-proof (string-utf8 500)))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (handle (get handle claim))
    (now-block-height block-height)
  )
    ;; Validate verification
    (asserts! (is-handle-validator handle tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none 
      (map-get? handle-claim-verifications 
        { claim-id: claim-id, validator: tx-sender }
      )
    ) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set handle-claim-verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        verification-proof: verification-proof,
        is-valid: is-valid
      }
    )
    
    ;; Update claim
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
    )
      (map-set handle-claims
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-handle-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Transfer handle ownership
(define-public (transfer-handle-ownership
    (handle (string-ascii 50))
    (new-owner principal))
  (let (
    (current-handle-info (unwrap! (map-get? handles { handle: handle }) ERR-HANDLE-NOT-FOUND))
  )
    ;; Authorization check
    (asserts! (is-eq tx-sender (get owner current-handle-info)) ERR-NOT-AUTHORIZED)
    
    ;; Update handle ownership
    (map-set handles
      { handle: handle }
      (merge current-handle-info { owner: new-owner })
    )
    
    (ok true)
  )
)

;; Finalize handle claim with expired verification period
(define-public (finalize-expired-handle-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u112))
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-handle-claim-status claim-id)
          (ok true))
        (begin
          (map-set handle-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)

;; Finalize claim with expired verification period
(define-public (finalize-expired-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? impact-claims { claim-id: claim-id }) ERR-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u114))
    (asserts! (is-eq (get status claim) "pending") ERR-CLAIM-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-claim-status claim-id)
          (ok true))
        (begin
          (map-set impact-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)

;; Register a new handle claim
(define-public (claim-handle
    (handle (string-ascii 50))
    (config-id uint)
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (claim-id (var-get next-handle-claim-id))
    (now-block-height block-height)
  )
    ;; Validate handle
    (asserts! (is-valid-handle handle config-id) ERR-INVALID-HANDLE-FORMAT)
    (asserts! (is-none (map-get? handles { handle: handle })) ERR-HANDLE-ALREADY-EXISTS)
    (asserts! (> verification-expiry now-block-height) (err u110))
    (asserts! (> verifications-required u0) (err u111))
    
    ;; Create handle claim
    (map-set handle-claims
      { claim-id: claim-id }
      {
        handle: handle,
        claimant: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0
      }
    )
    
    (var-set next-handle-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify a handle claim
(define-public (verify-handle-claim
    (claim-id uint)
    (is-valid bool)
    (verification-proof (string-utf8 500)))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (handle (get handle claim))
    (now-block-height block-height)
  )
    ;; Validate verification
    (asserts! (is-handle-validator handle tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none 
      (map-get? handle-claim-verifications 
        { claim-id: claim-id, validator: tx-sender }
      )
    ) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set handle-claim-verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        verification-proof: verification-proof,
        is-valid: is-valid
      }
    )
    
    ;; Update claim
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
    )
      (map-set handle-claims
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-handle-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Transfer handle ownership
(define-public (transfer-handle-ownership
    (handle (string-ascii 50))
    (new-owner principal))
  (let (
    (current-handle-info (unwrap! (map-get? handles { handle: handle }) ERR-HANDLE-NOT-FOUND))
  )
    ;; Authorization check
    (asserts! (is-eq tx-sender (get owner current-handle-info)) ERR-NOT-AUTHORIZED)
    
    ;; Update handle ownership
    (map-set handles
      { handle: handle }
      (merge current-handle-info { owner: new-owner })
    )
    
    (ok true)
  )
)

;; Finalize handle claim with expired verification period
(define-public (finalize-expired-handle-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u112))
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-handle-claim-status claim-id)
          (ok true))
        (begin
          (map-set handle-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)
;; Register a new handle claim
(define-public (claim-handle
    (handle (string-ascii 50))
    (config-id uint)
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (claim-id (var-get next-handle-claim-id))
    (now-block-height block-height)
  )
    ;; Validate handle
    (asserts! (is-valid-handle handle config-id) ERR-INVALID-HANDLE-FORMAT)
    (asserts! (is-none (map-get? handles { handle: handle })) ERR-HANDLE-ALREADY-EXISTS)
    (asserts! (> verification-expiry now-block-height) (err u110))
    (asserts! (> verifications-required u0) (err u111))
    
    ;; Create handle claim
    (map-set handle-claims
      { claim-id: claim-id }
      {
        handle: handle,
        claimant: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0
      }
    )
    
    (var-set next-handle-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Register a new handle claim
(define-public (claim-handle
    (handle (string-ascii 50))
    (config-id uint)
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (claim-id (var-get next-handle-claim-id))
    (now-block-height block-height)
  )
    ;; Validate handle
    (asserts! (is-valid-handle handle config-id) ERR-INVALID-HANDLE-FORMAT)
    (asserts! (is-none (map-get? handles { handle: handle })) ERR-HANDLE-ALREADY-EXISTS)
    (asserts! (> verification-expiry now-block-height) (err u110))
    (asserts! (> verifications-required u0) (err u111))
    
    ;; Create handle claim
    (map-set handle-claims
      { claim-id: claim-id }
      {
        handle: handle,
        claimant: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0
      }
    )
    
    (var-set next-handle-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify a handle claim
(define-public (verify-handle-claim
    (claim-id uint)
    (is-valid bool)
    (verification-proof (string-utf8 500)))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (handle (get handle claim))
    (now-block-height block-height)
  )
    ;; Validate verification
    (asserts! (is-handle-validator handle tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none 
      (map-get? handle-claim-verifications 
        { claim-id: claim-id, validator: tx-sender }
      )
    ) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set handle-claim-verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        verification-proof: verification-proof,
        is-valid: is-valid
      }
    )
    
    ;; Update claim
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
    )
      (map-set handle-claims
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-handle-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Transfer handle ownership
(define-public (transfer-handle-ownership
    (handle (string-ascii 50))
    (new-owner principal))
  (let (
    (current-handle-info (unwrap! (map-get? handles { handle: handle }) ERR-HANDLE-NOT-FOUND))
  )
    ;; Authorization check
    (asserts! (is-eq tx-sender (get owner current-handle-info)) ERR-NOT-AUTHORIZED)
    
    ;; Update handle ownership
    (map-set handles
      { handle: handle }
      (merge current-handle-info { owner: new-owner })
    )
    
    (ok true)
  )
)

;; Finalize handle claim with expired verification period
(define-public (finalize-expired-handle-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u112))
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-handle-claim-status claim-id)
          (ok true))
        (begin
          (map-set handle-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)

;; Verify a handle claim
(define-public (verify-handle-claim
    (claim-id uint)
    (is-valid bool)
    (verification-proof (string-utf8 500)))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (handle (get handle claim))
    (now-block-height block-height)
  )
    ;; Validate verification
    (asserts! (is-handle-validator handle tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none 
      (map-get? handle-claim-verifications 
        { claim-id: claim-id, validator: tx-sender }
      )
    ) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set handle-claim-verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        verification-proof: verification-proof,
        is-valid: is-valid
      }
    )
    
    ;; Update claim
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
    )
      (map-set handle-claims
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-handle-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Register a new handle claim
(define-public (claim-handle
    (handle (string-ascii 50))
    (config-id uint)
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (claim-id (var-get next-handle-claim-id))
    (now-block-height block-height)
  )
    ;; Validate handle
    (asserts! (is-valid-handle handle config-id) ERR-INVALID-HANDLE-FORMAT)
    (asserts! (is-none (map-get? handles { handle: handle })) ERR-HANDLE-ALREADY-EXISTS)
    (asserts! (> verification-expiry now-block-height) (err u110))
    (asserts! (> verifications-required u0) (err u111))
    
    ;; Create handle claim
    (map-set handle-claims
      { claim-id: claim-id }
      {
        handle: handle,
        claimant: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0
      }
    )
    
    (var-set next-handle-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify a handle claim
(define-public (verify-handle-claim
    (claim-id uint)
    (is-valid bool)
    (verification-proof (string-utf8 500)))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (handle (get handle claim))
    (now-block-height block-height)
  )
    ;; Validate verification
    (asserts! (is-handle-validator handle tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none 
      (map-get? handle-claim-verifications 
        { claim-id: claim-id, validator: tx-sender }
      )
    ) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set handle-claim-verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        verification-proof: verification-proof,
        is-valid: is-valid
      }
    )
    
    ;; Update claim
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
    )
      (map-set handle-claims
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-handle-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Transfer handle ownership
(define-public (transfer-handle-ownership
    (handle (string-ascii 50))
    (new-owner principal))
  (let (
    (current-handle-info (unwrap! (map-get? handles { handle: handle }) ERR-HANDLE-NOT-FOUND))
  )
    ;; Authorization check
    (asserts! (is-eq tx-sender (get owner current-handle-info)) ERR-NOT-AUTHORIZED)
    
    ;; Update handle ownership
    (map-set handles
      { handle: handle }
      (merge current-handle-info { owner: new-owner })
    )
    
    (ok true)
  )
)

;; Finalize handle claim with expired verification period
(define-public (finalize-expired-handle-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u112))
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-handle-claim-status claim-id)
          (ok true))
        (begin
          (map-set handle-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)

;; Transfer handle ownership
(define-public (transfer-handle-ownership
    (handle (string-ascii 50))
    (new-owner principal))
  (let (
    (current-handle-info (unwrap! (map-get? handles { handle: handle }) ERR-HANDLE-NOT-FOUND))
  )
    ;; Authorization check
    (asserts! (is-eq tx-sender (get owner current-handle-info)) ERR-NOT-AUTHORIZED)
    
    ;; Update handle ownership
    (map-set handles
      { handle: handle }
      (merge current-handle-info { owner: new-owner })
    )
    
    (ok true)
  )
)

;; Register a new handle claim
(define-public (claim-handle
    (handle (string-ascii 50))
    (config-id uint)
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (claim-id (var-get next-handle-claim-id))
    (now-block-height block-height)
  )
    ;; Validate handle
    (asserts! (is-valid-handle handle config-id) ERR-INVALID-HANDLE-FORMAT)
    (asserts! (is-none (map-get? handles { handle: handle })) ERR-HANDLE-ALREADY-EXISTS)
    (asserts! (> verification-expiry now-block-height) (err u110))
    (asserts! (> verifications-required u0) (err u111))
    
    ;; Create handle claim
    (map-set handle-claims
      { claim-id: claim-id }
      {
        handle: handle,
        claimant: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0
      }
    )
    
    (var-set next-handle-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify a handle claim
(define-public (verify-handle-claim
    (claim-id uint)
    (is-valid bool)
    (verification-proof (string-utf8 500)))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (handle (get handle claim))
    (now-block-height block-height)
  )
    ;; Validate verification
    (asserts! (is-handle-validator handle tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none 
      (map-get? handle-claim-verifications 
        { claim-id: claim-id, validator: tx-sender }
      )
    ) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set handle-claim-verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        verification-proof: verification-proof,
        is-valid: is-valid
      }
    )
    
    ;; Update claim
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
    )
      (map-set handle-claims
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-handle-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Transfer handle ownership
(define-public (transfer-handle-ownership
    (handle (string-ascii 50))
    (new-owner principal))
  (let (
    (current-handle-info (unwrap! (map-get? handles { handle: handle }) ERR-HANDLE-NOT-FOUND))
  )
    ;; Authorization check
    (asserts! (is-eq tx-sender (get owner current-handle-info)) ERR-NOT-AUTHORIZED)
    
    ;; Update handle ownership
    (map-set handles
      { handle: handle }
      (merge current-handle-info { owner: new-owner })
    )
    
    (ok true)
  )
)

;; Finalize handle claim with expired verification period
(define-public (finalize-expired-handle-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u112))
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-handle-claim-status claim-id)
          (ok true))
        (begin
          (map-set handle-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)

;; Finalize handle claim with expired verification period
(define-public (finalize-expired-handle-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u112))
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-handle-claim-status claim-id)
          (ok true))
        (begin
          (map-set handle-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)

;; Register a new handle claim
(define-public (claim-handle
    (handle (string-ascii 50))
    (config-id uint)
    (verification-expiry uint)
    (verifications-required uint))
  (let (
    (claim-id (var-get next-handle-claim-id))
    (now-block-height block-height)
  )
    ;; Validate handle
    (asserts! (is-valid-handle handle config-id) ERR-INVALID-HANDLE-FORMAT)
    (asserts! (is-none (map-get? handles { handle: handle })) ERR-HANDLE-ALREADY-EXISTS)
    (asserts! (> verification-expiry now-block-height) (err u110))
    (asserts! (> verifications-required u0) (err u111))
    
    ;; Create handle claim
    (map-set handle-claims
      { claim-id: claim-id }
      {
        handle: handle,
        claimant: tx-sender,
        submitted-at: now-block-height,
        status: "pending",
        verification-expiry: verification-expiry,
        verifications-required: verifications-required,
        verifications-received: u0
      }
    )
    
    (var-set next-handle-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify a handle claim
(define-public (verify-handle-claim
    (claim-id uint)
    (is-valid bool)
    (verification-proof (string-utf8 500)))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (handle (get handle claim))
    (now-block-height block-height)
  )
    ;; Validate verification
    (asserts! (is-handle-validator handle tx-sender) ERR-NOT-VALIDATOR)
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    (asserts! (< now-block-height (get verification-expiry claim)) ERR-VERIFICATION-PERIOD-ENDED)
    (asserts! (is-none 
      (map-get? handle-claim-verifications 
        { claim-id: claim-id, validator: tx-sender }
      )
    ) ERR-ALREADY-VERIFIED)
    
    ;; Save verification
    (map-set handle-claim-verifications
      { claim-id: claim-id, validator: tx-sender }
      {
        verified-at: now-block-height,
        verification-proof: verification-proof,
        is-valid: is-valid
      }
    )
    
    ;; Update claim
    (let (
      (new-verifications-received (+ (get verifications-received claim) u1))
    )
      (map-set handle-claims
        { claim-id: claim-id }
        (merge claim {
          verifications-received: new-verifications-received
        })
      )
      
      ;; Check if we have enough verifications to update status
      (if (>= new-verifications-received (get verifications-required claim))
          (begin
            (update-handle-claim-status claim-id)
            (ok true))
          (ok true))
    )
  )
)

;; Transfer handle ownership
(define-public (transfer-handle-ownership
    (handle (string-ascii 50))
    (new-owner principal))
  (let (
    (current-handle-info (unwrap! (map-get? handles { handle: handle }) ERR-HANDLE-NOT-FOUND))
  )
    ;; Authorization check
    (asserts! (is-eq tx-sender (get owner current-handle-info)) ERR-NOT-AUTHORIZED)
    
    ;; Update handle ownership
    (map-set handles
      { handle: handle }
      (merge current-handle-info { owner: new-owner })
    )
    
    (ok true)
  )
)

;; Finalize handle claim with expired verification period
(define-public (finalize-expired-handle-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? handle-claims { claim-id: claim-id }) ERR-HANDLE-CLAIM-NOT-FOUND))
    (now-block-height block-height)
  )
    ;; Check if verification period has ended and claim is still pending
    (asserts! (>= now-block-height (get verification-expiry claim)) (err u112))
    (asserts! (is-eq (get status claim) "pending") ERR-HANDLE-ALREADY-PROCESSED)
    
    ;; If we have enough verifications, verify the claim, otherwise reject it
    (if (>= (get verifications-received claim) (get verifications-required claim))
        (begin
          (update-handle-claim-status claim-id)
          (ok true))
        (begin
          (map-set handle-claims
            { claim-id: claim-id }
            (merge claim { status: "rejected" })
          )
          (ok false))
    )
  )
)
