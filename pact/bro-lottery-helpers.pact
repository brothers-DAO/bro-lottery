(module bro_lottery_helpers_mod GOVERNANCE
  (use bro_lottery_mod)
  (use DEX_NS.exchange)

  (defcap GOVERNANCE ()
    true)

  ;-----------------------------------------------------------------------------
  ; UTILITY FUNCTION
  ;-----------------------------------------------------------------------------
  (defun --try-create-bro-account:string (account:string bro-guard:guard)
    @doc "Create a $BRO account when it doesn't already exist"
    (let ((bal (try -1.0 (BRO_NS.bro.get-balance account))))
      (if (= bal -1.0)
          (BRO_NS.bro.create-account account bro-guard)
          ""))
  )

  (defun tickets-for-sale: bool()
    @doc "Return true if the lottery is running"
    (= (round-state) "RUNNING"))

  ;-----------------------------------------------------------------------------
  ; SALES IN BRO
  ;-----------------------------------------------------------------------------
  (defun sales-account-in-bro:string ()
    TICKET-SALES-ACCOUNT)

  (defun ticket-price-in-bro:decimal ()
    (ticket-price))

  (defun buy-ticket-in-bro:string (account:string star-number:integer)
    @doc "Buy a single ticket directly in $BRO"
    (BRO_NS.bro.transfer account TICKET-SALES-ACCOUNT (ticket-price))
    (buy-ticket account star-number)
  )

  (defun buy-ticket-in-bro-batch:[string] (account:string count:integer star-numbers:[integer])
    @doc "Buy several tickets in $BRO"
    (enforce (= count (length star-numbers)) "Length of star numbers doesn't match")
    (BRO_NS.bro.transfer account TICKET-SALES-ACCOUNT (* (dec count) (ticket-price)))
    (map (buy-ticket account) star-numbers)
  )

  ;-----------------------------------------------------------------------------
  ; SALES IN KDA
  ;-----------------------------------------------------------------------------
  (defun sales-account-in-kda:string ()
    (at 'account (get-pair coin BRO_NS.bro)))

  (defun ticket-price-in-kda:decimal ()
    @doc "Return the ticket price in KDA"
    (let ((p (get-pair coin BRO_NS.bro)))
      (ceiling (/ (* (reserve-for p coin) (ticket-price))
                  (* (- (reserve-for p BRO_NS.bro) (ticket-price)) (- 1.0 FEE)))
               (coin.precision)))
  )

  (defun buy-ticket-in-kda:string (account:string max-amount:decimal star-number:integer)
    @doc "Buy ticket in KDA"
    (swap-exact-out (ticket-price) max-amount [coin BRO_NS.bro] account TICKET-SALES-ACCOUNT TICKET-SALES-GUARD)
    (--try-create-bro-account account (at 'guard (coin.details account)))

    (buy-ticket account star-number)
  )

  ;-----------------------------------------------------------------------------
  ; SALES IN ANOTHER FUNGIBLE
  ;-----------------------------------------------------------------------------
  (defun sales-account-in-fungible:string (currency:module{fungible-v2})
    (at 'account (get-pair coin currency)))

  (defun ticket-price-in-fungible:decimal (currency:module{fungible-v2})
    @doc "Return the ticket price in another fungible"
    (let ((p (get-pair coin currency)))
      (ceiling (/ (* (reserve-for p currency) (ticket-price-in-kda))
                  (* (- (reserve-for p coin) (ticket-price-in-kda)) (- 1.0 FEE)))
               (currency::precision)))
  )

  (defun buy-ticket-in-fungible:string (currency:module{fungible-v2} account:string max-amount:decimal star-number:integer)
    @doc "Buy a ticket using another fungible"
    (swap-exact-out (ticket-price) max-amount [currency coin BRO_NS.bro] account TICKET-SALES-ACCOUNT TICKET-SALES-GUARD)
    (--try-create-bro-account account (at 'guard (currency::details account)))

    (buy-ticket account star-number)
  )

)
