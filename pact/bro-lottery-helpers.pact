(module bro_lottery_helpers_mod GOVERNANCE
  (use bro_lottery_mod)
  (use DEX_NS.exchange)

  (defcap GOVERNANCE ()
    true)

  (defun tickets-for-sale: bool()
    @doc "Return true if the lottery is running"
    (= (round-state) "RUNNING"))

  ;-----------------------------------------------------------------------------
  ; PRIVATE UTILITY FUNCTIONS
  ;-----------------------------------------------------------------------------
  (defun --try-create-bro-account:string (account:string bro-guard:guard)
    @doc "Create a $BRO account when it doesn't already exist"
    (let ((bal (try -1.0 (BRO_NS.bro.get-balance account))))
      (if (= bal -1.0)
          (BRO_NS.bro.create-account account bro-guard)
          ""))
  )

  (defun try-create-bro-account:string (account:string from:module{fungible-v2})
    @doc "Create a $BRO account when it doesn't already exist"
    (--try-create-bro-account account (at 'guard (from::details account))))

  (defun --enforce-list-length (expected-length:integer lst:[integer])
    @doc "Check that a list has the given length"
    (enforce (= expected-length (length lst)) "Length of star numbers doesn't match"))

  (defun -safe:decimal (x:decimal y:decimal)
    @doc "Substract and enforce a strictly positive result"
    ; In the context of calculations below, it means: not enough liquidity to buy a ticket
    (enforce (> x y) "Not enough liquidity")
    (- x y))

  (defun --apply-fee:decimal (x:decimal)
    @doc "Apply the DEX fee on an amount"
    (* x (- 1.0 FEE)))

  ;-----------------------------------------------------------------------------
  ; SALES IN BRO
  ;-----------------------------------------------------------------------------
  (defun sales-account-in-bro:string ()
    TICKET-SALES-ACCOUNT)

  (defun ticket-price-in-bro:decimal ()
    (ticket-price))

  (defun tickets-price-in-bro:decimal (count:integer)
    (* (dec count) (ticket-price)))

  (defun buy-ticket-in-bro:string (account:string star-number:integer)
    @doc "Buy a single ticket directly in $BRO"
    (BRO_NS.bro.transfer account TICKET-SALES-ACCOUNT (ticket-price))
    (buy-ticket account star-number)
  )

  (defun buy-ticket-in-bro-batch:[string] (account:string count:integer star-numbers:[integer])
    @doc "Buy several tickets in $BRO"
    (--enforce-list-length count star-numbers)
    (BRO_NS.bro.transfer account TICKET-SALES-ACCOUNT (tickets-price-in-bro count))
    (map (buy-ticket account) star-numbers)
  )

  ;-----------------------------------------------------------------------------
  ; SALES IN KDA
  ;-----------------------------------------------------------------------------
  (defun sales-account-in-kda:string ()
    (at 'account (get-pair coin BRO_NS.bro)))

  (defun ticket-price-in-kda-batch:decimal (count:integer)
    @doc "Return the tickets price in KDA for a given count of tickets"
    (let ((p (get-pair coin BRO_NS.bro)))
      (ceiling (/ (* (reserve-for p coin) (tickets-price-in-bro count))
                  (--apply-fee (-safe (reserve-for p BRO_NS.bro) (tickets-price-in-bro count))))
               (coin.precision)))
  )

  (defun ticket-price-in-kda:decimal ()
    @doc "Return the tickets price in KDA"
    (ticket-price-in-kda-batch 1))

  (defun buy-ticket-in-kda:string (account:string max-amount:decimal star-number:integer)
    @doc "Buy ticket in KDA"
    (swap-exact-out (ticket-price) max-amount [coin BRO_NS.bro] account TICKET-SALES-ACCOUNT TICKET-SALES-GUARD)
    (try-create-bro-account account coin)
    (buy-ticket account star-number)
  )

  (defun buy-ticket-in-kda-batch:[string] (account:string count:integer max-amount:decimal star-numbers:[integer])
    @doc "Buy several tickets in $KDA"
    (--enforce-list-length count star-numbers)
    (swap-exact-out (tickets-price-in-bro count) max-amount [coin BRO_NS.bro] account TICKET-SALES-ACCOUNT TICKET-SALES-GUARD)
    (try-create-bro-account account coin)
    (map (buy-ticket account) star-numbers)
  )

  ;-----------------------------------------------------------------------------
  ; SALES IN ANOTHER FUNGIBLE
  ;-----------------------------------------------------------------------------
  (defun sales-account-in-fungible:string (currency:module{fungible-v2})
    (at 'account (get-pair coin currency)))

  (defun ticket-price-in-fungible-batch:decimal (currency:module{fungible-v2} count:integer)
    @doc "Return the ticket price in another fungible for a given count of tickets"
    ; This is not really gas optimized => But not a big deal since this function is mainly
    ; supposed to be local called
    (let* ((p (get-pair coin currency)))
      (ceiling (/ (* (reserve-for p currency) (ticket-price-in-kda-batch count))
                  (--apply-fee (-safe (reserve-for p coin) (ticket-price-in-kda-batch count))))
               (currency::precision)))
  )

  (defun ticket-price-in-fungible:decimal (currency:module{fungible-v2})
      @doc "Return the ticket price in another fungible"
      (ticket-price-in-fungible-batch currency 1))

  (defun buy-ticket-in-fungible:string (currency:module{fungible-v2} account:string max-amount:decimal star-number:integer)
    @doc "Buy a ticket using another fungible"
    (swap-exact-out (ticket-price) max-amount [currency coin BRO_NS.bro] account TICKET-SALES-ACCOUNT TICKET-SALES-GUARD)
    (try-create-bro-account account currency)
    (buy-ticket account star-number)
  )

  (defun buy-ticket-in-fungible-batch:[string] (currency:module{fungible-v2} account:string count:integer max-amount:decimal star-numbers:[integer])
    @doc "Buy several tickets in $KDA"
    (--enforce-list-length count star-numbers)
    (swap-exact-out (tickets-price-in-bro count) max-amount [currency coin BRO_NS.bro] account TICKET-SALES-ACCOUNT TICKET-SALES-GUARD)
    (try-create-bro-account account currency)
    (map (buy-ticket account) star-numbers)
  )

)
