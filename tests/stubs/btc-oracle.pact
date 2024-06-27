(module btc_oracle_mod GOV
  (defcap GOV ()
    (enforce false "Module not upgradable"))

  (defschema btc-block
    header:string
    ts:time
    header-hash:integer
    target:integer
    height:integer
  )

  (defun select-block:object{btc-block} (after-height:integer after-time:time confirmations:integer)
    (enforce (= 3553 after-height) "Invalid height")
    (enforce (= 1 confirmations) "Invalid confirmations")

    {'header:"",
     'ts: (time "2024-06-24T11:26:54Z"),
     'header-hash:1000,
     'target: 1500,
     'height: 3555
   }
  )

  (defun select-hash:integer (after-height:integer after-time:time confirmations:integer)
    (at 'header-hash (select-block after-height after-time confirmations)))


  (defun est-btc-height-at-time:integer (target:time)
    @doc "Estimate a BTC height at a given time"
    3553
  )
)
