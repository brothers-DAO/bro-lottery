# BRO Lottery API

## For public-use clients: Buying a ticket

2 possibilities:
  - Use the low-level core module: **NS.bro-lottery**
  - Use the helpers module: **NS.bro-lottery-helpers**

### Using the helpers module

The helper modules define functions to buy tickets in $BRO, KDA or other fungible.

1 - Verify using a local call that ```(tickets-for-sale)``` returns ```true```

2 - Retrieve the ticket price by either:
  - ```(ticket-price-in-bro)```
  - ```(ticket-price-in-kda)```
  - ```(ticket-price-in-fungible f)```

3 - Retrieve the payment account by either:
  - ```(sales-account-in-bro)```
  - ```(sales-account-in-kda)```
  - ```(sales-account-in-fungible f)```

*Note that the sales-accounts are supposed to be fixed. This can be hardcoded or cached in the Dapp.*

In case of a sale in KDA or fungible, a slippage tolerance should be added to the ticket price (eg 0.5% / 1%)


4 - Buying a ticket is done by calling one of these functions:
  - ```(buy-ticket-in-bro account star-number)```
  - ```(buy-ticket-in-kda account max-amount star-number)```
  - ```(buy-ticket-in-fungible f account max-amount star-number)```

The transaction must be signed with the cap `(xxx.TRANSFER account sales-account max-amount)`

*max-amount* being the ticket-price retrieved in 3 + slippage tolerance.

*sales-account* being the account retrieved in 2 .


### Using the core module (low-level)

1 - Call ```(round-state)``` and check that it returns "RUNNING"

2 - Call ```(ticket-price)``` to retrieve the ticket price.

3 - retrieve the ```TICKET-SALES-ACCOUNT``` constant. A

4 - Buying a ticket is done by calling the function: ```(buy-ticket account:string star-number:integer)```, where:
  - ```account-name``` is the account who will receive the prize.
  - ```start-number``` is an Int between 0 and 10

Before calling ```(buy-ticket)```, all the following pre-conditions MUST be met:
  - account-name must already have an existing $BRO account
  - the ticket-price in $BRO must have previously sent to ```TICKET-SALES-ACCOUNT``` (for security it has to be done atomically: in the same transaction)


## View functions (Core contract)
Functions and date usable by a Frontend or another contract in Read-only mode:

#### Round Object
Each round is reference by a unique 8 bytes ID, computed at creation.

```pact
  (defschema lottery-round
    id:string ; A 8 chars ID
    start-time:time ; Start-time : only informative
    end-time:time ; End-time: end of tickets sales
    ticket-price:decimal ; Ticket price
    tickets-count:integer ; Number of sold tickets, automatically incremented
    tickets-limit:integer ; Limits of tickets being sold for this round
    inner-seed:string ; Internal seed (see doc), automatically updated by each sale
    btc-height:integer ; The minimum BTC height for retrieving the external seed
    settlement-tx:string ; Hash of the settlement TX: empty is not settled
  )
```

#### Result object

This object is created and stored when the round is settled.

```pact
  (defschema lottery-result
    inner-seed:integer ; Copy of the internal seed of the round
    btc-height:integer ; BTC height used for the external seed
    seed:integer ; Seed used for computing the results (Internal seed XOR external seed)
    star-number:integer ; Drawn STAR Number
    winning-tickets:[integer] ; Drawn winning tickets
    final-round-bal:decimal ; Final balance of the main pool
    final-jackpot-bal:decimal ; Final Jackpot balance
    jackpot-won:bool ; Whether the Jackpot was won
  )
```

#### Ticket object
```pact
(defschema ticket
  round-id:string ; Round-id linked to this ticket
  rank:integer ; Rank of the ticket in the round
  account:string ; Account to pay the winnings
  star-number:integer ; STAR Number chosen by the user
)
```

**Note:** A ticket has a rank number: automatically incremented and started from 0 for each round.
The tuple *(round-id, rank)* represents an unique ticket.

### Functions

`(current-round-id)`: *-> string* : Return the current Round-ID

`(current-round)`: *-> object{lottery-round}* : Return the current Round

`(get-round id)`: *string -> object{lottery-round}* : Return the round object from an ID

`(get-result id)`: *string -> object{lottery-result}* : Return the result object from a Round-ID

`(get-ticket round-id rank)`: *string integer-> object{ticket}* : Return a ticket from a tuple *(round-id, rank)*

`(get-all-tickets round-id)`: *string -> [object{ticket}]* : Return all tickets sold for a given Round-ID

`(round-state)`: *-> string*: Return the current round state : `"STARTING"`, `"RUNNING"`, `"ENDED"` or `"SETTLED"`

`(round-balance id)` *string -> decimal*: Return the $BRO balance of the prize pool for a round ID

`(jackpot-balance id)` *-> decimal*: Return the $BRO balance of the Jackpot

`(get-all-rounds)` *-> [object{lottery-round}]*: Return all rounds (current and archives)








## Administrative and/or special functions

TBC
