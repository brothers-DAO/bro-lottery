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










## Administrative and/or special functions

TBC
