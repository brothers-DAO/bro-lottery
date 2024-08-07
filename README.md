# BRO Lottery

## Modules (chain 2)
- Main module: `n_e47192edb40ff014ab9c82ae42972d09bbad4316.bro-lottery`
- Helpers modules : `n_e47192edb40ff014ab9c82ae42972d09bbad4316.bro-lottery-helpers`

## Accounts (chain 2)
- Sales (Deposit account) in $BRO: `c:SW1jyng-6fFDJW_sOTN5JV2T-fkn2NZJkZK2c3Sqgv8`
- Jackpot account in $BRO: `c:gCT8JcKDsYGG8RznKEdm_Lpu-dacVrbM9cAsBK3DGww`


## Rules

The round duration is 7 days. Its not possible to buy a ticket after the end-date.

Each participant buy a ticket, either:
   - 0.001 $BRO per round = 1 GiDoug
   - The corresponding amount in KDA (swapped automatically on EckoDEX)
   - The corresponding amount in zUSD(swapped automatically on EckoDEX)

The number of tickets is limited to 200 tickets. A single entity / account can buy several tickets.
During the sale, for each ticket the participant choose a **Star Number** between 0 and 9


Ticket price repartition:
  - 90% goes to the Prize pool
  - 10% goes to the Jackpot pool.

After the end-date + 2 hours, we randomly choose:
  - "1st prize" Ticket
  - "2nd prize" Ticket
  - "3rd prize" Ticket
  - A Star number

If the winner (1st prize) finds the right "Star number", he wins 80% of the Jackpot Pool.

| ~                            | Main Prize Pool  | Jackpot pool              |
|------------------------------|------------------|---------------------------|
| 1st prize                    | 50 %             | 80 % if Star Number found |
| 2nd prize                    | 25 %             |                           |
| 3rd prize                    | 15 %             |                           |
| -> Community fund            |  5 %             |                           |
| Fees                         |  5 %             |                            |
| Redirected for next round    |                  | 20% if Start Number found, 100% Otherwise

The Jackpot pool is initialized with 0.3 $BRO from the community fund.

In case we stop the lottery, the remaining $BRO in the Jackpot pool will be transferred back to the community fund.

Anyway, we will try as much as possible, to keep new rounds running until someone wins the Jackpot.

### Example
Example for the first round in case of 200 tickets sold at 0.01 $BRO, and the first prize finds the Star number.

| ~                            | Main Prize Pool  | Jackpot pool              |
|------------------------------|------------------|---------------------------|
| 1st prize                    | 0.95 $BRO        | 0.4 $BRO                  |
| 2nd prize                    | 0.4875 $BRO      |                           |
| 3rd prize                    | 0.2925 $BRO      |                           |
| -> Community fund            | 0.0975 $BRO      |                           |
| Fees                         | 0.0975 $BRO      |                           |
| Redirected for next round    |                  | 0.1 $BRO                  |


## Entropy / Seed Generation
The Draw Seed is defined as:

Seed = Intrinsic Seed ⊕ Extrinsic Seed

Intrinsic Seed is:
Blake2( *TXIDn* || Blake2( *TXIDn-1* || ( Blake2 (*TXIDn-2* || ........*TXID_init*))))

Where *TXIDn* is the TXid of the transaction where the Ticket n was bought.

Extrinsic Seed is:
The SHA256 hash of the **FIRST** Bitcoin block that meets the following conditions:
  - Block height >= Target Height
  - Block Timestamp >= Target Timestamp
  - Confirmed by at least another block.

where:
   - Target Height = BTC Height at start-date + 1008 (number of blocks expected for 7 days)
   - Target Timestamp = end-date + 2 hours

## Draw
The Smart contract draws 16 potential winning tickets:

1st = Blake2(Seed ⊕ 1) modulo N

2nd = Blake2(Seed ⊕ 2) modulo N

...

16st = Blake2(Seed ⊕ 16) modulo N

(N being the total number of tickets)

After removing duplicates, the 3 first tickets are designed as the 1st, 2nd and 3rd prize. This method prevents a single ticket for winning several prizes.

Star Number = Blake2(Seed ⊕ 888) modulo 10

## Licensing
 - Files under `/pact` (smart contracts) are licensed under Business Source License 1.1 (`BUSL-1.1`), see `LICENSE`.
 - Files under `/lottery-bot` are licensed under BSD License 2.0  (`BSD-3-Clause`)
 - Files under `/tests`, `/deploy` remains unlicensed  
