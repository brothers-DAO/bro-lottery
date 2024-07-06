# BRO Lottery Bot

A bot to autmatically settle lottery rounds, and create a new ones as soon it's possible.

Requires a `.env` (see envsample) file with:
  - An account + Associated key/pair to buy gas.
  - A key pair referenced by the "op" keyset (creating round is restricted)


## How to.

Install it by
```sh
yarn install
```

Then run by:
```sh
node index.js`
```
