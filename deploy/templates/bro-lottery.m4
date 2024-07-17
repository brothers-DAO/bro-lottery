include(`defs.m4')dnl
dnl
(namespace "LOTTERY_NS")
include(`../pact/bro-lottery.pact')dnl

ifdef(`__INIT__',dnl
(create-table result-table)
(create-table round-table)
(create-table current-table)
(create-table ticket-table)
(init), (enforce-admin-accounts))dnl
