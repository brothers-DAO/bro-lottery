include(`defs.m4')dnl
(namespace 'BTC_ORACLE_NS)
include(`../kadena-btc-oracle/btc-oracle.pact')dnl
(create-table btc-block-table)
(create-table tip-table)

dnl Double check the block height
(init-block "0020592cd8409aea33d7867eff30a82025c686c74174b2d877ac00000000000000000000fc3159ab767fc1d161b8d7b250dcb904001389d553cff3521d0b49cb5d0e4a8ba01398666d8a03171c7b14c2" 852652)
