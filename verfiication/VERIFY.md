# How to verify the result of a draw

For f**'s sake, **don't trust, verify** !!!

---

The result of a draw can be verified:

1 - Take the intrinsic seed. This seed was definitively frozen after the last ticket was sold.

2 - Find the **FIRST** Bitcoin Block, after the predefined Height, whose TS is at least 2 hours after the end of sales. Take its hash.

3 - Compute Seed = Intrisic_Seed XOR BTC_BlockHash.
  - *Note: BTC_BlockHash starting with 0x000.. must be interpreted in Little Endian*

4 - Find the 16 "Virtual winning tickets":
  - For `rank` from 1 to 16:
    - Compute Blake2 (seed XOR `rank`) Modulo Tickets_Count
    - *Note: before hashing, the integer must be serialized following Pact Rules*

5 - Deduplicate the 16 "Virtual winning tickets", and take the 3 firsts: **You have a 3 winning tickets rank**

6 - Compute Blake2 (seed XOR `rank`) Modulo 10: **You have the Lucky Star Number**
  - *Note: before hashing, the integer must be serialized following Pact Rules*

---

A small python script is provided as an example, and can be used to reproduce the computation.
