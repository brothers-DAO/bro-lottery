import hashlib
import json
from itertools import islice

#BTC_HEADER = "000000000000000000028e4a5ec89b76c99f4b933aaeb7a8d10331cae735bf4f"
#INTRISIC_SEED = 84865656326219339874807823925781056513853571110542331706044102493316005280053
#TICKETS_COUNT = 13

INTRISIC_SEED = int(input("Final intrinsic seed: "))
BTC_HEADER = input("Used BTC Header (starts with 000000.....): ")
TICKETS_COUNT = int(input("Number of sold tickets: "))


def serialize_int(x):
    return json.dumps({"int":str(x)}, separators=(',', ':'))
     
def k_hash(x):
    h = hashlib.blake2b(digest_size=32)
    h.update(x.encode('ascii'))
    return int.from_bytes(h.digest(), "big")

def dedup(tickets_it):
    seen = set()
    for x in tickets_it:
        if x not in seen:
            seen.add(x)
            yield x

# Reverse the endianess of the bitcoin BlockHash
extrinsic_seed = int.from_bytes(bytes.fromhex(BTC_HEADER), byteorder="little")

# Compute the seed
seed = INTRISIC_SEED ^ extrinsic_seed
print("Seed is: {:d}".format(seed))

# Take 16 Winning tickets
winners_16 = map(lambda x: k_hash(serialize_int(seed ^ x)) % TICKETS_COUNT, range(1,16))

# Take the first 3 non duplicate
winners_3 = islice(dedup(winners_16), 3)

for i,x in enumerate(winners_3):
    print("Ticket {:d} is = {:d}".format(i+1,x))

# Star Number
star_number = k_hash(serialize_int(seed ^ 888)) % 10
print("Star number is = {:d}".format(star_number))


