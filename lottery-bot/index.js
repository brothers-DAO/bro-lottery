import {} from 'dotenv/config'
import {Pact, createSignWithKeypair} from '@kadena/client'
import {local_pact, local_check, submit, status} from "./utils/pact.js";
import {randomUUID} from 'node:crypto'
import Decimal from 'decimal.js'
import * as dateMath from 'date-arithmetic'

const NODE = process.env.NODE;
const NETWORK_ID = process.env.NETWORKID;
const CHAIN_ID = process.env.CHAINID;
const lottery_mod = process.env.LOTTERY_MOD;
const SENDER = process.env.SENDER;
const SENDER_PUBKEY = process.env.SENDER_PUBKEY;
const OP_PUBKEY = process.env.OP_PUBKEY;

const PRICE = Decimal(process.env.TICKET_PRICE);
const TICKETS_LIMIT = parseInt(process.env.TICKETS_LIMIT);

const DURATION = parseInt(process.env.DURATION);
const DURATION_UNIT = process.env.DURATION_UNIT;

function display_config()
{
  console.log(`Node Host: ${NODE}`)
  console.log(`Module location: ${NETWORK_ID} / ${CHAIN_ID} / ${lottery_mod}`)
  console.log(`Sender: ${SENDER} (${SENDER_PUBKEY})`)
  console.log(`Op Pubkey: ${OP_PUBKEY}`)
  console.log(`Ticket Price: ${PRICE.toFixed(4)}`)
  console.log(`Tickets limit: ${TICKETS_LIMIT}`)
  console.log(`Round Duration: ${DURATION} ${DURATION_UNIT}`)
}

var lock = false;


const signer = createSignWithKeypair({ publicKey: SENDER_PUBKEY, secretKey: process.env.SENDER_PRIVKEY });
const op_signer = createSignWithKeypair({ publicKey: OP_PUBKEY, secretKey: process.env.OP_PRIVKEY });



const make_nonce = () => "In_Brothers_We_Trust:"+randomUUID()

const to_date = x=> x.time?new Date(x.time):new Date(x.timep)

const timep = x => x?{timep:x.toISOString()}:null

function get_state()
{
  return local_pact(`(${lottery_mod}.round-state)`, NETWORK_ID, CHAIN_ID )
}

function get_round_endtime()
{
  return local_pact(`(${lottery_mod}.current-round)`, NETWORK_ID, CHAIN_ID )
         .then( ({"end-time":et}) => to_date(et))
}

function try_compute_result()
{
  return local_pact(`(${lottery_mod}.compute-result (${lottery_mod}.current-round))`, NETWORK_ID, CHAIN_ID)
         .then(()=> ({ok:true, msg:""}))
         .catch((e) => ({ok:false, msg:e.toString()}))
}

const settle_transaction = () =>  Pact.builder.execution(`(${lottery_mod}.settle-round)`)
                                              .setMeta({chainId:CHAIN_ID, gasLimit:10000, gasPrice:1e-8, sender:SENDER})
                                              .setNetworkId(NETWORK_ID)
                                              .setNonce(make_nonce)
                                              .addSigner(SENDER_PUBKEY, (signFor) => [signFor("coin.GAS")])
                                              .createTransaction();

const create_round_transaction = (ed) =>  Pact.builder.execution(`(${lottery_mod}.create-round ${PRICE.toFixed(6)} ${TICKETS_LIMIT} (read-msg 'end))`)
                                                      .addData("end", timep(ed))
                                                      .setMeta({chainId:CHAIN_ID, gasLimit:5000, gasPrice:1e-8, sender:SENDER})
                                                      .setNetworkId(NETWORK_ID)
                                                      .setNonce(make_nonce)
                                                      .addSigner(SENDER_PUBKEY, (signFor) => [signFor("coin.GAS")])
                                                      .addSigner(OP_PUBKEY, (signFor) => [signFor(`${lottery_mod}.CREATE-ROUND`)])
                                                      .createTransaction();


async function do_settle()
{
    lock = true;
    const cmd = settle_transaction()
    const signed_cmd = await signer(cmd)
    await local_check(signed_cmd)
          .then(()=> console.log("Local OK"))
          .then(() => submit(signed_cmd))
          .then(()=> console.log("Submitted"))
          .then(() => status(signed_cmd, NETWORK_ID, CHAIN_ID))
          .then(x => console.log(x?.result?.status == "success"?"Settle OK":"Settle Error"))
          .finally(() => {console.log("-----------------------------------------");lock=false})
}

async function do_create()
{
  lock = true;
  /* We take the previous round end-time and try to add the duration */
  /* We repeat it until the new end-time is at least to hours in the future */
  /* By doing that, we handle properly "missed" rounds */
  const last_end = await get_round_endtime()
  const now = dateMath.add(new Date(), "2", "hours")
  let new_end = last_end
  while(new_end < now)
    new_end = dateMath.add(new_end, DURATION, DURATION_UNIT)

  console.log(`Previous end-time:${last_end.toISOString()}`)
  console.log(`New end-time:${new_end.toISOString()}`)

  const cmd = create_round_transaction(new_end)
  const signed_cmd = await signer(cmd)
                           .then(op_signer)
  await local_check(signed_cmd)
        .then(()=> console.log("Local OK"))
        .then(() => submit(signed_cmd))
        .then(()=> console.log("Submitted"))
        .then(() => status(signed_cmd, NETWORK_ID, CHAIN_ID))
        .then(x => console.log(x?.result?.status == "success"?"Create round OK":"Create round Error"))
        .finally(() => {console.log("-----------------------------------------");lock=false})
}


async function do_process(verbose)
{
  if(lock)
    return;

  if(verbose)
    console.log("--------------> GETTING STATE <--------------")
  const state = await get_state()

  if(verbose)
    console.log(`State = ${state}`)

  if(state == "ENDED")
  {
    const {ok,msg} = await try_compute_result();
    if(ok)
    {
      console.log("The draw can be settled")
      await do_settle()
    }
    else if(verbose)
      console.log(msg)
  }

  if(state == "SETTLED")
  {
    await do_create()


  }

}

async function run()
{
  console.log("|---------------------------------------------|");
  console.log("|      Starting $BRO Lottery bot              |");
  console.log("|---------------------------------------------|");
  display_config();
  await new Promise(r => setTimeout(r, 30_000));
  do_process(true)
  setInterval(do_process, 31_000);
  setInterval(()=>do_process(true), 600_000);
}

run()
