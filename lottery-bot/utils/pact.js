import {createClient, Pact} from '@kadena/client'

const LOCAL_GAS_LIMIT = 10000

const host = process.env.NODE && `${process.env.NODE}/chainweb/0.0/${process.env.NETWORKID}/chain/${process.env.CHAINID}/pact`

console.log(host)

const client = createClient(host)

function local_check(cmd, options)
{
  return client.local(cmd, options)
        .then((resp) => { if(resp?.result?.status !== 'success')
                           {throw Error(`Error in local call:${resp?.result?.error?.message}`);}
                          else
                            return resp.result.data;});
}

function local_pact(pact_code, network, chain)
{
  const cmd = Pact.builder
                  .execution(pact_code)
                  .setMeta({chainId:chain, gasLimit:LOCAL_GAS_LIMIT})
                  .setNetworkId(network)
                  .createTransaction();
  return local_check(cmd, {signatureVerification:false, preflight:false});
}

function submit(cmd)
{
  return client.submitOne(cmd)
}

function status(cmd, network, chain)
{
  return client.pollStatus({requestKey:cmd.hash, chainId:chain , networkId: network},
                           {timeout:300_000, interval:10_000})
               .then( x=> x?.[cmd.hash])
}


export {local_check, local_pact, submit, status}
