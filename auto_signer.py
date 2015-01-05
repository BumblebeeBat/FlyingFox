import transactions, tools, custom, time, sys, api, random
def create_sign_tx():
    on_block=tools.local_get('length')+1
    if on_block==0:
        time.sleep(1)
        return{'error':'not ready'}
    r=tools.det_random(on_block)
    jackpots=[]
    address=tools.local_get('address')
    l=max(-1, on_block-1-(custom.long_time*2-custom.medium_time))
    election_block=tools.db_get(l+1)
    proof=tools.local_get('balance_proofs'+str(l))
    if 'root_hash' not in election_block:
        return({'error':'database changed'})
    a=tools.db_verify(election_block['root_hash'], address, proof)
    if a==False:
        #tools.log('election block: ' +str(election_block))
        #tools.log('proof: ' +str(proof))
        return({'error':'not valid proof'})
    old_balance=a['amount']
    M=custom.all_money
    for j in range(custom.jackpot_nonces):
        if tools.winner(old_balance, M, r, address, j):
            jackpots.append(j)
    if len(jackpots)>0:
        tx={'on_block':on_block, 'jackpots':jackpots, 'type':'sign', 'amount':M/3000/3}
        tx['B']=old_balance
        tx['proof']=proof
        if proof=='empty':
            time.sleep(1)
            return {'error':'not ready'}
        secrets=tools.local_get('secrets')
        if str(on_block) in secrets:
            secret=secrets[str(on_block)]
        else:
            secret=tools.unpackage(tools.package({'salt':str(random.random())+str(random.random()), 'entropy':random.randint(0,1)}))
            secrets[str(on_block)]=secret
            tools.local_put('secrets', secrets)
        tx['secret_hash']=tools.det_hash(secret)
        if on_block>0:
            block=tools.db_get(on_block-1)
            if 'amount' in block and block['amount']==0:
                return({'error':'database changed'})
            #tools.log('on_block: ' +str(a))
            tx['prev']=block['block_hash']
    else:
        tx= {'error':'no jackpots'}
    return tx
#first off, I need to collect a proof of how much money I had at every single block. Deal with jackpots in a seperate thread.
def mainloop():
    while True:
        if tools.local_get('stop'):
            return
        time.sleep(1)
        txs=tools.local_get('txs')
        address=tools.local_get('address')
        txs=filter(lambda x: address==tools.addr(x), txs)
        txs=filter(lambda x: x['type']=='sign', txs)
        if len(txs)==0:
            tx=create_sign_tx()
            #tools.log('tx: ' +str(tx))
            api.easy_add_transaction(tx)
        else:
            time.sleep(1)

