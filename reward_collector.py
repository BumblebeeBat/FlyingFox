import api, custom, tools, time, random

def create_reward_tx():
    tx={}
    tx['type']='reward'
    length=tools.local_get('length')
    tx['on_block']=length-custom.long_time+random.randint(-custom.medium_time/2, custom.medium_time/2)
    if tx['on_block']<=0:
        time.sleep(1)
        return {'error':'no rewards to collect'}
    address=tools.local_get('address')
    acc=tools.db_get(address)
    if str(tx['on_block']) in acc['entropy']:
        return {'error':'already collected that reward'}
    zeroths=tools.local_get('txs')
    zeroths=filter(lambda t: tools.addr(t)==address, zeroths)
    zeroths=filter(lambda t: t['type']=='reward', zeroths)
    if len(zeroths)>0:
        {'error':'already made the tx to collect that reward'}
    txs=tools.db_get(tx['on_block'])['txs']
    txs=filter(lambda t: t['type']=='sign', txs)
    #tools.log('on block: ' +str(tx['on_block']))
    #tools.log('txs: ' +str(txs))
    sign_tx=filter(lambda t: tools.addr(t)==address, txs)[0]
    #tools.log('txs: ' +str(sign_tx))
    relative_reward=tools.relative_reward(tx['on_block'], address)
    tx['amount']=relative_reward+sign_tx['amount']
    tx['reveal']=tools.local_get('secrets')[str(tx['on_block'])]
    tx['jackpots']=len(sign_tx['jackpots'])
    return tx
def mainloop():
    while True:
        time.sleep(1)
        tx=create_reward_tx()
        if tools.local_get('stop'): return
        if 'error' not in tx:
            api.easy_add_transaction(tx)
def doit():
    try:
        mainloop()
    except Exception as exc:
        tools.log('reward collector error')
        tools.log(exc)
        
