"""We regularly check on peers to see if they have mined new blocks.
This file explains how we initiate interactions with our peers.
"""
import time, networking, tools, blockchain, custom, random, sys
def cmd(peer, x): #return networking.send_command(peer, x)
    if type(peer)!=list:
        peer=tools.peer_split(peer)
    return networking.send_command(peer, x)
def download_blocks(peer, DB, peers_block_count, length):
    #tools.log('download blocks')
    b=[max(0, length-10), min(peers_block_count, length+custom.download_many)]
    #tools.log('b: ' +str(b))
    blocks = cmd(peer, {'type': 'rangeRequest', 'range': b})
    #tools.log('b: ' +str(blocks))
    if type(blocks)!=list: return -1
    if not isinstance(blocks, list): return []
    length=tools.local_get('length')
    if length>=0:
        block=tools.db_get(length)
        for i in range(10):#this part should be re-written so badly
            if tools.fork_check(blocks, DB, length, block):
                blockchain.delete_block(DB)
                length-=1
    for block in blocks:
        DB['suggested_blocks'].put([block, peer])
    return 0
def ask_for_txs(peer, DB):
    txs = cmd(peer, {'type': 'txs'})
    if not isinstance(txs, list):
        return -1
    txs=filter(lambda t: t['type']!='mint', txs)
    for tx in txs:
        DB['suggested_txs'].put(tx)
    T=tools.local_get('txs')
    pushers = filter(lambda t: t not in txs, T)
    for push in pushers:
        cmd(peer, {'type': 'pushtx', 'tx': push})
    return 0
def give_block(peer, DB, block_count_peer):
    blocks=[]
    b=[max(block_count_peer-5, 0), min(tools.local_get('length'), block_count_peer+custom.download_many)]
    for i in range(b[0], b[1]+1):
        blocks.append(tools.db_get(i, DB))
    cmd(peer, {'type': 'pushblock',
               'blocks': blocks})
    return 0
def ask_for_count(peer):
    peers=tools.local_get('peers')
    block_count = cmd(peer, {'type': 'blockCount'})
    if not isinstance(block_count, dict):
        return
    if 'error' in block_count.keys():
        return
    peers[peer]['length']=block_count['length']
    tools.local_put('peers', peers)
def trade_peers(peer):
    peers=tools.local_get('peers')
    peer_length=peers[peer]['length']
    their_peers=cmd(peer, {'type':'peers'})
    if type(their_peers)!=dict: return {'error': 'cannot connect'}
    if 'error' in their_peers.keys(): return {'error': 'cannot connect'}
    def minus(a, b): return filter(lambda p: p not in b, a)
    to_them=minus(peers.keys(), their_peers.keys())
    to_me=minus(their_peers.keys(), peers.keys())
    for p in to_me:
        if not ':' in p:
            p=p+':'+str(their_peers[p]['port'])
        tools.log('peer: ' +str(p))
        tools.add_peer(p)
    cmd(peer, {'type':'recieve_peer', 'peers':to_them})
def peer_check(peer, DB):
    peers=tools.local_get('peers')
    if peers[peer]['length']==0 or random.random()<0.1:
        ask_for_count(peer)
        out=trade_peers(peer)
        if type(out)==dict and 'error' in out: return 1
    peers=tools.local_get('peers')
    length = tools.local_get('length')
    us = length
    them = peers[peer]['length']
    if them < us:
        #tools.log('less than')
        return give_block(peer, DB, peers[peer]['length'])
    elif us == them:
        #tools.log('equal')
        try:
            return ask_for_txs(peer, DB)
        except Exception as exc:
            tools.log('ask for tx error')
            tools.log(exc)
    else:
        return download_blocks(peer, DB, peers[peer]['length'], length)
def exponential_random(r, i=0):
    if random.random()<r: return i
    return exponential_random(r, i+1)
def main(peers, DB):
    map(tools.add_peer, peers)
    try:
        while True:
            time.sleep(0.5)#changing this from 0.01 to 0.5 made blocks load way faster. the add_block queue was getting overfilled.
            if tools.local_get('stop'): return
            main_once(DB)
    except Exception as exc:
        tools.log(exc)
def main_once(DB):
    pr=tools.local_get('peers')
    keys=filter(lambda x: pr[x]['blacklist']<500, pr.keys())
    keys=sorted(keys, key=lambda r: pr[r]['lag'])
    if len(keys)<1:
        time.sleep(0.5)
        return
    time.sleep(0.05)
    while not DB['suggested_blocks'].empty():
        time.sleep(0.1)
        if tools.local_get('stop'): return 0
    i=exponential_random(9.0/10)%len(keys)
    t1=time.time()
    r=peer_check(keys[i], DB)
    t2=time.time()
    a=0.5
    pr=tools.local_get('peers')
    pr[keys[i]]['lag']*=(1-a)
    if r==0: a*=(t2-t1)
    else:
        a*=60
    pr[keys[i]]['lag']+=a
    tools.local_put('peers', pr)



