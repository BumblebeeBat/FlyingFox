""" This file explains explains the rules for adding and removing blocks from the local chain.
"""
import time
import copy
import custom
import networking
import transactions
import tools
import db


def add_tx(tx, DB={}):
    # Attempt to add a new transaction into the pool.
    #print('top of add_tx')
    out=['']
    if type(tx) != type({'a':1}): 
        return False
    address = tools.make_address(tx['pubkeys'], len(tx['signatures']))
    '''
    def verify_count(tx, txs):
        return tx['count'] != tools.count(address, DB)
    '''
    def repeat_check(tx, txs):
        l=tools.local_get('length')
        if l<=1: return True
        h=tx['recent_hash']
        r=range(l-10, l)
        r=filter(lambda l: l>0, r)
        recent_blocks=map(lambda x:tools.db_get(x), r)
        recent_hashes=map(lambda x: x['block_hash'], recent_blocks)
        if h not in recent_hashes:
            tools.log('have : ' +str(h))
            tools.log('need: ' +str(recent_hashes))
            tools.log('recent hash error')
            return False
        recent_txs=[]
        for block in recent_blocks:
            recent_txs+=block['txs']
        def f(d):
            d=copy.deepcopy(d)
            d.pop('signatures')
            return tools.det_hash(d)
        if f(tx) in map(f, recent_txs):
            tools.log('no repeated tx')
            return False
        return True
    def type_check(tx, txs):
        if not tools.E_check(tx, 'type', [str, unicode]):
            out[0]+='blockchain type'
            return False
        if tx['type'] not in transactions.tx_check:
            out[0]+='bad type'
            return False
        return True
    def too_big_block(tx, txs):
        return len(tools.package(txs+[tx])) > networking.MAX_MESSAGE_SIZE - 5000
    def verify_tx(tx, txs, out):
        #do not allow tx which fail to reference one of the 10 most recent blocks. do not allow tx which have an identical copy in the last 10 blocks.
        if not type_check(tx, txs):
            out[0]+='type error'
            return False
        if tx in txs:
            out[0]+='no duplicates'
            return False
        #if verify_count(tx, txs):
        #    out[0]+='count error'
        #    return False
        if too_big_block(tx, txs):
            out[0]+='too many txs'
            return False
        if not tools.fee_check(tx, txs, DB):
            out[0]+='not enough money: ' +str(tx)
            return False
        if not tools.signature_check(tx):
            out[0]+='bad signature: ' +str(tx)
            return False
        if not transactions.tx_check[tx['type']](tx, txs, out, DB):
            out[0]+= 'tx: ' + str(tx)
            return False
        return True
    #tools.log('attempt to add tx: ' +str(tx))
    T=tools.local_get('txs')
    try:
        if verify_tx(tx, T, out):
            T.append(tx)
            tools.local_put('txs', T)
            return('added tx: ' +str(tx))
        else:
            return('failed to add tx because: '+out[0])
    except Exception as exc:
        tools.log(exc)
        return('failed to add tx because an error occured while adding tx')
def recent_blockthings(key, size, length=0):
    storage = tools.local_get(key)
    def get_val(length):
        leng = str(length)
        if not leng in storage:            
            block=tools.db_get(leng)
            if block==db.default_entry():
                if leng==tools.local_get('length'):
                    tools.local_put('length', int(leng)-1)
                    block=tools.db_get(leng)
                else:
                    error()
            #try:
            storage[leng] = tools.db_get(leng)[key[:-1]]
            tools.db_put(key, storage)
        return storage[leng]
    def clean_up(storage, end):
        if end<0: return
        if not str(end) in storage: return
        else:
            storage.pop(str(end))
            return clean_up(storage, end-1)
    if length == 0:
        length = tools.local_get('length')
    start = max((length-size), 0)
    clean_up(storage, length-max(custom.mmm, custom.history_length)-100)
    return map(get_val, range(start, length))
def hexSum(a, b):
    # Sum of numbers expressed as hexidecimal strings
    return tools.buffer_(str(hex(int(a, 16)+int(b, 16)))[2: -1], 64)
def hexInvert(n):
    # Use double-size for division, to reduce information leakage.
    return tools.buffer_(str(hex(int('f' * 128, 16) / int(n, 16)))[2: -1], 64)
def add_block(block_pair, recent_hashes, DB={}):
    """Attempts adding a new block to the blockchain.
     Median is good for weeding out liars, so long as the liars don't have 51%
     hashpower. """
    def median(mylist):
        if len(mylist) < 1:
            return 0
        return sorted(mylist)[len(mylist) / 2]

    def block_check(block, DB):
        def log_(txt): pass #return tools.log(txt)
        def tx_check(txs):
            start = copy.deepcopy(txs)
            out = []
            start_copy = []
            invalid_because = ['']
            while start != start_copy:
                if start == []:
                    return False  # Block passes this test
                start_copy = copy.deepcopy(start)
                if transactions.tx_check[start[0]['type']](start[0], out, invalid_because, DB):
                    out.append(start.pop())
                else:
                    tools.log('invalid tx: '+str(invalid_because[0]))
                    return True  # Block is invalid
            tools.log('block invalid because it has no txs')
            return True  # Block is invalid
        if 'error' in block: 
            log_('error in block')
            return False
        length =tools.local_get('length')
        if type(block['length'])!=type(1): 
            log_('wrong length type')
            return False
        if int(block['length']) != int(length) + 1:
            log_('wrong longth')
            return False
        block_creator_address=tools.addr(block)
        mint_address=tools.addr(filter(lambda t: t['type']=='mint', block['txs'])[0])
        if block_creator_address!=mint_address:
            log_('bad mint')
            return False
        if block['root_hash']!=tools.db_root():
            log_('bad root, have: '+str(tools.db_root())+'  need ' +str(block['root_hash']))
            return False
        txs=filter(lambda x: x['type']=='mint', block['txs'])
        if len(txs)!=1:
            log_('wrong number of mint txs')
            return False
        txs=filter(lambda x: x['type']=='sign', block['txs'])
        txs=map(lambda x: len(x['jackpots']), txs)
        if sum(txs)<custom.signers*2/3 and length>-1:
            log_('not enough signatures')
            return False
        if length >= 0:
            prev_block=tools.db_get(length)
            to_hash={'prev_hash':prev_block['block_hash'], 'txs':block['txs']}
            if not block['block_hash']==tools.det_hash(to_hash):
                log_('det hash error')
                return False
        #total money spent must be less than the total amount of money in signed deposits for this block.
        if tx_check(block['txs']): 
            log_('tx check')
            return False
        return True
    if type(block_pair)==type([1,2,3]):
        block=block_pair[0]
        peer=block_pair[1]
    else:
        block=block_pair
        peer=False
    if 'block_hash' in block and block['block_hash'] in recent_hashes:
        #tools.log('we already have that block:' +str(block))
        return 0
    #tools.log('attempt to add block: ' +str(block))
    if block_check(block, DB):
        #tools.log('add_block: ' + str(block))
        tools.db_put(block['length'], block, DB)
        tools.local_put('height', block['height'])
        #take money from the creator
        tools.local_put('length', block['length'])
        orphans = tools.local_get('txs')
        orphans=filter(lambda t: t['type']!='mint', orphans)
        tools.local_put('txs', [])
        for tx in block['txs']:
            try:
                transactions.update[tx['type']](tx, DB, True)
            except Exception as exc:
                tools.log('blockchain broke while adding block. Current datafiles are probably corrupted, and should be deleted.')
                tools.log(exc)
                error()
        for tx in orphans:
            add_tx(tx, DB)
        peers=tools.local_get('peers')
        if peer!=False and peers[peer]['blacklist']>0:
            peers[peer]['blacklist']-=1
        tools.local_put('peers', peers)#root hash written on the block is for the state before that block
        tools.local_put('balance_proofs'+str(block['length']),tools.db_proof(tools.local_get('address')))
        return
    elif not peer==False:
        peers=tools.local_get('peers')
        if peer not in peers:
            peers[peer]=tools.empty_peer()
        peers[peer]['blacklist']+=1
def delete_block(DB):
    """ Removes the most recent block from the blockchain. """
    length=tools.local_get('length')
    if length < 0:
        return
    try:
        ts=tools.local_get('targets')
        ts.pop(str(length))
        tools.local_put('targets', ts)
    except:
        pass
    try:
        ts=tools.local_get('times')
        ts.pop(str(length))
        tools.local_put('times', ts)
    except:
        pass
    block = tools.db_get(length, DB)
    orphans = tools.local_get('txs')
    orphans=filter(lambda t: t['type']!='mint', orphans)
    tools.local_put('txs', [])
    for tx in block['txs']:
        orphans.append(tx)
        tools.local_put('add_block', False)
        transactions.update[tx['type']](tx, DB, False)
    tools.db_delete(length, DB)
    length-=1
    tools.local_put('length', length)
    if length>=0:
        block=tools.db_get(length)
        tools.local_put('height', filter(lambda t: t['type']=='mint', block['txs'])[0]['height'])
    else:
        tools.local_put('height', -1)
    for orphan in orphans:
        add_tx(orphan, DB)
    #while tools.db_get('length')!=length:
    #    time.sleep(0.0001)
def f(blocks_queue, txs_queue):
    def bb(): return blocks_queue.empty()
    def tb(): return txs_queue.empty()
    def ff(queue, g, b, s):
        while not b():
            time.sleep(0.0001)
            try:
                g(queue.get(False))
            except Exception as exc:
                tools.log('suggestions ' + s)
                tools.log(exc)
    while True:
        try:
            time.sleep(0.1)
            l=tools.local_get('length')+1
            v=range(l-10, l)
            v=filter(lambda x: x>0, v)
            v=map(lambda x: tools.db_get(x), v)
            v=map(lambda x: x['block_hash'], v)
            if tools.local_get('stop'):
                tools.dump_out(blocks_queue)
                tools.dump_out(txs_queue)
                return
            while not bb() or not tb():
                ff(blocks_queue, lambda x: add_block(x, v), bb, 'block')
                ff(txs_queue, add_tx, tb, 'tx')
        except Exception as exc:
            tools.log(exc)
import cProfile
def main(DB): return f(DB["suggested_blocks"], DB["suggested_txs"])
def profile(DB):
    import pprint
    p=cProfile.Profile()
    p.run('blockchain.main(custom.DB)')
    g=p.getstats()
    #g=g.sorted(lambda x: x.inlinetime)
    g=sorted(g, key=lambda x: x.totaltime)
    g.reverse()
    pprint.pprint(g)
    #return f(DB['suggested_blocks'], DB['suggested_txs'])
    
