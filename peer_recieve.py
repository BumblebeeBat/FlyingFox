"""When a peer talks to us, this is how we generate a response. This is the external API.
"""
import networking, custom, tools, blockchain, time
def security_check(dic):
    if 'version' not in dic or dic['version'] != custom.version:
        return {'bool': False, 'error': 'version'}
    else:
        #we could add security features here.
        return {'bool': True, 'newdic': dic}
def recieve_peer(dic, DB):
    if 'peers' in dic:
        map(tools.add_peer, dic['peers'])
    else:
        tools.add_peer(dic['peer'])
def blockCount(dic, DB):
    length = tools.local_get('length')
    return {'length': length}
def rangeRequest(dic, DB):
    ran = dic['range']
    out = []
    counter = 0
    while (len(tools.package(out)) < custom.max_download
           and ran[0] + counter <= ran[1]):
        block = tools.db_get(ran[0] + counter, DB)
        if 'length' in block:
            out.append(block)
        counter += 1
    return out
def txs(dic, DB):
    return tools.local_get('txs')
def pushtx(dic, DB):
    DB['suggested_txs'].put(dic['tx'])
    return 'success'
def pushblock(dic, DB):
    length=tools.local_get('length')
    block = tools.db_get(length, DB)    
    if 'peer' in dic: peer=dic['peer']
    else: peer=False
    if 'blocks' in dic:
        if length>=0:
            for i in range(20):
                if tools.fork_check(dic['blocks'], DB, length, block):
                    blockchain.delete_block(DB)
                    length-=1
        for block in dic['blocks']:
            DB['suggested_blocks'].put([block, peer])
    else:
        DB['suggested_blocks'].put([dic['block'], peer])
    return 'success'
def peers(dic, DB): return tools.local_get('peers')
def main(dic, DB):
    #tools.log(dic)
    funcs = {'recieve_peer':recieve_peer, 'blockCount': blockCount, 'rangeRequest': rangeRequest,'txs': txs, 'pushtx': pushtx, 'pushblock': pushblock, 'peers':peers}
    if 'type' not in dic:
        return 'oops: ' +str(dic)
    if dic['type'] not in funcs:
        return ' '.join([dic['type'], 'is not in the api'])
    check = security_check(dic)
    if not check['bool']:
        return check
    try:
        return funcs[dic['type']](check['newdic'], DB)
    except Exception as exc:
        tools.log(exc)

