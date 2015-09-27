"""This is the internal API. These are the words that are used to interact with a local node that you have the password to.
"""
import copy, tools, blockchain, custom, random, transactions, sys, time, networking
def add_recent_hash(tx):
    length=tools.local_get('length')
    if 'recent_hash' not in tx and length>0:
        b=tools.db_get(max(1,length-2))['block_hash']
        tools.log('b: ' +str(b))
        tx['recent_hash']=b
    return tx
def sign(tx, privkey):
    pubkey=tools.privtopub(privkey)
    address=tools.make_address([pubkey], 1)
    tx=add_recent_hash(tx)
    if 'pubkeys' not in tx:
        tx['pubkeys']=[pubkey]
    if 'signatures' not in tx:
        tx['signatures']=[tools.sign(tools.det_hash(tx), privkey)]
    return tx
def easy_add_transaction(tx_orig, DB={}, privkey='default'):
    tx = copy.deepcopy(tx_orig)
    length=tools.local_get('length')
    if 'recentHash' not in tx and length>3:
        tx['recentHash']=tools.db_get(length-2)['block_hash']
    if privkey in ['default', 'Default']:
        privkey=tools.local_get('privkey')
    tx=sign(tx_orig, privkey)
    custom.DB['suggested_txs'].put(tx)
    return('success')#blockchain.add_tx(tx, DB))#this is a mistake. It should append to the queue instead.
def help_(DB, args):      
    tell_about_command={
        'help':'type \'./cli.py help <cmd>\' to learn about <cmd>. type \'./cli.py commands\' to get a list of all commands',
        'commands':'returns a list of the commands',
        'start':'type \'./cli.py start\' to start a full node',
        'new_address':'type \'./cli.py new_address <brain>\' to make a new privkey, pubkey, and address using the brain wallet=<brain>. If you want to use this address, you need to copy/paste the pubkey into the file custom.py',
        'DB_print':'prints the database that is shared between threads',
        'patty_info':'This is like "info", but it accesses the database that is mirrored accross all the nodes',
        'info':'prints the contents of an entree in the hashtable. If you want to know what the first block was: info 0, if you want to know about a particular address <addr>: info <addr>, if you want to know about yourself: info address',
        'my_address':'tells you your own address',
        'spend':'spends money, in satoshis, to an address <addr>. Example: spend 1000 11j9csj9802hc982c2h09ds 50',
        'blockcount':'returns the number of blocks since the genesis block',
        'txs':'returns a list of the zeroth confirmation transactions that are expected to be included in the next block',
        'my_balance':'the amount of money that you own',
        'balance':'if you want to know the balance for address <addr>, type: ./cli.py balance <addr>',
        'log':'records the following words into the file \'log.py\'',
        'stop':'This is the correct way to stop the node. If you turn off in any other way, then you are likely to corrupt your database, and you have to redownload all the blocks again.',
        'DB':'returns a database of information that is shared between threads',
        'pushtx':'publishes this transaction to the blockchain, will automatically sign the transaction if necessary: ./cli.py pushtx tx privkey',
        'peers':'tells you your list of peers',
        'buy_block':'./cli.py buy_block'
    }
    if len(args)==0:
        return("needs 2 words. example: 'help help'")
    try:
        return tell_about_command[args[0]]    
    except:
        return(str(args[0])+' is not a word in the help documentation.')
def peers(DB, args):
    return(tools.local_get('peers'))
def DB_print(DB, args):
    return(DB)
def patty_info(DB, args):
    if len(args)<1:
        return ('not enough inputs')
    if args[0]=='my_address':
        address=tools.local_get('address')
    else:
        address=args[0]
    return(tools.db_get(address, DB))    
def info(DB, args): 
    if len(args)<1:
        return ('not enough inputs')
    address=args[0]
    try:
        return(tools.local_get(address))
    except Exception as exc:
        tools.log(exc)
        return(address+'  is not in the local database. maybe you meant to do the command: "patty_info '+address+'"?')
def my_address(DB, args):
    return(tools.local_get('address'))
def spend(DB, args): 
    if len(args)<2:
        return('not enough inputs')
    if len(args)<3:
        args[2]=custom.default_spend_fee#default fee
    return easy_add_transaction({'type': 'spend', 'amount': int(args[0]), 'to':args[1], 'fee':args[2], 'recent_hash':tools.db_get(max(1,int(tools.local_get('length'))-2))['block_hash']}, DB)
def accumulate_words(l, out=''):
    if len(l)>0: return accumulate_words(l[1:], out+' '+l[0])
    return out
def pushtx(DB, args):
    tx=tools.unpackage(args[0].decode('base64'))
    if len(args)==1:
        return easy_add_transaction(tx, DB)
    privkey=tools.det_hash(args[1])
    return easy_add_transaction(tx, DB, privkey)
def blockcount(DB, args): return(tools.local_get('length'))
def txs(DB, args):        return(tools.local_get('txs'))
def my_balance(DB, args, address='default'): 
    if address=='default':
        address=tools.local_get('address')
    return(tools.db_get(address, DB)['amount']-tools.cost_0(tools.local_get('txs'), address))
def balance(DB, args): 
    if len(args)<1:
        return('what address do you want the balance for?')
    return(my_balance(DB, args, args[0]))
def log(DB, args): tools.log(accumulate_words(args)[1:])
def stop_(DB, args): 
    tools.local_put('stop', True)
    return('turning off all threads')
def commands(DB, args): return sorted(Do.keys()+['start', 'new_address'])
def default_block(n, h, txs=[]):
    return({'length':int(n), 'height':h, 'txs':txs, 'version':custom.version, 'block_hash':''})
def mint_tx(gap):
    txs=tools.local_get('txs')
    height=tools.local_get('height')
    on_block=int(tools.local_get('length'))+1
    return {'type':'mint', 'fee':tools.mint_cost(txs, gap), 'on_block':on_block, 'height':height+gap}
def buy_block(DB, args):
    gap=1#this should be an argument. 
    #we should also let the user delete as many blocks first as they want, to build a fork from a point in history.
    length=tools.local_get('length')
    prev_block=tools.db_get(length)
    txs=tools.local_get('txs')
    privkey=tools.local_get('privkey')
    height=tools.local_get('height')
    block=default_block(length+1, height+gap, txs+[sign(mint_tx(gap), privkey)])
    to_hash=''
    if length>-1: to_hash={'prev_hash':prev_block['block_hash'], 'txs':block['txs']}
    block['block_hash']=tools.det_hash(to_hash)
    block['root_hash']=tools.db_root()
    block=sign(block, privkey)
    block = tools.unpackage(tools.package(block))
    DB['suggested_blocks'].put(block)
    return block
def pass_(DB, args): return ' '
def error_(DB, args): return error
Do={'spend':spend, 'help':help_, 'blockcount':blockcount, 'txs':txs, 'balance':balance, 'my_balance':my_balance, 'b':my_balance, 'info':info, 'patty_info':patty_info, '':pass_, 'DB':DB_print, 'my_address':my_address, 'log':log, 'stop':stop_, 'commands':commands, 'pushtx':pushtx, 'peers':peers, 'buy_block':buy_block}
def main(DB, heart_queue):
    def responder(dic):
        command=dic['command']
        if command[0] in Do: 
            args=command[1:]
            try:
                out=Do[command[0]](DB, args)
            except Exception as exc:
                tools.log(exc)
                out='api main failure : ' +str(sys.exc_info())
        else: 
            out=str(command[0]) + ' is not a command. use "./cli.py commands" to get the list of commands. use "./cli.py help help" to learn about the help tool.'
        return out
    try:
        return networking.serve_forever(responder, custom.api_port, heart_queue)
    except Exception as exc:
        tools.log('api error')
        tools.log(exc)
