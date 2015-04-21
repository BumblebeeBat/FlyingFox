"""This program starts all the threads going. When it hears a kill signal, it kills all the threads.
"""
import peer_recieve, time, threading, tools, custom, networking, sys, api, blockchain, peers_check, multiprocessing, db, threads, copy, auto_signer, reward_collector
#windows was complaining about lambda
def peer_recieve_func(d, DB=custom.DB):
    return peer_recieve.main(d, DB)

def main(brainwallet, pubkey_flag=False):
    DB=custom.DB
    tools.log('custom.current_loc: ' +str(custom.current_loc))
    print('starting full node')
    if not pubkey_flag:
        privkey=tools.det_hash(brainwallet)
        pubkey=tools.privtopub(privkey)
    else:
        pubkey=brainwallet
    a=tools.empty_peer()
    b=custom.peers
    #b[tools.getPublicIp()+':'+str(custom.port)]=a
    processes= [
        {'target': db.main,
         'args': (DB['heart_queue'], custom.database_name, tools.log, custom.database_port),
         'name': 'db'},
        {'target': auto_signer.mainloop,
         'args': (),
         'name': 'auto_signer'},        
        {'target': reward_collector.doit,
         'args': (),
         'name': 'auto_signer'},        
        #{'target':tools.heart_monitor,
        # 'args':(DB['heart_queue'], ),
        # 'name':'heart_monitor'},
        {'target': blockchain.main,
         'args': (DB,),
         'name': 'blockchain'},
        {'target': api.main,
         'args': (DB, DB['heart_queue']),
         'name': 'api'},
        {'target': peers_check.main,
         'args': (b, DB),
         'name': 'peers_check'},
        {'target': networking.serve_forever,
         'args': (peer_recieve_func, custom.port, DB['heart_queue'], True),
         'name': 'peer_recieve'}
    ]
    cmds=[]
    cmd=multiprocessing.Process(**processes[0])
    cmd.start()
    cmds.append(cmd)
    tools.log('starting '+cmd.name)
    time.sleep(4)
    b=tools.db_existence(0)
    #def empty_memoized(): return({'blockcount':-3000})
    if not b:
        tools.local_put('length', -1)
        tools.local_put('height', -1)
        tools.local_put('memoized_votes', {})
        tools.local_put('txs', [])
        tools.local_put('peers', {})
        tools.local_put('targets', {})
        tools.local_put('times', {})
        tools.local_put('mine', False)
        tools.local_put('my_sign_txs', {})#empty_memoized())
        tools.local_put('secrets', {})
        money=db.default_entry()
        money['amount']+=custom.all_money
        tools.db_put(custom.creator, money)
    tools.local_put('stop', False)
    tools.log('stop: ' +str(tools.local_get('stop')))
    for process in processes[1:]:
        cmd=multiprocessing.Process(**process)
        cmd.start()
        cmds.append(cmd)
        tools.log('starting '+cmd.name)
    if not pubkey_flag:
        tools.local_put('privkey', privkey)
    else:
        tools.local_put('privkey', 'Default')
    Address=tools.make_address([pubkey], 1)
    tools.local_put('address', Address)
    a=tools.db_proof(Address)
    tools.local_put('balance_proofs-1', a)
    tools.log('stop: ' +str(tools.local_get('stop')))
    while not tools.local_get('stop'):
        time.sleep(0.5)
    tools.log('about to stop threads')
    DB['heart_queue'].put('stop')
    for p in [[custom.port, '127.0.0.1'],
              [custom.api_port, '127.0.0.1']]:
        networking.connect('stop', p[0], p[1])
    cmds.reverse()
    for cmd in cmds[:-1]:
        cmd.join()
        tools.log('stopped a thread: '+str(cmd))
    time.sleep(2)
    networking.connect('stop', custom.database_port, '127.0.0.1')
    cmds[-1].join()
    tools.log('stopped a thread: '+str(cmds[-1]))
    tools.log('all threads stopped')
    sys.exit(0)
if __name__=='__main__': #for windows
    try:
        main(sys.argv[1])
    except Exception as exc:
        tools.log(exc)
