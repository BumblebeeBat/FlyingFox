from multiprocessing import Process
import os
import json
import leveldb
def default_entry(): return dict(count=0, amount=0, secrets={}, entropy={})
def main(heart_queue, database_name, logf, database_port):
    import networking
    import sys
    import patricia as patty
    DB=leveldb.LevelDB(database_name+'_local')
    def local_get(args): 
        try:
            a=json.loads(DB.Get(args[0]))
            return a
        except Exception as exc:
            logf(exc)
            logf('local get: '+str(args))
            return 'empty'
    def local_put(args): return DB.Put(args[0], json.dumps(args[1]))
    def get(args):
        try:
            return json.loads(patty.get(str(args[0])))
        except:# KeyError:
            return default_entry()
    def put(args): return patty.put(str(args[0]), json.dumps(args[1]))
    def existence(args):
        try:
            patty.get(str(args[0]))
        except:# KeyError:
            return False
        else:
            return True
    def delete(args):
        try:
            patty.delete(str(args[0]))
        except:#we should make sure this is the type of error we are expecting.
            pass
    def proof(args): return patty.prove(args[0])
    def verify(args):#root, key, proof
        try:
            return json.loads(json.loads(patty.verify(args[0], args[1], args[2])))
        except:
            return False
    def root(args): return patty.root()
    do={'get':get, 'put':put, 'existence':existence, 'delete':delete, 'proof':proof, 'verify':verify, 'root':root, 'local_get':local_get, 'local_put':local_put}
    def command_handler(command):
        try:
            name = command['type']
            if name not in do.keys(): 
                logf('name: ' +str(name))
                error()
            return do[name](command['args'])
        except Exception as exc:
            logf(exc)
            logf('command: ' + str(command))
            logf('command type: ' + str(type(command)))
            return {'error':'bad data'}
    networking.serve_forever(command_handler, database_port, heart_queue)
