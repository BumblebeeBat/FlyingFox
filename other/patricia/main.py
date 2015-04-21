import trie, rlp
from json import dumps as package, loads as unpackage
state=trie.Trie('db', trie.BLANK_ROOT)
def get(key): return unpackage(rlp.decode(state.get(key)))
def put(key, val): return state.update(key, rlp.encode(package(val)))
def delete(key):  return state.delete(key)
def root():  return state.root_hash.encode('hex')
def prove(key): 
    p=state.produce_spv_proof(key)
    p=rlp.encode(p).encode('base64')
    #p=package(p)
    return p
def verify(root, key, proof): 
    return trie.verify_spv_proof(root.decode('hex'), key, rlp.decode(proof.decode('base64')))

def test():
    put('a', 123)
    print(get('a'))
    a=prove('a')
    print('proof a: ' +str(a))
    print('verify: ' +str(verify(root(), 'a', a)))

