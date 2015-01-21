import copy
import tools
do={'swap':(lambda l: [l[1]]+[l[0]]+l[2:]), 
    'drop':(lambda l: l[1:]),
    '*':(lambda l: [l[0]*l[1]]+l[2:]),
    '/':(lambda l: [l[0]/l[1]]+l[2:]),
    '-':(lambda l: [l[0]-l[1]]+l[2:]),
    '+':(lambda l: [l[0]+l[1]]+l[2:])}
def forth(f, stack): 
    if len(f)==0: return stack
    try:
        f[0]=float(f[0])
        return forth(f[1:], [f[0]]+stack)
    except Exception as exc:
        tools.log(exc)
        return forth(f[1:], do[f[0]](stack))
def apply_script(f, stack):#this function is a tiny forth-like language.
    return forth(f.split(' '), copy.deepcopy(stack))
def share_value_helper(stack, algorithm, functions, state):
    ratio=apply_script(functions[algorithm[0]], stack)[0]
    if state in algorithm[1]: 
        if len(algorithm[1])==1:
            return ratio
        n=2
    else: 
        ratio=1-ratio
        if len(algorithm)==2:
            return ratio
        n=3
    return ratio*share_value_helper(stack, algorithm[n], functions, state)
def share_value(pm, state):
    return share_value_helper(pm['results'], pm['algorithm'], pm['functions'], state)
def test():
    example_pm={'results':[1,0], 'predictions':['obama wins election', 'pinto beans above 80 cents'], 'functions':['swap drop', 'drop'], 'output states':['a', 'b', 'c', 'd'], 'algorithm':[0, ['a', 'b'], [1, ['a']], [1, ['c']]]}
    print(share_value(example_pm, 'a'))
    print(share_value(example_pm, 'b'))
    print(share_value(example_pm, 'c'))
    print(share_value(example_pm, 'd'))
    for p in [0.2, 0.5, 0.8]:
        print("-----------------------")
        print('at price: ' +str(p))
        example_pm={'results':[p], 'predictions':['price of BTC in USD within 200-900'], 'functions':['700 * 200 + 1 / 200 *'], 'output states':['stable-coin', 'vol-coin'], 'algorithm':[0, ['stable-coin']]}
        print('vol')
        print(share_value(example_pm, 'vol-coin'))
        print('stable')
        print(share_value(example_pm, 'stable-coin'))
        print('value in USD' +str(share_value(example_pm, 'stable-coin')*(p*700+200)))
if __name__=='__main__': #for windows
    test()
