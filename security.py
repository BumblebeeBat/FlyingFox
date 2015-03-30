import math, numpy
from cdecimal import Decimal
def control_block_trial(total, clique, p):
    if numpy.random.poisson(clique)>p*total:
        return 1
    return 0
def control_block_probability(clique, total=Decimal(78), p=Decimal('0.66666666666666666666666666666666666666666666666666666')):
    #how often does the clique, which is a subset of the total, control > p of the block?
    a=Decimal(0)
    rounds=Decimal(500000)
    for i in range(rounds):
        a+=control_block_trial(total, clique, p)
    print("a: " +str(a))
    return(Decimal(a)/rounds)
def frequency_half_control(x): return Decimal(1)/control_block_probability(x/2, x)
def test():
    #We are aiming for a clique of 50% of the bonded validators to be able to verify 4% of the blocks. verification requres 2/3 of all signatures.
    j=54
    print(str(j)+" : " +str(frequency_half_control(Decimal(j))))
    print("this shows that if there are 54 validators on average, then an attacking group with 50% of bonded stake would control 1 block in 25.7 on average")
def proof_gap():
    #gap is given by numpy.random.gamma(1, 50)
    import random
    def f(): return random.random()*25>24
    def g(): return "1" if f() else "0"
    def h():
        out=""
        for i in range(1000):
            out+=g()
        out=out.split("1")
        #print(out)
        out=map(lambda x: len(x), out)
        return out
    #h() =doing=same= gamma()

    def gamma():
        out=[]
        for i in range(25):
            out+=[int(numpy.random.gamma(1, 50))]
        return out

def fork_time():#how long till fork dies? assuming 50% want the fork to live.
    #assuming 50 blocks is too big of a gap to jump.
    #this shows that that we should aim for a 4% probability that attackers can control the block. (1/25 = 0.04)
    #forks usually die within 200 blocks.
    def tg():#probability next gap is big enough to kill
        out=0
        times=1000
        for i in range(times):
            if numpy.random.gamma(1, 25)>50:
                out+=1
        return out*1.0/times
    def fg():#average gap size
        out=0
        times=1000
        for i in range(times):
            out+=numpy.random.gamma(1, 25)
        return out*1.0/times
    return fg()*(1.0/tg())#~180
if __name__=="__main__":
    print(test())
    #print(gamma())
