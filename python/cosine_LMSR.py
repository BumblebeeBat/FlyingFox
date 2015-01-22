# v is used for fourier series. f(x) is used for functions between 0 and 1.
#http://en.wikipedia.org/wiki/Discrete_cosine_transform
from cdecimal import Decimal
zero=Decimal('0')
one=Decimal('1')
pi=Decimal('3.141592653589793')
E=Decimal('2.718281828459045')
def graph(f, m=100): 
    for i in range(m):
        print(f(Decimal(i)/m))
def dec_greater_than(a, b): return float(a)>float(b)
def factorial(n): 
    if not dec_greater_than(n, 1): return one
    return n*factorial(n-1)
def alternate_sum(v, positive=1, s=0): 
    if len(v)==0: return s
    return alternate_sum(v[1:], -positive, s+v[0]*positive)
Taylor_series_depth=13
def trig(x, t):
    if not dec_greater_than(x, Decimal('0.0000001')) and dec_greater_than(x, Decimal('-0.0000001')): return 1-t
    if not dec_greater_than(x, -pi): return trig(x+2*pi, t)
    if dec_greater_than(x, 10*pi): return trig(x-11*pi, t)
    if dec_greater_than(x, pi): return trig(x-2*pi, t)
    a=filter(lambda y: y%2==t, range(Taylor_series_depth))
    a=map(lambda y: (x**y)/factorial(y), a)
    return alternate_sum(a)
def cos(x): return trig(x, 0)
def sin(x): return trig(x, 1)
def mul(a, b): return a*b
def add(a, b): return a+b
def plug_in(x, v):#use the cosine series to draw a graph, and see what the value of x is on that graph.
    #wikipedia: DCT III
    c=map(lambda y: 2*cos((y+(one/2))*pi*x), range(len(v)))
    #c[0]=c[0]/2
    return sum(map(mul, v, c))
def v2f(v): return lambda x: plug_in(x, v)
def integrate(f, m=5): return sum(map(lambda x: f(Decimal(x)/m), range(m)))/m#this m determines initial liquidity=B*ln(m)
def C(v, B): return B*integrate(lambda x: E**(v2f(v)(x)/B)).ln()
def cost_to_buy_shares(current_state, shares, B):
    f=lambda x: C(x, B)
    return f(map(add, current_state, shares))-f(current_state)
def DCT(f, s=5):
    #wikipedia: DCT II
    def g(f, n): return (lambda x: f(x)*cos(pi*(n+(one/2))*x))
    return map(lambda n: integrate(g(f, n), s), range(s))

#print(cost_to_buy_shares([Decimal('10'),Decimal('50'),0,0,0], [Decimal('10'),0,0,0,0], 1000))
#print(cost_to_buy_shares([Decimal('10'),Decimal('50'),0,0,0], [Decimal('5'),0,0,0,0], 1000))
#print(cost_to_buy_shares([Decimal('10'),Decimal('50'),0,0,0], [Decimal('5'),Decimal('5'),0,0,0], 1000))
