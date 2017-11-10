# pollard p-1 factoring method

def ilog(b, n): # max e where b**e <= n
    lo, blo, hi, bhi = 0, 1, 1, b
    while bhi < n:
        lo, blo, hi, bhi = hi, bhi, hi+hi, bhi*bhi
    while 1 < (hi - lo):
        mid = (lo + hi) // 2
        bmid = blo * pow(b, (mid - lo))
        if n < bmid: hi, bhi = mid, bmid
        elif bmid < n: lo, blo = mid, bmid
        else: return mid
    if bhi == n: return hi
    return lo

def gcd(a,b): # euclid's algorithm
    if b == 0: return a
    return gcd(b, a%b)

def primegen(start=0): # stackoverflow.com/a/20660551
    if start <= 2: yield 2    # prime (!) the pump
    if start <= 3: yield 3    # prime (!) the pump
    ps = primegen()           # sieving primes
    p = next(ps) and next(ps) # first sieving prime
    q = p * p; D = {}         # initialization
    def add(m, s):            # insert multiple/stride
        while m in D: m += s  #   find unused multiple
        D[m] = s              #   save multiple/stride
    while q <= start:         # initialize multiples
        x = (start // p) * p  #   first multiple of p
        if x < start: x += p  #   must be >= start
        if x % 2 == 0: x += p #   ... and must be odd
        add(x, p+p)           #   insert in sieve
        p = next(ps)          #   next sieving prime
        q = p * p             #   ... and its square
    c = max(start-2, 3)       # first prime candidate
    if c % 2 == 0: c += 1     # must be odd
    while True:               # generate infinite list
        c += 2                #   next odd candidate
        if c in D:            #   c is composite
            s = D.pop(c)      #     fetch stride
            add(c+s, s)       #     add next multiple
        elif c < q: yield c   #   c is prime; yield it
        else: # (c == q)      #   add p to sieve
            add(c+p+p, p+p)   #     insert in sieve
            p = next(ps)      #     next sieving prime
            q = p * p         #     ... and its square

def pminus1(n, b, x=2):
    q = 0; pgen = primegen(); p = next(pgen)
    while p < b:
        x = pow(x, p**ilog(p,b), n)
        q, p = p, next(pgen)
    g = gcd(x-1, n)
    if 1 < g < n: return g
    return False

print pminus1(10001, 10)

print pminus1(834126253476422235279396896780306823377996256349,125000)# your code goes here