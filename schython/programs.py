def fib(n):
    if n == 0 or n == 1 : # fib(0) = 0; fib(1) = 1
      return n
    else:
      return fib(n-1) + fib(n-2) # fib(n) = fib(n-1) + fib(n-2)

def fastFib(x, memo):
    assert type(x) == int and x >= 0 and type(memo) == dict
    if x == 0 or x == 1:
        return x
    if x in memo:
        return memo[x]
    result = fastFib(x-1, memo) + fastFib(x-2, memo)
    memo[x] = result
    return result


def make_fib_memos():
    fib_memos = {}
    def fib(x):
      if x in fib_memos:     #fibonacci(x) has already been computed, return this value
        return fib_memos[x]
      elif x <= 1:
        return x
      else:
        t = fib(x-1) + fib(x-2)
        fib_memos[x] = t
        return t
    return fib

# print fib(10)
# print fastFib(10, {})

# make_fib_memos()(10)

# memo_fib = make_fib_memos()
# print memo_fib(10) #prints 55
# print memo_fib(50) #prints 12586269025
# print memo_fib(100) #prints 354224848179261915075
# print make_fib_memos


def make_fact_memos():
    fact_memos = {}
    def fact(x):
        if x in fact_memos:    # we've already computed fact(x)
            return fact_memos[x]
        elif x <= 1:
            return 1            # fact(0) = fact(1) = 1
        else:
            t = x * fact(x-1)
            fact_memos[x] = t
            return t
    return fact

memo_fact = make_fact_memos()
print memo_fact(0) #prints 1
print memo_fact(2) #prints 2
print memo_fact(20) #prints 2432902008176640000