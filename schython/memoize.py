# def __fib(x, memo):
#     if x <= 1:
#       return x
#     else:
#       return memo(x-1) + memo(x-2)

# def __factorial(x, memo):
#     if x <= 1:
#       return 1
#     else:
#       return x * memo(x-1)

def memoize(func):
    d = {}
    def memo(num):
        if num in d:
            return d[num]
        else:
            v = func(num, memo)
            d[num] = v
        return v
    return memo



# factorial = memoize(__factorial)
# fib_memo = memoize(__fib)
# print fib_memo(100)
# print factorial(100)