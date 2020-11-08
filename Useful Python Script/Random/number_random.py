import random

def number(x):
    ''' Give a random number from a range of 0 to x
        x is the number given in the function'''
    y = random.randint(0,x)
    return y


print(number(10), number(10), number(10), sep=",")


