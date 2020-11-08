import random, string

def random_char(y):
     '''Insert how many random letters.'''
    return ''.join(random.choice(string.ascii_letters) for x in range(y))

print (random_char(5))

