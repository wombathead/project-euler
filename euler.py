import math

def problem1(n):
    """ Sum of multiples of 3 and/or 5 less than n """
    sum = 0
    for i in range(1, n):
        if i % 3 == 0 or i % 5 == 0:
            sum += i
    return sum

def problem2(n):
    """ Sum of even Fibonacci numbers that do not exceed n """
    a, b = 1, 2
    sum = 2
    
    while a + b <= n:
        c = a + b
        if c % 2 == 0:
            sum += c
        a, b = b, c

    return sum

def problem3(n):
    """ Largest prime factor of n """
    root = math.floor(math.sqrt(n))
    
    factors = list()
    for i in range(2, root + 1):
        if n % i == 0:
            factors.append(i)
            if n // i != i:
                factors.append(n // i)
    return factors

    max = 0
    for f in factors:
        if f % 2 == 0:
            continue

        r = math.floor(math.sqrt(f))
        is_prime = True
        for i in range(3, r + 1):
            if f % i == 0:
                is_prime = False
                break

        if is_prime and f > max:
            max = f

    return max

def problem4():
    """ Largest palindrome made from the product of two 3-digit numbers """

    def isPalindrome(n):
        n_str = str(n)
        digits = len(n_str)

        for i in range(0, digits):
            if n_str[i] != n_str[digits - 1 - i]:
                return False

        return True

    max = 0
    for i in range(100, 1000):
        for j in range(100, 1000):
            product = i * j
            if isPalindrome(product) and product > max:
                max = product

    return max

def problem5(n):
    """ Smallest number evenly divisible by all numbers 1-n """
    a = 1
    factors = []

    for i in range(1, n+1):
        for f in factors:
            if i % f == 0:
                i = i // f
                print(i)
        factors.append(i)
        a *= i

    return a

def problem6(n):
    """ Difference between sum of squares in 1..n and square of their sum """
    squared_sum = (n * (n + 1) // 2) ** 2
    sum_squares = (n * (n + 1) * (2 * n + 1)) // 6

    return squared_sum - sum_squares

def problem7(n):
    """ Get the nth prime """
    primes = [2]
    count = 1

    i = 3
    while count < n:
        is_prime = True

        root = math.floor(math.sqrt(i))
        for p in primes:
            if i % p == 0:
                is_prime = False
                break
        
        if is_prime:
            primes.append(i)
            count += 1

        i += 2

    return primes[n-1]

def problem8(places):
    """ Largest product of 13 adjacent digits in the number """
    n = "73167176531330624919225119674426574742355349194934\
            96983520312774506326239578318016984801869478851843\
            85861560789112949495459501737958331952853208805511\
            12540698747158523863050715693290963295227443043557\
            66896648950445244523161731856403098711121722383113\
            62229893423380308135336276614282806444486645238749\
            30358907296290491560440772390713810515859307960866\
            70172427121883998797908792274921901699720888093776\
            65727333001053367881220235421809751254540594752243\
            52584907711670556013604839586446706324415722155397\
            53697817977846174064955149290862569321978468622482\
            83972241375657056057490261407972968652414535100474\
            82166370484403199890008895243450658541227588666881\
            16427171479924442928230863465674813919123162824586\
            17866458359124566529476545682848912883142607690042\
            24219022671055626321111109370544217506941658960408\
            07198403850962455444362981230987879927244284909188\
            84580156166097919133875499200524063689912560717606\
            05886116467109405077541002256983155200055935729725\
            71636269561882670428252483600823257530420752963450"
    n = n.replace(" ", "")
    length = len(n)

    max = 0
    for i in range(0, length - places + 1):
        adj = n[i:i+places]

        product = 1
        for d in adj:
            product *= int(d)
            
            if product > max:
                max = product

    return max

def problem9():
    """ Pythagorean triple such that a + b + c = 1000, return a*b*c """

    for a in range(1, 500):
        b = (1000 * a - 5e5) // (a - 1000)
        c = 1000 - a - b

        if a**2 + b**2 == c**2:
            return a*b*c

def problem10(n):
    """ Sum of all primes below n (2e6)"""

    nums = [True] * (n+1)
    nums[0] = nums[1] = False

    nums[2] = True
    for i in range(2, n // 2 + 1):
        nums[2 * i] = False

    for i in range(3, n, 2):

        iterations = n // i
        for j in range(i, iterations + 1):
            nums[i * j] = False

    primes = [i for i in range(len(nums)) if nums[i] == True]
    return sum(primes)

def problem11():
    """ Largest product for 4 numbers in a line """
    nums = [
        [ 8,  2, 22, 97, 38, 15,  0, 40,  0, 75,  4,  5,  7, 78, 52, 12, 50, 77, 91,  8],
        [49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48,  4, 56, 62,  0],
        [81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30,  3, 49, 13, 36, 65],
        [52, 70, 95, 23,  4, 60, 11, 42, 69, 24, 68, 56,  1, 32, 56, 71, 37,  2, 36, 91],
        [22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80],
        [24, 47, 32, 60, 99,  3, 45,  2, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50],
        [32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70],
        [67, 26, 20, 68,  2, 62, 12, 20, 95, 63, 94, 39, 63,  8, 40, 91, 66, 49, 94, 21],
        [24, 55, 58,  5, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72],
        [21, 36, 23,  9, 75,  0, 76, 44, 20, 45, 35, 14,  0, 61, 33, 97, 34, 31, 33, 95],
        [78, 17, 53, 28, 22, 75, 31, 67, 15, 94,  3, 80,  4, 62, 16, 14,  9, 53, 56, 92],
        [16, 39,  5, 42, 96, 35, 31, 47, 55, 58, 88, 24,  0, 17, 54, 24, 36, 29, 85, 57],
        [86, 56,  0, 48, 35, 71, 89,  7,  5, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58],
        [19, 80, 81, 68,  5, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77,  4, 89, 55, 40],
        [ 4, 52,  8, 83, 97, 35, 99, 16,  7, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66],
        [88, 36, 68, 87, 57, 62, 20, 72,  3, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69],
        [ 4, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18,  8, 46, 29, 32, 40, 62, 76, 36],
        [20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74,  4, 36, 16],
        [20, 73, 35, 29, 78, 31, 90,  1, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57,  5, 54],
        [ 1, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52,  1, 89, 19, 67, 48],
    ]

    max = 0
    # vertical lines
    for i in range(0, len(nums[0])):
        for j in range(0, len(nums) - 4):
            prod = nums[j][i] * nums[j+1][i] * nums[j+2][i] * nums[j+3][i]
            print(prod)
            if prod > max:
                max = prod

def main():
    import time

    start = time.time()
    print(problem4())
    end = time.time()
    print("Took "+str(end - start)+"s")

if __name__=="__main__":
    main()
