import Data.List
import System.IO

-- Assuming loading in ghci

--  --- MATH ---
minInt :: Int
minInt = minBound :: Int

maxInt :: Int
maxInt = maxBound :: Int

sumOfNums = sum [0 .. 1000]

addEx = 3 + 4

subEx = 3 - 4

mulEx = 3 * 4

divEx = 12 / 3

modEx = mod 5 4

modEx2 = 5 `mod` 4

negNumEx = 5 + (-4)

num9 = 9 :: Int

sqrtOf9 = sqrt $ fromIntegral num9

piVal = pi

ePow1 = exp 1

logOf9 = log 9

square9 = 9 ** 2

truncatedVal = truncate 9.999

roundVal = round 9.999

ceilVal = ceiling 9.999

floorVal = floor 9.999

trueAndFalse = False

trueOrFalse = True

notTrue = False

-- --- LISTS ---

primeNums = [3, 5, 7]

morePrimes = primeNums ++ [11, 13, 17]

numnumnum = [3, 2, 5, 15]

nestList = [[1, 2, 3], [3, 2, 1]]

morePrimes2 = 2 : morePrimes

firstPrime = head morePrimes2

lastPrime = last morePrimes2

primeTail = tail morePrimes2

primeInit = init morePrimes2

sndPrime = morePrimes2 !! 1

isListEmpty = null morePrimes2

first3Primes = take 3 morePrimes2

remainedPrimes = drop 3 morePrimes2

is7InList = 7 `elem` morePrimes2

maxPrime = maximum morePrimes2

minPrime = minimum morePrimes2

sumPrimes = sum morePrimes2

newList = [2, 3, 4]

prodPrimes = product newList

zeroToTen = [0 .. 10]

evenList = [0, 2 .. 10]

oddList = [1, 3 .. 11]

letterList = ['A', 'C' .. 'Z']

infinPow10 = take 10 [10, 20 ..]

many2s = replicate 10 2

cycleList = take 10 (cycle [1, 2, 3, 4, 5])

listTimes2 = [x * 2 | x <- [1 .. 10]]

listTimes3 = [x * 3 | x <- [1 .. 20], x * 3 <= 50]

divsBy9n13 = [x | x <- [1 .. 500], x `mod` 13 == 0, x `mod` 9 == 0]

sorted = sort [123, 3, 114, 5, 23, 19]

sumOfLists = zipWith (+) [1, 2, 3, 4, 5] [6, 7, 8, 9, 10]

sumOfLists2 = zipWith (+) [1, 2, 3] [6, 7, 8, 9, 10]

listBiggerThan10 = filter (> 10) sumOfLists

evensUpTo20 = takeWhile (<= 20) [2, 4 ..]

multOfList = foldl (*) 2 [2, 3, 4, 5]

pow3List = [3 ^ n | n <- [1 .. 10]]

pow3ListDiv9 = [3 ^ n | n <- [1 .. 10], 3 ^ n `mod` 9 == 0]

multTable = [[x * y | y <- [1..3]] | x <- [1..3]]