module Main where

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

multTable = [[x * y | y <- [1 .. 3]] | x <- [1 .. 3]]

-- --- TUPLES ---

aTuple = ('a', 42, "asdfasdf")

bob = ("Bob", 44)

bobName = fst bob

bobAge = snd bob

names = ["Bob", "Hoge", "Fuga"]

addrs = ["123-123", "312-321", "423-234"]

nameNAddrs = zip names addrs

(_, hogeAddr) = nameNAddrs !! 1

-- --- FUNCTIONS ---

main :: IO ()
main = do
  putStrLn "Type your name:"

  name <- getLine

  putStrLn $ "Hello" <> name

addMe :: Num a => a -> a -> a
addMe x y = x + y

addTuples :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addTuples (x, y) (x2, y2) = (x + x2, y + y2)

inKanji :: Int -> String
inKanji 1 = "一"
inKanji 2 = "二"
inKanji 3 = "三"
inKanji x = "他"

print3InKanji :: IO ()
print3InKanji = putStrLn $ inKanji 3

fact 0 = 1
fact n = n * fact (n - 1)

fact2 n = product [1 .. n]

isHoge :: String -> Bool
isHoge str
  | str == "Hoge" = True
  | otherwise = False

fzbz :: Int -> String
fzbz n
  | n `mod` 3 == 0 && n `mod` 5 == 0 = "FzBz"
  | n `mod` 3 == 0 = "Fz"
  | n `mod` 5 == 0 = "Bz"
  | otherwise = show n

scoreAvgRating :: Double -> Double -> String
scoreAvgRating counts shoots
  | avg <= 0.5 = "meh"
  | avg <= 0.6 = "ok"
  | avg <= 0.7 = "great"
  | otherwise = "godlike"
  where
    avg = counts / shoots

getListItems :: [Int] -> String
getListItems [] = "empty"
getListItems [x] = show x
getListItems (x : xs) = getListItems [x] <> " and " <> getListItems xs

getFirstItem :: String -> String
getFirstItem [] = "empty"
getFirstItem inputs@(x : xs) = inputs <> "'s first letter is " <> [x]

double :: Int -> Int
double x = x * 2

listDouble :: [Int]
listDouble = map double [1 .. 10]

-- re-invent map
mulBy2 :: [Int] -> [Int]
mulBy2 xs = map double xs

areStrEq :: String -> String -> Bool
areStrEq [] [] = True
areStrEq (x : xs) (y : ys) = x == y && areStrEq xs ys
areStrEq _ _ = False

aplNum3 :: (Int -> Int) -> Int
aplNum3 fun = fun 3

num3Double = aplNum3 double

-- --- LAMBDA ---
dblList = map (* 2) [0 .. 10]

dblEven x =
  if even x
    then x * 2
    else x

getCls :: Int -> String
getCls age = case age of
  5 -> "kindergarten"
  6 -> "elem school"
  _ -> "some place"

-- --- Algebraic Data Types ---

data Block
  = DirtBlock
  | StoneBlock
  | WoodBlock
  deriving (Show)

showBlock :: Block -> String
showBlock b = "BlockName: " <> show b

data Customer = Customer String String Int
  deriving (Show)

tomSmith :: Customer
tomSmith = Customer "tom smith" "123 main" 123

getAge :: Customer -> Int
getAge (Customer _ _ age) = age

data Shape = Circle Double | Rectangle Double Double
  deriving (Show)

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Rectangle x y) = x * y

-- --- TypeClass ---

data Employee = Employee
  { name :: String,
    position :: String,
    idNum :: Int
  }
  deriving (Eq, Show)

samSmith = Employee {name = "Sam Smith", position = "mngr", idNum = 1}
pamSmith = Employee {name = "Pam Smith", position = "mngr", idNum = 2}

isSamPam = samSmith == pamSmith

data ShirtSize = S | M | L

instance Eq ShirtSize where
  S == S = True 
  M == M = True 
  L == L = True
  _ == _ = False 

instance Show ShirtSize where
  show S = "Small"
  show M = "Medium"
  show L = "Large"

showSizeS = show S

class MyEq a where
  areEqual :: a -> a -> Bool 

instance MyEq ShirtSize where
  areEqual S S = True 
  areEqual M M = True 
  areEqual L L = True
  areEqual _ _ = False 

newSize = areEqual S S

sayHello = do
  putStrLn "what ur name"
  name <- getLine
  putStrLn  $ "Hello" <> name

writeToFile = do
  theFile <- openFile "test.txt" WriteMode
  hPutStrLn theFile "Blahdsflaksdf"
  hClose theFile

readFromFile = do
  theFile2 <- openFile "test.txt" ReadMode
  contents <- hGetContents theFile2
  putStr contents
  hClose theFile2