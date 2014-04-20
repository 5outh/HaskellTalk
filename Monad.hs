import Data.Monoid
import Data.List(nub)

{- Maybe -}

safeCalculation :: [Int] -> [a] -> Maybe a
safeCalculation xs ys = do
  x <- safeHead xs
  ys !? x

{- [a] -}

type Point = (Int, Int)

neighbors :: Point -> [Point]
neighbors (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

allNeighbors :: [Point] -> [Point]
allNeighbors ps = ps >>= neighbors

neighborsOfNeighbors :: Point -> [Point]
neighborsOfNeighbors p = do
  x <- neighbors p 
  neighbors x

{- Writer -}

newtype Writer w a = Writer{ runWriter :: (a, w) } deriving (Show, Eq)

instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  (Writer (a, w)) >>= f = Writer (b, w `mappend` w')
    where (b, w') = runWriter $ f a

tellFactorial :: Int -> Writer [String] Int
tellFactorial 0 = Writer (1, ["Got to 0! Returning 1."])
tellFactorial n = tellFactorial (n-1) >>= 
  \m -> Writer (n*m, ["Calculated " ++ show n ++ "! " ++ "and got " ++ show (n*m)])

tell :: w -> Writer w ()
tell w = Writer ((), w)

tellFactorial' :: Int -> Writer [String] Int
tellFactorial' 0 = do
  tell ["Got to 0! Returning 1"]
  return 1
tellFactorial' n = do
  m <- tellFactorial' (n-1)
  tell ["Calculated " ++ show n ++ "! " ++ "and got " ++ show (n*m)]
  return (n*m)
  
-- EXTRAS

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

(!?) :: [a] -> Int -> Maybe a
xs !? idx
  | idx < length xs = Just $ head $ drop idx xs
  | otherwise       = Nothing