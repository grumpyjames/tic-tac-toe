import Data.List
import Test.QuickCheck
import Test.Hspec
import Control.Applicative
import Control.Monad

data Mark = Oh
          | Ex
          | Empty
          deriving (Show, Eq)

-- does the game have a result?
resultN :: [[Mark]] -> Bool
resultN rows = or[any result rows, any result (transpose rows)]

-- does a row contain three in a row?
result :: [Mark] -> Bool
result = checkCount . foldl foldFn (0, Empty)

foldFn :: (Int, Mark) -> Mark -> (Int, Mark)
foldFn win@(3, _) _ = win
foldFn _ Empty = (0, Empty) 
foldFn (count, a) b 
  | a == b = (count + 1, a)
  | otherwise = (1, b)           

checkCount :: (Int, Mark) -> Bool
checkCount (count, _) = count >= 3
                   
-- Properties, generators and some helpers

main = hspec $ do 
  describe "Calculations on a tic tac toe board" $ do 
    it "has no winner if there are no more than two marks each" $ property $ \xs -> 
      and [(count Oh xs) < 3, (count Ex xs) < 3] ==> result xs == False
    it "has a winner if there are three of the same marks consecutively" $ property $ \xs ->
      or [[Oh, Oh, Oh] `isInfixOf` xs, [Ex, Ex, Ex] `isInfixOf` xs]  ==> result xs == True
    it "has a winner if there are three consecutive marks in any row" $ property $ \ttt ->
      any three_in_a_row (unwrap ttt) ==> resultN (unwrap ttt) == True

prop_all_rows_start_with_x ttt = all (headEquals Ex) rows ==> resultN rows == True 
  where rows = unwrap ttt

count :: (Eq a) => a -> [a] -> Int
count x = length . filter ((==) x)

instance Arbitrary Mark where
  arbitrary = elements [Oh, Ex, Empty]

newtype TicTacToe = TicTacToe [[Mark]] deriving (Show, Eq)
instance Arbitrary TicTacToe where
  arbitrary = sized $ \s -> do
    rows <- vectorOf 3 (vectorOf 3 (elements [Oh, Ex, Empty]))
    return (TicTacToe rows)

unwrap :: TicTacToe -> [[Mark]]
unwrap (TicTacToe rows) = rows 

three_in_a_row xs = or [[Oh, Oh, Oh] `isInfixOf` xs, [Ex, Ex, Ex] `isInfixOf` xs]
                              
headEquals :: Mark -> [Mark] -> Bool                              
headEquals x (y:ys) = x == y
headEquals _ _ = False
