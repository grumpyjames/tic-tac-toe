import Data.List
import Test.QuickCheck

-- does the game have a result?
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

resultN :: [[Mark]] -> Bool
resultN rows = or[any result rows, any result (transpose rows)]

data Mark = Oh
          | Ex
          | Empty
          deriving (Show, Eq)
                   
-- Properties, generators and some helpers

instance Arbitrary Mark where
  arbitrary = elements [Oh, Ex, Empty]

newtype TicTacToe = TicTacToe [[Mark]] deriving (Show, Eq)
instance Arbitrary TicTacToe where
  arbitrary = sized $ \s -> do
    rows <- vectorOf 3 (vectorOf 3 (elements [Oh, Ex, Empty]))
    return (TicTacToe rows)

unwrap :: TicTacToe -> [[Mark]]
unwrap (TicTacToe rows) = rows 

prop_only_empty_elements_is_no_result xs = and [not $ elem Oh xs, not $ elem Ex xs] ==> result xs == False

prop_three_in_a_row_has_a_result xs = or [[Oh, Oh, Oh] `isInfixOf` xs, [Ex, Ex, Ex] `isInfixOf` xs]  ==> result xs == True

prop_three_in_any_row ttt = any (three_in_a_row) rows ==> resultN rows == True
  where rows = unwrap ttt
prop_all_rows_start_with_x ttt = all (headEquals Ex) rows ==> resultN rows == True 
  where rows = unwrap ttt

n_by_n :: Int -> [[a]] -> Bool
n_by_n desired_n rows = and[length rows == desired_n, all (lenEqual desired_n) rows]

three_in_a_row xs = or [[Oh, Oh, Oh] `isInfixOf` xs, [Ex, Ex, Ex] `isInfixOf` xs]

lenEqual :: Int -> [a] -> Bool
lenEqual desired_len row = length row == desired_len
                              
headEquals :: Mark -> [Mark] -> Bool                              
headEquals x (y:ys) = x == y
headEquals _ _ = False
