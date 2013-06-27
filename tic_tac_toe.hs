import Data.List
import Test.QuickCheck

-- does the game have a result?
result :: [Mark] -> Bool
result = checkCount . foldl foldFn (0, Empty)

foldFn :: (Int, Mark) -> Mark -> (Int, Mark)
foldFn win@(3, _) _ = win
foldFn _ Empty = (0, Empty) 
foldFn current@(count, mark) currentMark = if mark == currentMark
                                           then (count + 1, mark)
                                           else (1, currentMark)
                                               
checkCount :: (Int, Mark) -> Bool
checkCount (count, mark) = and[count >= 3, not $ mark == Empty]

resultN :: [GameRow] -> Bool
resultN gameRows = or[any result rows, any result (transpose rows)]
  where rows = unwrap gameRows

unwrap :: [GameRow] -> [[Mark]]
unwrap ((GameRow g):grs) = g:(unwrap grs) 
unwrap [] = []

data Mark = Oh
          | Ex
          | Empty
          deriving (Show, Eq)

instance Arbitrary Mark where
  arbitrary = elements [Oh, Ex, Empty]
  
newtype GameRow = GameRow [Mark] deriving (Show, Eq)
instance Arbitrary GameRow where
  arbitrary = sized $ \s -> do
    xs <- vectorOf 3 (elements [Oh, Ex, Empty])
    return (GameRow xs)

prop_only_empty_elements_is_no_result xs = and [not $ elem Oh xs, not $ elem Ex xs] ==> result xs == False

prop_three_in_a_row_has_a_result xs = or [[Oh, Oh, Oh] `isInfixOf` xs, [Ex, Ex, Ex] `isInfixOf` xs]  ==> result xs == True

prop_n_rows rows = and[any (three_in_a_row) rows, length rows == 3] ==> resultN rows == True

prop_all_rows_start_with_x rows = and[all (headEquals Ex) rows, length rows == 3] ==> resultN rows == True 

n_by_n :: Int -> [[a]] -> Bool
n_by_n desired_n rows = and[length rows == desired_n, all (lenEqual desired_n) rows]

three_in_a_row (GameRow xs) = or [[Oh, Oh, Oh] `isInfixOf` xs, [Ex, Ex, Ex] `isInfixOf` xs]

lenEqual :: Int -> [a] -> Bool
lenEqual desired_len row = length row == desired_len
                              
headEquals :: Mark -> GameRow -> Bool                              
headEquals x (GameRow (y:ys)) = x == y
headEquals _ _ = False
