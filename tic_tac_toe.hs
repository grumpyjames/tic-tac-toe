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

data Mark = Oh
          | Ex
          | Empty
          deriving (Show, Eq)

instance Arbitrary Mark where
  arbitrary = elements [Oh, Ex, Empty]

prop_only_empty_elements_is_no_result xs = and [not $ elem Oh xs, not $ elem Ex xs] ==> result xs == False

three_in_a_row xs = or [[Oh, Oh, Oh] `isInfixOf` xs, [Ex, Ex, Ex] `isInfixOf` xs]

prop_three_in_a_row_has_a_result xs = or [[Oh, Oh, Oh] `isInfixOf` xs, [Ex, Ex, Ex] `isInfixOf` xs]  ==> result xs == True

prop_two_rows x1s x2s = any (three_in_a_row) [x1s, x2s] ==> result2 x1s x2s == True
                        where result2 rowOne rowTwo = or [result rowOne, result rowTwo]