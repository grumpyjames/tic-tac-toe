import Data.List
import Test.QuickCheck

-- does the game have a result?
result :: [Mark] -> Bool
result = checkCount . foldl foo (0, Empty)

foo :: (Int, Mark) -> Mark -> (Int, Mark)
foo win@(count, _) Empty = if count < 3 then (0, Empty) else win 
foo current@(count, mark) currentMark = if mark == currentMark
                                        then (count + 1, mark)
                                        else if count < 3 
                                             then (1, currentMark)
                                             else current 
                                               
checkCount :: (Int, Mark) -> Bool
checkCount (count, mark) = and[count >= 3, not $ mark == Empty]

data Mark = Oh
          | Ex
          | Empty
          deriving (Show, Eq)

instance Arbitrary Mark where
  arbitrary = elements [Oh, Ex, Empty]

prop_only_empty_elements_is_no_result xs = and [not $ elem Oh xs, not $ elem Ex xs] ==> result xs == False

prop_three_in_a_row_has_a_result xs = or [[Oh, Oh, Oh] `isInfixOf` xs, [Ex, Ex, Ex] `isInfixOf` xs]  ==> result xs == True 