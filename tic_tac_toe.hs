import Test.QuickCheck

-- does the game have a result?
result :: [Int] -> Bool
result _ = False

prop_list_of_two_elements_is_no_result xs = (length xs < 3) ==> result xs == False
