import Test.QuickCheck

prop_empty_list_is_no_result xs = (null xs) ==> result xs == False 
  where result xs = False