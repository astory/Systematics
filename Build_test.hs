import Build
import Test.QuickCheck.Test
import Test.QuickCheck

main = do
      quickCheck (prop_preserves :: (Int, [Int]) -> Bool)
      quickCheck
          (\ (start, list) -> not (null list) ==>
          ((prop_last_number :: (Int, [Int]) -> Bool) (start, list)))
