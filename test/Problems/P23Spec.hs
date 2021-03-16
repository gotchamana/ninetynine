module Problems.P23Spec (spec) where

import           Data.List             (isSubsequenceOf, sort, unfoldr)
import qualified Problems.P23          as Problem
import qualified Solutions.P23         as Solution
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

properties :: ([Int] -> Int -> StdGen -> ([Int], StdGen)) -> String -> Spec
properties randomSelect name = do
  describe name $ do
    prop "selects given number of elements" $
      \xs -> forAll (chooseInt (0, length xs)) $ \n -> \seed ->
        fst (randomSelect xs n $ mkStdGen seed) `shouldSatisfy` (==) n . length

    prop "selects elements from list" $
      \xs -> forAll (chooseInt (0, length xs)) $ \n -> \seed ->
        fst (randomSelect xs n $ mkStdGen seed) `shouldSatisfy` flip isSubsequenceOf (sort xs) . sort

    modifyMaxSuccess (const 1) $ do
      prop "is random and returns new random generator" $
        -- Make a number of selections and confirm that the selection is random by
        -- checking at least one of them is different from another.
        -- It is theoretically possible for all of them to be the same with
        -- true random numbers, but it is vanishingly unlikely.
        --
        -- Similarly, this also tests that randomSelect returns a new random generator.
        -- If it did not, the use of the same generator would return identical selections.
        let xs = [1..100]
            n = 10
            select (_, g) = randomSelect xs n g
            selections g = map fst $ tail $ iterate select ([], g)
            isRandom ls = any (\(x,y) -> x /= y) $ zip ls $ tail ls
        in \seed -> (take 10 $ selections $ mkStdGen seed) `shouldSatisfy` isRandom

examples :: Spec
examples = do
  describe "Examples" $ do
    it "fst $ randomSelect \"abcdefgh\" 3 $ mkStdGen 10" $ do
      (fst $ randomSelect "abcdefgh" 3 $ mkStdGen 10)
        `shouldSatisfy` flip isSubsequenceOf "abcdefgh" . sort

    it "take 5 $ unfoldr (\\g -> Just $ randomSelect [1..100] 3 g) $ mkStdGen 111" $ do
      (take 5 $ unfoldr (\g -> Just $ randomSelect [1..100 :: Int] 3 g) $ mkStdGen 111)
        `shouldSatisfy` all (\l -> sort l `isSubsequenceOf` [1..100] && length l == 3)

    it "newStdGen >>= return . fst . randomSelect \"abcdefgh\" 3" $ do
      (newStdGen >>= return . fst . randomSelect "abcdefgh" 3)
        >>= (`shouldSatisfy` flip isSubsequenceOf "abcdefgh" . sort)

  where randomSelect l n g = Problem.randomSelect l n g

spec :: Spec
spec = parallel $ do
  properties Problem.randomSelect "randomSelect"
  examples
  describe "From solutions" $ do
    properties Solution.randomSelect "randomSelect"
