import           Test.Hspec
import Lib

main :: IO ()
main = putStrLn "Test suite not yet implemented"

-- testInitBoard = hspec $ do
--   describe "testing initBoard" $ do
--     it "Beginner correct number of mines" $ do
--       length (map (filter (\c -> number c == 0)) (initBoard beginner)) `shouldBe` 10
