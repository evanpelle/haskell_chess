import Test.HUnit
--import chess

test1 = TestCase (assertEqual "this is a test" True True)


tests = TestList [TestLabel "test1" test1]