import Test.HUnit
import Chess

test1 = TestCase (assertEqual "this is a test" True True)



test2 = TestCase (assertEqual "replaceNth" (replaceNth 1 5 [1,2,3]) [1,5,3])

test3 = TestCase (assertEqual "getPiece"
    (getPiece (0, 0) startBoard)
    (Rook Black))

boardWithRook = setPiece (3,3) (Rook White) startBoard
test4 = TestCase (assertEqual "setPiece"
    (getPiece (3,3) boardWithRook)
    (Rook White))

blackKing = getPiece (0, 3) startBoard
changedBoard = setPiece (4,4) blackKing startBoard
test5 = TestCase (assertEqual "makeMove"
    (makeMove ((0,3), (4,4)) startBoard)
    (changedBoard))


------calcLine------
line =  [(2,2), (2,3), (2,4), (2,5), (2,6), (2,7), (2,8)]
result1 = filterLine Black line startBoard
correctValue1 = take 5 line
test6 = TestCase (assertEqual "filterLine" result1 correctValue1)

result2 = filterLine White line startBoard
correctValue2 = take 4 line
test7 = TestCase (assertEqual "filterLine" result2 correctValue2)

line2 = [(4, 3), (5, 3), (6, 3), (7, 3), (8, 3), (9, 3)]
result3 = filterLine White line2 startBoard
correctValue3 = take 4 line2
test8 = TestCase (assertEqual "filterLine" result3 correctValue3)
--------------------

test9 = TestCase (assertEqual "mergeList"
    (mergeList [0..3] [0..9])
    ([(0,0), (1,1), (2,2), (3,3)]))
--------------------
test10 = TestCase (assertEqual "genLine" 
    ([(3,3), (3,2), (3,1), (3,0)])
    (genLine (3,3) (0, -1)))


test11 = TestCase (assertEqual "getSeries"
    ([(3,2), (3,1)])
    (getSeries White (3,3) (0,-1) startBoard))



--runTestTT tests
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2,
    TestLabel "getPiece" test3, TestLabel "setPiece" test4,
    TestLabel "makeMove" test5, TestLabel "calcLineDiffColor" test6,
    TestLabel "calcLineSameColor" test7, TestLabel "calcLineEdge" test8,
    TestLabel "mergeList" test9, TestLabel "genLine" test10,
    TestLabel "getSeries" test11]