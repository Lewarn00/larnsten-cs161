module Tests where

import Lab3

data TestResult
    = Success
    | Failure String

isSuccess :: TestResult -> Bool
isSuccess Success = True
isSuccess _       = False

message :: TestResult -> String
message Success           = "Success!"
message (Failure message) = message

-- Test a function that takes one argument.
-- Usage: expect1 "myFunc" myFunc arg expectedOutput
expect1 :: (Show a, Show b, Eq b) => String -> (a -> b) -> a -> b -> TestResult
expect1 funcName func input expectedOutput =
    if expectedOutput == actual then
        Success
    else
        Failure $
            "Expected " ++ evaledStr ++
            " to be " ++ show expectedOutput ++
            ", but got " ++ show actual
    where
        actual    = func input
        evaledStr = funcName ++ " " ++ show input


-- This is where you add new test cases.
tests :: [TestResult]
tests =
    [ expect1 "eval" eval
        (NumberA (-3))
        (-3)
    , expect1 "eval" eval
        (MultA (Plus (NumberA 2) (NumberA (-6))) (Plus (NumberA 3) (NumberA 2)))
        (-20)
    , expect1 "eval" eval
        (DivA (NumberA 13) (NumberA 6))
        2
    , expect1 "noSpace" noSpace
        "he llo wor l d"
        "helloworld"
    , expect1 "tokenize" tokenize
         "1+2"
         [TokN 1, Add, TokN 2]        
    , expect1 "tokenize" tokenize
         "1+2"
         [TokN 1, Add, TokN 2]
    , expect1 "tokenize" tokenize
         "1 + 20"
         [TokN 1, Add, TokN 20]
    , expect1 "tokenize" tokenize
         "1 * -2"
         [TokN 1, Mult, TokN (-2)]
    , expect1 "tokenize" tokenize
         "1 + 2 * 3 + 4"
         [TokN 1, Add, TokN 2, Mult, TokN 3, Add, TokN 4]
    , expect1 "tokenize" tokenize
         "1 * 2 + 3 + 4"
         [TokN 1, Mult, TokN 2, Add, TokN 3, Add, TokN 4]
    , expect1 "tokenize" tokenize
        "(1+2)"
        [Par [TokN 1, Add, TokN 2]]
    , expect1 "tokenize" tokenize
         " (1 + 2 )"
         [Par [TokN 1, Add, TokN 2]]
    , expect1 "tokenize" tokenize
         "(1 * 2 + 3)"
         [Par [TokN 1, Mult, TokN 2, Add, TokN 3]]
    , expect1 "tokenize" tokenize
         "(-10 + 2) * 5"
         [Par [TokN (-10), Add, TokN 2], Mult, TokN 5]
    , expect1 "tokenize" tokenize
         "5 * (-10 + (2 + 4) * 3)"
         [TokN 5, Mult, Par [TokN (-10), Add, Par [TokN 2, Add, TokN 4], Mult, TokN 3]]
    , expect1 "tokenize" tokenize
         "5 * (-10 + (2 + 4) * 3) * (3 + 2)"
         [TokN 5, Mult, Par [TokN (-10), Add, Par [TokN 2, Add, TokN 4], Mult, TokN 3], Mult, Par [TokN 3, Add, TokN 2]]
    , expect1 "parse" parse
         [TokN 1, Add, TokN 20]
         (Plus (NumberA 1) (NumberA 20))
    , expect1 "parse" parse
         [Par [TokN 1, Mult, TokN 2, Add, TokN 3]]
         (Plus (MultA (NumberA 1) (NumberA 2)) (NumberA 3))
    , expect1 "parse" parse
         [TokN 1, Add, TokN 2, Mult, TokN 3, Add, TokN 4]
         (Plus (NumberA 1) (Plus (MultA (NumberA 2) (NumberA 3)) (NumberA 4)))
    , expect1 "parse" parse
         [TokN 1, Mult, TokN 2, Add, TokN 3, Add, TokN 4]
         (Plus (MultA (NumberA 1) (NumberA 2)) (Plus (NumberA 3) (NumberA 4)))           
    , expect1 "parse" parse
         [Par [TokN (-10), Add, TokN 2], Mult, TokN 5]
         (MultA (Plus (NumberA (-10)) (NumberA 2)) (NumberA 5))         
    , expect1 "parse" parse
         [TokN 5, Mult, Par [TokN (-10), Add, Par [TokN 2, Add, TokN 4], Mult, TokN 3]]
         (MultA (NumberA 5) (Plus (NumberA (-10)) (MultA (Plus (NumberA 2) (NumberA 4)) (NumberA 3))))
    , expect1 "evaluate" evaluate
         "(1 * 2 + 3) + 4"
         9
    , expect1 "evaluate" evaluate
         "(-10 + 2) * 5 + 6 / 3"
         (-38)
    , expect1 "evaluate" evaluate
         "5 * (-10 + (2 + 4) * 3) + (9 + 0) / 3"
         43
    , expect1 "evaluate" evaluate
         "5 * (-10 * 3) + (-9 + 2 / 2)" 
         (-158)
    ]


-- Inspect the below in GHCi.

-- DO NOT MODIFY BELOW THIS LINE IN YOUR SUBMISSION --

successes       = filter isSuccess tests
failures        = filter (not . isSuccess) tests
failureMessages = map message failures

results =
    ( length successes
    , length failures
    , failureMessages
    )

showFailures :: IO ()
showFailures = mapM_ putStrLn failureMessages
