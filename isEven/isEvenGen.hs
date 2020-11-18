-- SPDX-License-Identifier: WTFPL
-- Copyright (c) 2020 Chua Hou

exprs :: [String]
exprs = map (\x -> if even x then "True if x == "  <> show x <> " else"
                             else "False if x == " <> show x <> " else")
            ([0..maxBound] :: [Int])

main :: IO ()
main =  putStrLn "def isEven(x):"
     >> putStr   "    return "
     >> (putStr . unwords) exprs
     >> putStrLn " false"
