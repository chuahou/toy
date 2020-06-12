-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou
-- https://www.codewars.com/kata/5470c635304c127cad000f0d/train/haskell
-- 
-- Additional reference:
--  G. Jones, Functional Programming. Lecture Notes, 2019.
--  https://www.youtube.com/watch?v=N9RUqGYuGfw

module RegExpParser (RegExp(..) , parseRegExp) where

data RegExp = Normal Char       -- A character that is not in "()*|."
            | Any               -- Any character (.)
            | ZeroOrMore RegExp -- 0+ occurences of the same regexp (*)
            | Or RegExp RegExp  -- A choice between 2 regexps (|)
            | Str [RegExp]      -- A sequence of regexps
    deriving (Show, Eq)

parseRegExp :: String -> Maybe RegExp
parseRegExp s = undefined
