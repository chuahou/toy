-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou
-- https://www.codewars.com/kata/5470c635304c127cad000f0d/train/haskell

module RegExpParser ( RegExp(..)
                    , parseRegExp
                    ) where

import           Text.ParserCombinators.ReadP

data RegExp = Normal Char       -- ^ A character that is not in "()*|."
            | Any               -- ^ Any character
            | ZeroOrMore RegExp -- ^ Zero or more occurrences of the same regexp
            | Or RegExp RegExp  -- ^ A choice between 2 regexps
            | Str [RegExp]      -- ^ A sequence of regexps
    deriving (Show, Eq)

orP     = strP    +++ (Or <$> strP <* char '|' <*> strP)
strP    = Str     <$> many1 zeroOrP
zeroOrP = atomP   +++ (ZeroOrMore <$> atomP <* char '*')
atomP   = normalP +++ anyP +++ between (char '(') (char ')') orP
normalP = Normal  <$> satisfy (`notElem` "()*|.")
anyP    = Any     <$  char '.'

parseRegExp :: String -> Maybe RegExp
parseRegExp s = case readP_to_S (orP <* eof) s of
                  [(r, [])] -> Just $ unStr r
                  _         -> Nothing
    where
        unStr (ZeroOrMore x) = ZeroOrMore (unStr x)
        unStr (Or x y)       = Or (unStr x) (unStr y)
        unStr (Str [x])      = unStr x
        unStr (Str xs)       = Str (map unStr xs)
        unStr other          = other
