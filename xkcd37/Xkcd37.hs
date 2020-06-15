-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

xkcdify :: String -> String
xkcdify = unwords . filter (not . null) . assify . words

assify :: [String] -> [String]
assify []   = []
assify [cs] = [cs]
assify (cs:ds:css)
    | endcs == ass = prunehyp startcs : (ass ++ (hyphen:ds)) : assify css
    | otherwise    = cs               : assify (ds:css)
        where (startcs, endcs) = splitAt (length cs - length ass) cs
              prunehyp ""     = ""
              prunehyp (c:"") | c == hyphen = ""
                              | otherwise   = [c]
              prunehyp (c:cs) = c:prunehyp cs

----- Constants -----

ass :: String
ass = "ass"

hyphen :: Char
hyphen = '-'
