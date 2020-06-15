-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

xkcdify :: String -> String
xkcdify = unwords . assify . words

assify :: [String] -> [String]
assify []   = []
assify [cs] = [cs]
assify (cs:ds:css)
    | endcs == ass = startcs' : (ass ++ (hyphen:ds)) : assify css
    | otherwise    = cs       : assify (ds:css)
        where (startcs, endcs) = splitAt (length cs - length ass) cs
              startcs' = if last startcs == hyphen -- if ends in hyphen
                         then init startcs         -- discard that hyphen
                         else startcs

----- Constants -----

ass :: String
ass = "ass"

hyphen :: Char
hyphen = '-'
