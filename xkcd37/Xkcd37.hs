-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

xkcdify :: String -> String
xkcdify = unwords . assify . words

assEnd :: String
assEnd = "-ass"

assEnd' :: String
assEnd' = tail assEnd

assPre :: String
assPre = cs ++ [c]
    where (c:cs) = assEnd

assify :: [String] -> [String]
assify []   = []
assify [cs] = [cs]
assify (cs:ds:css)
    | endcs == assEnd   = startcs  : (assPre ++ ds) : assify css
    | endcs' == assEnd' = startcs' : (assPre ++ ds) : assify css
    | otherwise       = cs : assify (ds:css)
        where (startcs,  endcs)  = splitAt (length cs - length assEnd) cs
              (startcs', endcs') = (startcs ++ [head endcs], tail endcs)
