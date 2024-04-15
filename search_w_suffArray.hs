--   Search for a pattern within a text using a suffix array and binary search----------------
-- 
--   new DataTpye: Suffix: Tuple containing a suffix string and its starting index.
--
--  Fnc:
--        suffixes: Gener all suffices of a given string.
---       compareSufficxes: Compares two suffics based on their string values.
---       suffixArray: Builds the suffix array of a string.
---       binarySearch: Performs binary search on the suffix array to find occur of a pat.
---       search: for a pattern in a text using the suffArr and binSearch.
-----------------------------------------------------------------------------------------------
import Data.List

--type for suffixes
type Suffix = (String, Int)

--generate all suffixes of a text
suffixes :: String -> [Suffix]
suffixes s = [(drop i s, i) | i <- [0..length s - 1]]

compareSuffixes :: Suffix -> Suffix -> Ordering
compareSuffixes (s1, _) (s2, _) = compare s1 s2

suffixArray :: String -> [Int]
suffixArray s = map snd $ sortBy compareSuffixes $ suffixes s

--binary search for pattern in suffix array
binarySearch :: String -> [Suffix] -> String -> [Int]
binarySearch pattern [] _ = []
binarySearch pattern ((suffix, index):suffixes) text
    | pattern `isPrefixOf` suffix = index : binarySearch pattern suffixes text
    | pattern > suffix = binarySearch pattern suffixes text
    | otherwise = []
    where suffixLength = length suffix

--search for a pattern in text using suffix array
search :: String -> String -> [Int]
search pattern text = binarySearch pattern (suffixes text) text


main :: IO ()
main = do
    let text = "banana"
    let pattern = "an"
    let result = search pattern text
    putStrLn $ "Pattern '" ++ pattern ++ "' found in text '" ++ text ++ "' at these starting indices:  " ++ show result

--------OUTPUT--------------------
-- Pattern 'an' found in text 'banana' at these starting indices:  [1,3]
-----------------------------------
