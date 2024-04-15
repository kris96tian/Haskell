import Data.List


generateKmers :: Int -> String -> [String]
generateKmers k sequence = take (length sequence - k + 1) $ map (take k) (tails sequence)

countKmers :: [String] -> [(String, Int)]
countKmers kmers = map (\x -> (head x, length x)) $ group $ sort kmers

-- find the most freq kmers and their freq
mostFrequentKmer :: Int -> String -> [(String, Int)]
mostFrequentKmer k sequence =
    let kmers = generateKmers k sequence               -- Get all k-mers from sequence
        kmerCounts = countKmers kmers                  -- Count how often
        maxCount = findMaxFrequency kmerCounts        -- Find max f
        frequentKmers = filter (\(_, freq) -> freq == maxCount) kmerCounts  -- Filter k-mers with maxi freq
    in frequentKmers


-- Helper function to find the maaxxfreq using fold
findMaxFrequency :: [(String, Int)] -> Int
findMaxFrequency [] = 0  --indukt.Anfang
findMaxFrequency ((_, freq):rest) = foldl max freq (map snd rest)  


-- Main 
main :: IO ()
main = do
    let sequence1 = "ACGTACGTACGTACGTACGTACGTACGTACGT"
    let sequence2 = "ACGTTTTACGTTTTACGTTTTACGTTTTACGTTTT"
    let k = 3
    let frequentKmers1 = mostFrequentKmer k sequence1
    let frequentKmers2 = mostFrequentKmer k sequence2
    putStrLn $ "Sequence 1: " ++ sequence1
    putStrLn $ "Most frequent " ++ show k ++ "-mer(s) and their frequencies in Sequence 1:"
    mapM_ (\(mer, freq) -> putStrLn $ "K-mer: " ++ mer ++ ", Frequency: " ++ show freq) frequentKmers1
    putStrLn ""
    putStrLn $ "Sequence 2: " ++ sequence2
    putStrLn $ "Most frequent " ++ show k ++ "-mer(s) and their frequencies in Sequence 2:"
    mapM_ (\(mer, freq) -> putStrLn $ "K-mer: " ++ mer ++ ", Frequency: " ++ show freq) frequentKmers2
