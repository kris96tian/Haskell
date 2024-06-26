-- A program that analyzes DNA sequence & gives the longestCommonSubsequence of the two and the Edt distance.

--  1.Data Type Definition:
--  DNASequence: This is a newtype representing a DNA sequence. A Text value, which stores the DNA sequence as text.

--  2.Function Definitions:
--   *longestCommonSubsequence: this Fnc  takes two DNASequence objects and finds their longest common subsequence (LCS).
--    It compares the sequences char by char, finding the longest sequence of characters that appear in the same order in both sequences. 
--   *editDistance: Fnc calculates the edit distance between two DNASequence objects. The edit distance is the minimum number of operations (insertion, deletion, or substitution) required to change one sequence into the other.

import Data.Text (Text)
import qualified Data.Text as T

-- Define DNA-sequence type
newtype DNASequence = DNASequence { getDNAString :: Text }
  deriving (Eq, Show)

-- Find longest common subseq. (LCS) 
longestCommonSubsequence :: DNASequence -> DNASequence -> DNASequence
longestCommonSubsequence (DNASequence seq1) (DNASequence seq2) =
  DNASequence $ T.pack $ lcs (T.unpack seq1) (T.unpack seq2)
  where
    lcs [] _ = []
    lcs _ [] = []
    lcs (x:xs) (y:ys)
      | x == y    = x : lcs xs ys
      | otherwise = longer (lcs xs (y:ys)) (lcs (x:xs) ys)
    longer xs ys = if length xs > length ys then xs else ys

-- Find edit distance 
editDistance :: DNASequence -> DNASequence -> Int
editDistance (DNASequence seq1) (DNASequence seq2) = ed (T.length seq1) (T.length seq2)
  where
    ed i 0 = i
    ed 0 j = j
    ed i j
      | T.index seq1 (i-1) == T.index seq2 (j-1) = ed (i-1) (j-1)
      | otherwise = 1 + minimum [ed (i-1) j, ed i (j-1), ed (i-1) (j-1)]


--  MAIN - - - - - - - - -- - - - - - - - - - - - -  
main :: IO ()
main = do
  let seq1 = DNASequence (T.pack "ATCG")
      seq2 = DNASequence (T.pack "ATGC")
  
  putStrLn $ "LCS: " ++ T.unpack (getDNAString (longestCommonSubsequence seq1 seq2))
  
  putStrLn $ "Edit Dist: " ++ show (editDistance seq1 seq2)

------------------------------------------
--       Output:
--
--        LCS: ATC
--  Edit Dist: 2
-------------------------------------------
