--   Anyalyse sequences with functions:
--   DATA TYPSE >> DNASeq: Newtype for DNA sequences.
--   Functions:
----------------isValidDNA: Checks if a sequence contains valid nucleotides.
----------------reverseDNA: Reverses a DNA sequence.
----------------complementDNA: Finds the complementary sequence.
----------------gcContent: Calculates the GC content.

-------------------------------------------------------------------------------
module Main where

newtype DNASeq = DNASeq { getSeq :: String }
  deriving Show

isValidDNA :: DNASeq -> Bool
isValidDNA (DNASeq seq) = all (`elem` "ATCG") seq

reverseDNA :: DNASeq -> DNASeq
reverseDNA (DNASeq seq) = DNASeq (reverse seq)

complementDNA :: DNASeq -> DNASeq
complementDNA (DNASeq seq) = DNASeq (map complement seq)
  where
    complement 'A' = 'T'
    complement 'T' = 'A'
    complement 'C' = 'G'
    complement 'G' = 'C'
    complement x   = x

gcContent :: DNASeq -> Double
gcContent (DNASeq seq) = (fromIntegral (count 'G' + count 'C') / fromIntegral (length seq)) * 100
  where
    count c = length (filter (== c) seq)

main :: IO ()
main = do
  let dnaSeq = DNASeq "ATCGATTGACATCG"
  putStrLn $ show $ isValidDNA dnaSeq
  putStrLn $ getSeq $ reverseDNA dnaSeq
  putStrLn $ getSeq $ complementDNA dnaSeq
  putStrLn $ show $ gcContent dnaSeq

----------------------------------------------------
-- OUTPUT

--  True
--  GCTACAGTTAGCTA
--  TAGCTAACTGTAGC
--  42.857142857142854
-----------------------------------------------------
