-- Translation Module
-- By Gregory W. Schwartz

-- Collects all functions pertaining to the translation of nucleotides to
-- amino acids for strings.

module Data.Fasta.String.Translation where

-- Built in
import Data.Char

-- Cabal
import qualified Data.List.Split as Split

-- Local
import Data.Fasta.String.Types

-- Converts a codon to an amino acid
-- Remember, if there is an "N" in that DNA sequence, then it is invalid
codon2aa :: Codon -> Char
codon2aa x
    | codon `elem` ["GCT", "GCC", "GCA", "GCG"]               = 'A'
    | codon `elem` ["CGT", "CGC", "CGA", "CGG", "AGA", "AGG"] = 'R'
    | codon `elem` ["AAT", "AAC"]                             = 'N'
    | codon `elem` ["GAT", "GAC"]                             = 'D'
    | codon `elem` ["TGT", "TGC"]                             = 'C'
    | codon `elem` ["CAA", "CAG"]                             = 'Q'
    | codon `elem` ["GAA", "GAG"]                             = 'E'
    | codon `elem` ["GGT", "GGC", "GGA", "GGG"]               = 'G'
    | codon `elem` ["CAT", "CAC"]                             = 'H'
    | codon `elem` ["ATT", "ATC", "ATA"]                      = 'I'
    | codon `elem` ["ATG"]                                    = 'M'
    | codon `elem` ["TTA", "TTG", "CTT", "CTC", "CTA", "CTG"] = 'L'
    | codon `elem` ["AAA", "AAG"]                             = 'K'
    | codon `elem` ["TTT", "TTC"]                             = 'F'
    | codon `elem` ["CCT", "CCC", "CCA", "CCG"]               = 'P'
    | codon `elem` ["TCT", "TCC", "TCA", "TCG", "AGT", "AGC"] = 'S'
    | codon `elem` ["ACT", "ACC", "ACA", "ACG"]               = 'T'
    | codon `elem` ["TGG"]                                    = 'W'
    | codon `elem` ["TAT", "TAC"]                             = 'Y'
    | codon `elem` ["GTT", "GTC", "GTA", "GTG"]               = 'V'
    | codon `elem` ["TAA", "TGA", "TAG"]                      = '*'
    | codon `elem` ["---", "..."]                             = '-'
    | codon == "~~~"                                          = '-'
    | 'N' `elem` codon                                        = '-'
    | '-' `elem` codon                                        = '-'
    | '.' `elem` codon                                        = '-'
    | otherwise                                               = error errorMsg
  where
    codon    = map toUpper x
    errorMsg = "Unidentified codon: " ++ codon

-- Translates a string of nucleotides
translate :: FastaSequence -> FastaSequence
translate x = x { fastaSeq = map codon2aa
                           . filter ((== 3) . length)
                           . Split.chunksOf 3
                           . fastaSeq
                           $ x }
