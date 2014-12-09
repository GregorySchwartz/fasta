-- Translation Module
-- By Gregory W. Schwartz

{- | Collects all functions pertaining to the translation of nucleotides to
amino acids for Text.
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.Fasta.Text.Translation ( codon2aa
                                   , translate ) where

-- Built in
import Data.Either
import qualified Data.Text as T

-- Local
import Data.Fasta.Text.Types

-- | Converts a codon to an amino acid
-- Remember, if there is an "N" in that DNA sequence, then it is invalid
codon2aa :: Codon -> Either T.Text T.Text
codon2aa x
    | codon `elem` ["GCT", "GCC", "GCA", "GCG"]               = Right "A"
    | codon `elem` ["CGT", "CGC", "CGA", "CGG", "AGA", "AGG"] = Right "R"
    | codon `elem` ["AAT", "AAC"]                             = Right "N"
    | codon `elem` ["GAT", "GAC"]                             = Right "D"
    | codon `elem` ["TGT", "TGC"]                             = Right "C"
    | codon `elem` ["CAA", "CAG"]                             = Right "Q"
    | codon `elem` ["GAA", "GAG"]                             = Right "E"
    | codon `elem` ["GGT", "GGC", "GGA", "GGG"]               = Right "G"
    | codon `elem` ["CAT", "CAC"]                             = Right "H"
    | codon `elem` ["ATT", "ATC", "ATA"]                      = Right "I"
    | codon `elem` ["ATG"]                                    = Right "M"
    | codon `elem` ["TTA", "TTG", "CTT", "CTC", "CTA", "CTG"] = Right "L"
    | codon `elem` ["AAA", "AAG"]                             = Right "K"
    | codon `elem` ["TTT", "TTC"]                             = Right "F"
    | codon `elem` ["CCT", "CCC", "CCA", "CCG"]               = Right "P"
    | codon `elem` ["TCT", "TCC", "TCA", "TCG", "AGT", "AGC"] = Right "S"
    | codon `elem` ["ACT", "ACC", "ACA", "ACG"]               = Right "T"
    | codon `elem` ["TGG"]                                    = Right "W"
    | codon `elem` ["TAT", "TAC"]                             = Right "Y"
    | codon `elem` ["GTT", "GTC", "GTA", "GTG"]               = Right "V"
    | codon `elem` ["TAA", "TGA", "TAG"]                      = Right "*"
    | codon `elem` ["---", "..."]                             = Right "-"
    | codon == "~~~"                                          = Right "-"
    | "N" `T.isInfixOf` codon                                 = Right "-"
    | "-" `T.isInfixOf` codon                                 = Right "-"
    | "." `T.isInfixOf` codon                                 = Right "-"
    | otherwise                                               = Left errorMsg
  where
    codon    = T.toUpper x
    errorMsg = T.append "Unidentified codon: " codon

-- | Translates a string of nucleotides. Returns a text with the error if the
-- codon is invalid.
translate :: Int -> FastaSequence -> Either T.Text FastaSequence
translate pos x
    | any isLeft' translation = Left $ head . lefts $ translation
    | otherwise               = Right $ x { fastaSeq = T.concat
                                                     . rights
                                                     $ translation }
  where
    translation = map codon2aa
                . filter ((== 3) . T.length)
                . T.chunksOf 3
                . T.drop (pos - 1)
                . fastaSeq
                $ x
    isLeft' (Left _) = True
    isLeft' _        = False
