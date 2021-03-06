-- Translation Module
-- By Gregory W. Schwartz

{- | Collects all functions pertaining to the translation of nucleotides to
amino acids for Lazy ByteString.
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.Fasta.ByteString.Lazy.Translation ( chunksOf
                                              , codon2aa
                                              , customCodon2aa
                                              , translate
                                              , customTranslate
                                              ) where

-- Built in
import Data.Char
import Data.Either
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Int

-- Local
import Data.Fasta.ByteString.Lazy.Types

-- | Lazy ByteString version of chunksOf
chunksOf :: Int64 -> BL.ByteString -> [BL.ByteString]
chunksOf k = go
  where
    go t = case BL.splitAt k t of
             (a,b) | BL.null a    -> []
                   | otherwise    -> a : go b

-- | Converts a codon to an amino acid
-- Remember, if there is an "N" in that DNA sequence, then it is translated
-- as an X, an unknown amino acid.
codon2aa :: Codon -> Either BL.ByteString AA
codon2aa x
    | codon `elem` ["GCT", "GCC", "GCA", "GCG"]               = Right 'A'
    | codon `elem` ["CGT", "CGC", "CGA", "CGG", "AGA", "AGG"] = Right 'R'
    | codon `elem` ["AAT", "AAC"]                             = Right 'N'
    | codon `elem` ["GAT", "GAC"]                             = Right 'D'
    | codon `elem` ["TGT", "TGC"]                             = Right 'C'
    | codon `elem` ["CAA", "CAG"]                             = Right 'Q'
    | codon `elem` ["GAA", "GAG"]                             = Right 'E'
    | codon `elem` ["GGT", "GGC", "GGA", "GGG"]               = Right 'G'
    | codon `elem` ["CAT", "CAC"]                             = Right 'H'
    | codon `elem` ["ATT", "ATC", "ATA"]                      = Right 'I'
    | codon `elem` ["ATG"]                                    = Right 'M'
    | codon `elem` ["TTA", "TTG", "CTT", "CTC", "CTA", "CTG"] = Right 'L'
    | codon `elem` ["AAA", "AAG"]                             = Right 'K'
    | codon `elem` ["TTT", "TTC"]                             = Right 'F'
    | codon `elem` ["CCT", "CCC", "CCA", "CCG"]               = Right 'P'
    | codon `elem` ["TCT", "TCC", "TCA", "TCG", "AGT", "AGC"] = Right 'S'
    | codon `elem` ["ACT", "ACC", "ACA", "ACG"]               = Right 'T'
    | codon `elem` ["TGG"]                                    = Right 'W'
    | codon `elem` ["TAT", "TAC"]                             = Right 'Y'
    | codon `elem` ["GTT", "GTC", "GTA", "GTG"]               = Right 'V'
    | codon `elem` ["TAA", "TGA", "TAG"]                      = Right '*'
    | codon `elem` ["---", "..."]                             = Right '-'
    | codon == "~~~"                                          = Right '-'
    | 'N' `BL.elem` codon                                     = Right 'X'
    | '-' `BL.elem` codon                                     = Right '-'
    | '.' `BL.elem` codon                                     = Right '-'
    | otherwise                                               = Left errorMsg
  where
    codon    = BL.map toUpper x
    errorMsg = BL.append "Unidentified codon: " codon

-- | Translate a codon using a custom table
customCodon2aa :: [(Codon, Char)] -> Codon -> Either BL.ByteString AA
customCodon2aa table codon = case lookup codon table of
                                (Just x) -> Right x
                                Nothing  -> codon2aa codon

-- | Translates a bytestring of nucleotides given a reading frame (1, 2, or
-- 3) -- drops the first 0, 1, or 2 nucleotides respectively. Returns
-- a bytestring with the error if the codon is invalid. Also has customized
-- codon translations as well overriding the defaults.
customTranslate :: [(Codon, AA)]
                -> Int64
                -> FastaSequence
                -> Either BL.ByteString FastaSequence
customTranslate table pos x
    | any isLeft' translation = Left $ head . lefts $ translation
    | otherwise               = Right $ x { fastaSeq = BL.pack
                                                     . rights
                                                     $ translation }
  where
    translation = map (customCodon2aa table)
                . filter ((== 3) . BL.length)
                . chunksOf 3
                . BL.drop (pos - 1)
                . fastaSeq
                $ x
    isLeft' (Left _) = True
    isLeft' _        = False

-- | Translates a bytestring of nucleotides given a reading frame (1, 2, or
-- 3) -- drops the first 0, 1, or 2 nucleotides respectively. Returns
-- a bytestring with the error if the codon is invalid.
translate :: Int64 -> FastaSequence -> Either BL.ByteString FastaSequence
translate = customTranslate []
