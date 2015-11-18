-- Category Module
-- By Gregory W. Schwartz

{- | Collects all functions pertaining to the categorization of nucleotides
- or amino acids
-}

module Data.Fasta.Category where

-- Algebraic
data Hydrophobicity = Hydrophobic | Neutral | Hydrophilic
                      deriving (Eq, Ord, Read, Show)

-- | Returns the hydrophobicity of an amino acid
aaToHydrophobicity :: Char -> Either String Hydrophobicity
aaToHydrophobicity x
    | x `elem` "IVLFCMW" = Right Hydrophobic
    | x `elem` "AGTSYPH" = Right Neutral
    | x `elem` "NDQEKR"  = Right Hydrophilic
    | otherwise          = Left (x : " is not a valid amino acid")
