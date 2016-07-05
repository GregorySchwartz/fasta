-- Utility Module
-- By Gregory W. Schwartz

{- | Collects all helpful random functions.
-}

module Data.Fasta.String.Utility ( getField
                                 ) where

-- Built in

-- Cabal
import qualified Data.List.Split as Split

-- Local
import Data.Fasta.String.Types

-- | Gets a 1 indexed field from the header of a fasta sequence using a certain
-- delimiter.
getField :: Int -> Char -> FastaSequence -> String
getField field delim = (!! (field - 1)) . Split.splitWhen (== delim) . fastaHeader
