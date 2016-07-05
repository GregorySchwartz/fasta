-- Utility Module
-- By Gregory W. Schwartz

{- | Collects all helpful random functions.
-}

module Data.Fasta.Text.Utility ( getField
                               ) where

-- Built in

-- Cabal
import qualified Data.Text as T

-- Local
import Data.Fasta.Text.Types

-- | Gets a 1 indexed field from the header of a fasta sequence using a certain
-- delimiter.
getField :: Int -> Char -> FastaSequence -> T.Text
getField field delim = (!! (field - 1)) . T.split (== delim) . fastaHeader
