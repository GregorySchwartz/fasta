-- Utility Module
-- By Gregory W. Schwartz

{- | Collects all helpful random functions.
-}

module Data.Fasta.ByteString.Lazy.Utility ( getField
                                          ) where

-- Built in

-- Cabal
import qualified Data.ByteString.Lazy.Char8 as BL

-- Local
import Data.Fasta.ByteString.Lazy.Types

-- | Gets a 1 indexed field from the header of a fasta sequence using a certain
-- delimiter.
getField :: Int -> Char -> FastaSequence -> BL.ByteString
getField field delim = (!! (field - 1)) . BL.split delim . fastaHeader
