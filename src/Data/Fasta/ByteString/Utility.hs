-- Utility Module
-- By Gregory W. Schwartz

{- | Collects all helpful random functions.
-}

module Data.Fasta.ByteString.Utility ( getField
                                     ) where

-- Built in

-- Cabal
import qualified Data.ByteString.Char8 as B

-- Local
import Data.Fasta.ByteString.Types

-- | Gets a 1 indexed field from the header of a fasta sequence using a certain
-- delimiter.
getField :: Int -> Char -> FastaSequence -> B.ByteString
getField field delim = (!! (field - 1)) . B.split delim . fastaHeader
