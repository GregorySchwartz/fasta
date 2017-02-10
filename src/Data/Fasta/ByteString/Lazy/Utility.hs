-- Utility Module
-- By Gregory W. Schwartz

{- | Collects all helpful random functions.
-}

module Data.Fasta.ByteString.Lazy.Utility ( getField
                                          , compl
                                          , revCompl
                                          ) where

-- Built in
import Data.Maybe

-- Cabal
import Control.Lens
import qualified Data.ByteString.Lazy.Char8 as BL

-- Local
import Data.Fasta.ByteString.Lazy.Types
import Data.Fasta.Utility

-- | Gets a 1 indexed field from the header of a fasta sequence using a certain
-- delimiter.
getField :: Int -> Char -> FastaSequence -> BL.ByteString
getField field delim = fromMaybe (error "getField: Field out of bounds in fasta header")
                     . preview (ix $ field - 1)
                     . BL.split delim
                     . fastaHeader

-- | Gets the complement of the sequence.
compl :: FastaSequence -> FastaSequence
compl fs = fs { fastaSeq = BL.map complRules . fastaSeq $ fs }

-- | Gets the reverse complement of the sequence.
revCompl :: FastaSequence -> FastaSequence
revCompl fs = fs { fastaSeq = BL.reverse . BL.map complRules . fastaSeq $ fs }
