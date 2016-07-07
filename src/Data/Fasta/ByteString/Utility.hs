-- Utility Module
-- By Gregory W. Schwartz

{- | Collects all helpful random functions.
-}

module Data.Fasta.ByteString.Utility ( getField
                                     , compl
                                     , revCompl
                                     ) where

-- Built in

-- Cabal
import qualified Data.ByteString.Char8 as B

-- Local
import Data.Fasta.ByteString.Types
import Data.Fasta.Utility

-- | Gets a 1 indexed field from the header of a fasta sequence using a certain
-- delimiter.
getField :: Int -> Char -> FastaSequence -> B.ByteString
getField field delim = (!! (field - 1)) . B.split delim . fastaHeader

-- | Gets the complement of the sequence.
compl :: FastaSequence -> FastaSequence
compl fs = fs { fastaSeq = B.map complRules . fastaSeq $ fs }

-- | Gets the reverse complement of the sequence.
revCompl :: FastaSequence -> FastaSequence
revCompl fs = fs { fastaSeq = B.reverse . B.map complRules . fastaSeq $ fs }
