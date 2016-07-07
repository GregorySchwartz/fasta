-- Utility Module
-- By Gregory W. Schwartz

{- | Collects all helpful random functions.
-}

module Data.Fasta.String.Utility ( getField
                                 , compl
                                 , revCompl
                                 ) where

-- Built in

-- Cabal
import qualified Data.List.Split as Split

-- Local
import Data.Fasta.String.Types
import Data.Fasta.Utility

-- | Gets a 1 indexed field from the header of a fasta sequence using a certain
-- delimiter.
getField :: Int -> Char -> FastaSequence -> String
getField field delim = (!! (field - 1)) . Split.splitWhen (== delim) . fastaHeader

-- | Gets the complement of the sequence.
compl :: FastaSequence -> FastaSequence
compl fs = fs { fastaSeq = fmap complRules . fastaSeq $ fs }

-- | Gets the reverse complement of the sequence.
revCompl :: FastaSequence -> FastaSequence
revCompl fs = fs { fastaSeq = reverse . fmap complRules . fastaSeq $ fs }
