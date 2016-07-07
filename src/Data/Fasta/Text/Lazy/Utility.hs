-- Utility Module
-- By Gregory W. Schwartz

{- | Collects all helpful random functions.
-}

module Data.Fasta.Text.Lazy.Utility ( getField
                                    , compl
                                    , revCompl
                                    ) where

-- Built in

-- Cabal
import qualified Data.Text.Lazy as T

-- Local
import Data.Fasta.Text.Lazy.Types
import Data.Fasta.Utility

-- | Gets a 1 indexed field from the header of a fasta sequence using a certain
-- delimiter.
getField :: Int -> Char -> FastaSequence -> T.Text
getField field delim = (!! (field - 1)) . T.split (== delim) . fastaHeader

-- | Gets the complement of the sequence.
compl :: FastaSequence -> FastaSequence
compl fs = fs { fastaSeq = T.map complRules . fastaSeq $ fs }

-- | Gets the reverse complement of the sequence.
revCompl :: FastaSequence -> FastaSequence
revCompl fs = fs { fastaSeq = T.reverse . T.map complRules . fastaSeq $ fs }
