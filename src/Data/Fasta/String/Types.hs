-- Types module.
-- By G.W. Schwartz
--
-- Collects all application specific types. Used here for strings.

module Data.Fasta.String.Types where

-- Algebraic
data FastaSequence = FastaSequence { fastaInfo :: String
                                   , fastaSeq  :: String
                                   } deriving (Eq, Ord, Show)
