-- Types module.
-- By G.W. Schwartz
--
{- | Collects all application specific types. Used here for strings.
-}

module Data.Fasta.String.Types where

-- Built-in
import qualified Data.Map as M

-- Algebraic
data FastaSequence = FastaSequence { fastaHeader :: String
                                   , fastaSeq    :: String
                                   } deriving (Eq, Ord, Show)

-- Basic
type Codon    = String
type AA       = Char
type Clone    = FastaSequence
type Germline = FastaSequence

-- Advanced
-- | A clone is a collection of sequences derived from a germline with
-- a specific identifier
type CloneMap = M.Map (Int, Germline) [Clone]

-- Classes
class ShowFasta a where
    showFasta :: a -> String

-- Instances
instance ShowFasta FastaSequence where
    showFasta FastaSequence {fastaHeader = x, fastaSeq = y} = concat [ ">"
                                                                     , x
                                                                     , "\n"
                                                                     , y ]
