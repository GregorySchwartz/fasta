-- Types module.
-- By G.W. Schwartz
--
-- Collects all application specific types. Used here for strings.

module Data.Fasta.String.Types where

-- Built-in
import qualified Data.Map as M

-- Algebraic
data FastaSequence = FastaSequence { fastaHeader :: String
                                   , fastaSeq    :: String
                                   } deriving (Eq, Ord, Show)

-- Basic
type Codon    = String
type Clone    = FastaSequence
type Germline = FastaSequence

-- Advanced
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
