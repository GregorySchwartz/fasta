-- Types module.
-- By G.W. Schwartz
--
-- Collects all application specific types. Used here for strings.

module Data.Fasta.String.Types where

-- Built-in
import qualified Data.Map as M

-- Algebraic
data FastaSequence = FastaSequence { fastaInfo :: String
                                   , fastaSeq  :: String
                                   } deriving (Eq, Ord)

-- Basic
type Codon    = String
type Clone    = FastaSequence
type Germline = FastaSequence

-- Advanced
type CloneMap = M.Map (Int, Germline) [Clone]

-- Instances
instance Show FastaSequence where
    show FastaSequence {fastaInfo = x, fastaSeq = y} = ">" ++ x ++ "\n" ++ y
