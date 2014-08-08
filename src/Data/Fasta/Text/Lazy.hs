-- Types module.
-- By G.W. Schwartz
--
-- Collects all application specific types. Used here for Text.Lazy

module Data.Fasta.Text.Lazy where

-- Built-in
import qualified Data.Text.Lazy as T
import qualified Data.Map as M

-- Algebraic
data FastaSequence = FastaSequence { fastaInfo :: T.Text
                                   , fastaSeq  :: T.Text
                                   } deriving (Eq, Ord, Show)

-- Basic
type Clone    = FastaSequence
type Germline = FastaSequence

-- Advanced
type CloneMap = M.Map (Int, Germline) [Clone]
