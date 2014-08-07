-- Types module.
-- By G.W. Schwartz
--
-- Collects all application specific types. Used here for Text.Lazy

module Data.Fasta.Text.Lazy.Types where

-- Built-in
import qualified Data.Text.Lazy as T

-- Algebraic
data FastaSequence = FastaSequence { fastaInfo :: T.Text
                                   , fastaSeq  :: T.Text
                                   } deriving (Eq, Ord, Show)
