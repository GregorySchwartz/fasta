-- Types module.
-- By G.W. Schwartz
--
-- Collects all application specific types. Used here for Text.

module Data.Fasta.Text where

-- Built-in
import qualified Data.Text as T

-- Algebraic
data FastaSequence = FastaSequence { fastaInfo :: T.Text
                                   , fastaSeq  :: T.Text
                                   } deriving (Eq, Ord, Show)
