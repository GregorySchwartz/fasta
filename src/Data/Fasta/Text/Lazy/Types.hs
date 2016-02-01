-- Types module.
-- By G.W. Schwartz
--
{- | Collects all application specific types. Used here for Text.Lazy
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.Fasta.Text.Lazy.Types where

-- Built-in
import qualified Data.Text.Lazy as T
import qualified Data.Map as M

-- Algebraic
data FastaSequence = FastaSequence { fastaHeader :: T.Text
                                   , fastaSeq    :: T.Text
                                   } deriving (Eq, Ord, Show)

-- Basic
type Codon    = T.Text
type AA       = Char
type Clone    = FastaSequence
type Germline = FastaSequence

-- Advanced
-- | A clone is a collection of sequences derived from a germline with
-- a specific identifier
type CloneMap = M.Map (Int, Germline) [Clone]

-- Classes
class ShowFasta a where
    showFasta :: a -> T.Text

-- Instances
instance ShowFasta FastaSequence where
    showFasta FastaSequence {fastaHeader = x, fastaSeq = y} = T.concat [ ">"
                                                                       , x
                                                                       , "\n"
                                                                       , y ]
