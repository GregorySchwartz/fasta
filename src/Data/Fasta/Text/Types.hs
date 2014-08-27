-- Types module.
-- By G.W. Schwartz
--
-- Collects all application specific types. Used here for Text.

{-# LANGUAGE OverloadedStrings #-}

module Data.Fasta.Text.Types where

-- Built-in
import qualified Data.Text as T
import qualified Data.Map as M

-- Algebraic
data FastaSequence = FastaSequence { fastaHeader :: T.Text
                                   , fastaSeq    :: T.Text
                                   } deriving (Eq, Ord, Show)

-- Basic
type Clone    = FastaSequence
type Germline = FastaSequence
type Codon    = T.Text

-- Advanced
type CloneMap = M.Map (Int, Germline) [Clone]

-- Classes
class ShowText a where
    showText :: a -> T.Text

-- Instances
instance ShowText FastaSequence where
    showText FastaSequence {fastaHeader = x, fastaSeq = y} = T.concat [ ">"
                                                                      , x
                                                                      , "\n"
                                                                      , y ]
