-- Types module.
-- By Gregory W. Schwartz
--
{- | Collects all application specific types. Used here for Text.
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.Fasta.ByteString.Types where

-- Built-in
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

-- Algebraic
data FastaSequence = FastaSequence { fastaHeader :: B.ByteString
                                   , fastaSeq    :: B.ByteString
                                   } deriving (Eq, Ord, Show)

-- Basic
type Codon    = B.ByteString
type AA       = Char
type Clone    = FastaSequence
type Germline = FastaSequence

-- Advanced
-- | A clone is a collection of sequences derived from a germline with
-- a specific identifier
type CloneMap = M.Map (Int, Germline) [Clone]

-- Classes
class ShowFasta a where
    showFasta :: a -> B.ByteString

-- Instances
instance ShowFasta FastaSequence where
    showFasta FastaSequence {fastaHeader = x, fastaSeq = y} = B.concat [ ">"
                                                                       , x
                                                                       , "\n"
                                                                       , y ]
