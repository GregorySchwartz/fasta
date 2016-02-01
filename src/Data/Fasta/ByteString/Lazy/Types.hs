-- Types module.
-- By G.W. Schwartz
--
{- | Collects all application specific types. Used here for ByteString.Lazy
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.Fasta.ByteString.Lazy.Types where

-- Built-in
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as M

-- Algebraic
data FastaSequence = FastaSequence { fastaHeader :: BL.ByteString
                                   , fastaSeq    :: BL.ByteString
                                   } deriving (Eq, Ord, Show)

-- Basic
type Codon    = BL.ByteString
type AA       = Char
type Clone    = FastaSequence
type Germline = FastaSequence

-- Advanced
-- | A clone is a collection of sequences derived from a germline with
-- a specific identifier
type CloneMap = M.Map (Int, Germline) [Clone]

-- Classes
class ShowFasta a where
    showFasta :: a -> BL.ByteString

-- Instances
instance ShowFasta FastaSequence where
    showFasta FastaSequence {fastaHeader = x, fastaSeq = y} = BL.concat [ ">"
                                                                        , x
                                                                        , "\n"
                                                                        , y ]
