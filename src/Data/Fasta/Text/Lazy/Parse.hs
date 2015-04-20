-- Parse module.
-- By G.W. Schwartz
--
{- | Collection of functions for the parsing of a fasta file. Uses the lazy Text
type.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Data.Fasta.Text.Lazy.Parse ( parseFasta
                                  , parseCLIPFasta
                                  , removeNs
                                  , removeN
                                  , removeCLIPNs ) where

-- Built-in
import Data.Char
import Control.Monad (void)
import qualified Data.Map.Strict as M
import Text.Parsec
import Text.Parsec.Text.Lazy
import qualified Data.Text.Lazy as T

-- Local
import Data.Fasta.Text.Lazy.Types

eol :: Parsec T.Text u String
eol = choice . map (try . string) $ ["\n\r", "\r\n", "\n", "\r"]

eoe :: Parsec T.Text u ()
eoe  = do
    lookAhead (void $ char '>') <|> eof

fasta :: Parsec T.Text u FastaSequence
fasta = do
    spaces
    char '>'
    header <- manyTill (satisfy (/= '>')) eol
    fseq <- manyTill anyChar eoe
    return (FastaSequence { fastaHeader = T.pack header
                          , fastaSeq = T.pack
                                     . map toUpper
                                     . removeWhitespace $ fseq } )
  where
    removeWhitespace = filter (`notElem` ("\n\r " :: String))

fastaFile :: Parsec T.Text u [FastaSequence]
fastaFile = do
    spaces
    many fasta

fastaCLIP :: Parsec T.Text u (FastaSequence, [FastaSequence])
fastaCLIP = do
    spaces
    char '>'
    germline <- fasta
    clones <- many $ try fasta
    return (germline, clones)

fastaCLIPFile :: Parsec T.Text u [(FastaSequence, [FastaSequence])]
fastaCLIPFile = do
    spaces
    many fastaCLIP

-- | Parse a standard fasta file into lazy text sequences
parseFasta :: T.Text -> [FastaSequence]
parseFasta = eToV . parse fastaFile "error"
  where
    eToV (Right x) = x
    eToV (Left x)  = error ("Unable to parse fasta file\n" ++ show x)

-- | Parse a CLIP fasta file into lazy text sequences
parseCLIPFasta :: T.Text -> CloneMap
parseCLIPFasta = M.fromList
               . map (\(!x, (!y, !z)) -> ((x, y), z))
               . zip [0..]
               . eToV
               . parse fastaCLIPFile "error"
  where
    eToV (Right x) = x
    eToV (Left x)  = error ("Unable to parse fasta file\n" ++ show x)

-- | Remove Ns from a collection of sequences
removeNs :: [FastaSequence] -> [FastaSequence]
removeNs = map (\x -> x { fastaSeq = noN . fastaSeq $ x })
  where
    noN = T.map (\y -> if (y /= 'N' && y /= 'n') then y else '-')

-- | Remove Ns from a sequence
removeN :: FastaSequence -> FastaSequence
removeN x = x { fastaSeq = noN . fastaSeq $ x }
  where
    noN = T.map (\y -> if (y /= 'N' && y /= 'n') then y else '-')

-- | Remove Ns from a collection of CLIP fasta sequences
removeCLIPNs :: CloneMap -> CloneMap
removeCLIPNs = M.fromList . map remove . M.toList
  where
    remove   ((!x, !y), !z)    = ((x, newSeq y), map newSeq z)
    newSeq !x = x { fastaSeq = noN . fastaSeq $ x }
    noN = T.map (\y -> if (y /= 'N' && y /= 'n') then y else '-')
