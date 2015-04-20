-- Parse module.
-- By G.W. Schwartz
--
{- | Collection of functions for the parsing of a fasta file. Uses the string
type.
-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Fasta.String.Parse ( parseFasta
                               , parseCLIPFasta
                               , removeNs
                               , removeCLIPNs ) where

-- Built-in
import qualified Data.Map as M
import Data.Char
import Text.Parsec
import Control.Monad (void)

-- Local
import Data.Fasta.String.Types

eol :: Parsec String u  String
eol = choice . map (try . string) $ ["\n\r", "\r\n", "\n", "\r"]

eoe :: Parsec String u ()
eoe  = do
    lookAhead (void $ char '>') <|> eof

fasta :: Parsec String u FastaSequence
fasta = do
    spaces
    char '>'
    header <- manyTill (satisfy (/= '>')) eol
    fseq <- manyTill anyChar eoe
    return ( FastaSequence { fastaHeader = header
                           , fastaSeq    = map toUpper
                                         $ removeWhitespace fseq } )
  where
    removeWhitespace = filter (`notElem` "\n\r ")

fastaFile :: Parsec String u [FastaSequence]
fastaFile = do
    spaces
    many fasta

fastaCLIP :: Parsec String u (FastaSequence, [FastaSequence])
fastaCLIP = do
    spaces
    char '>'
    germline <- fasta
    clones <- many $ try fasta
    return (germline, clones)

fastaCLIPFile :: Parsec String u [(FastaSequence, [FastaSequence])]
fastaCLIPFile = do
    spaces
    many fastaCLIP

-- | Parse a standard fasta file into string sequences
parseFasta :: String -> [FastaSequence]
parseFasta = eToV . parse fastaFile "error"
  where
    eToV (Right x) = x
    eToV (Left x)  = error ("Unable to parse fasta file\n" ++ show x)

-- | Parse a CLIP fasta file into string sequences
parseCLIPFasta :: String -> CloneMap
parseCLIPFasta = M.fromList
               . map (\(x, (y, z)) -> ((x, y), z))
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
    noN = map (\y -> if (y /= 'N' && y /= 'n') then y else '-')

-- | Remove Ns from a sequence
removeN :: FastaSequence -> FastaSequence
removeN x = x { fastaSeq = noN . fastaSeq $ x }
  where
    noN = map (\y -> if (y /= 'N' && y /= 'n') then y else '-')

-- | Remove Ns from a collection of CLIP fasta sequences
removeCLIPNs :: CloneMap -> CloneMap
removeCLIPNs = M.fromList . map remove . M.toList
  where
    remove   ((x, y), z)    = ((x, newSeq y), map newSeq z)
    newSeq x = x { fastaSeq = noN . fastaSeq $ x }
    noN = map (\y -> if (y /= 'N' && y /= 'n') then y else '-')
