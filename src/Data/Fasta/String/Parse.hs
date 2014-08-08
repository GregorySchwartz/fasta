-- Parse module.
-- By G.W. Schwartz
--
-- | Collection of functions for the parsing of a fasta file. Uses the string
-- type.

{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Fasta.String.Parse (parseFasta, parseCLIPFasta, removeNs) where

-- Built-in
import Text.Parsec
import Control.Monad (void)

-- Local
import Data.Fasta.String

eol :: Parsec String u  String
eol = choice . map (try . string) $ ["\n\r", "\r\n", "\n", "\r"]

eoe :: Parsec String u ()
eoe  = do
    lookAhead (void $ char '>') <|> eof

fasta :: Parsec String u FastaSequence
fasta = do
    spaces
    char '>'
    info <- manyTill (satisfy (/= '>')) eol
    fseq <- manyTill anyChar eoe
    return (FastaSequence {fastaInfo = info, fastaSeq = removeWhitespace fseq})
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

parseFasta :: String -> [FastaSequence]
parseFasta = eToV . parse fastaFile "error"
  where
    eToV (Right x) = x
    eToV (Left x)  = error ("Unable to parse fasta file\n" ++ show x)

parseCLIPFasta :: String -> [((Int, FastaSequence), [FastaSequence])]
parseCLIPFasta = map (\(x, (y, z)) -> ((x, y), z))
               . zip [0..]
               . eToV
               . parse fastaCLIPFile "error"
  where
    eToV (Right x) = x
    eToV (Left x)  = error ("Unable to parse fasta file\n" ++ show x)

removeNs :: [FastaSequence] -> [FastaSequence]
removeNs = map (\x -> x { fastaSeq = noN . fastaSeq $ x })
  where
    noN = map (\y -> if (y /= 'N' && y /= 'n') then y else '-')
