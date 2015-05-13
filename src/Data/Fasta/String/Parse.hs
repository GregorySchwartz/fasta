-- Parse module.
-- By G.W. Schwartz
--
{- | Collection of functions for the parsing of a fasta file. Uses the string
type.
-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns #-}

module Data.Fasta.String.Parse ( parseFasta
                               , parseCLIPFasta
                               , pipesFasta
                               , removeNs
                               , removeN
                               , removeCLIPNs ) where

-- Built-in
import Data.Char
import Text.Parsec
import Control.Monad (void)
import qualified Data.Map as Map
import qualified System.IO as IO

-- Cabal
import Pipes

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
                           , fastaSeq    = removeWhitespace fseq } )
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
parseCLIPFasta = Map.fromList
               . map (\(x, (y, z)) -> ((x, y), z))
               . zip [0..]
               . eToV
               . parse fastaCLIPFile "error"
  where
    eToV (Right x) = x
    eToV (Left x)  = error ("Unable to parse fasta file\n" ++ show x)

-- | Parse a standard fasta file into string sequences for pipes. This is
-- the highly recommeded way of parsing, as it is computationally fast and
-- uses constant file memory
pipesFasta :: (MonadIO m) => IO.Handle -> Pipe String FastaSequence m ()
pipesFasta h = do
    first <- await
    getRest first ""
  where
    getRest x !acc = do
        eof <- liftIO $ IO.hIsEOF h
        if eof
            then yield FastaSequence { fastaHeader = tail x
                                     , fastaSeq    = filter
                                                     (`notElem` "\n\r ")
                                                     acc }
            else do
                y <- await
                if take 1 y == ">"
                    then do
                        yield FastaSequence { fastaHeader = tail x
                                            , fastaSeq    = filter
                                                            (`notElem` "\n\r ")
                                                            acc }
                        getRest y ""
                    else getRest x (acc ++ y)

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
removeCLIPNs = Map.fromList . map remove . Map.toList
  where
    remove   ((x, y), z)    = ((x, newSeq y), map newSeq z)
    newSeq x = x { fastaSeq = noN . fastaSeq $ x }
    noN = map (\y -> if (y /= 'N' && y /= 'n') then y else '-')
