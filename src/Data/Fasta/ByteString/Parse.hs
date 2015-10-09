-- Parse module.
-- By Gregory W. Schwartz
--
{- | Collection of functions for the parsing of a fasta file. Uses the
- ByteString type.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Data.Fasta.ByteString.Parse ( parsecFasta
                                   , parsecCLIPFasta
                                   , attoFasta
                                   , attoCLIPFasta
                                   , pipesFasta
                                   , pipesCLIPFasta
                                   , removeNs
                                   , removeN
                                   , removeCLIPNs ) where

-- Built-in
import Data.Char
import Text.Parsec
import Text.Parsec.ByteString
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BW
import qualified Data.ByteString.Char8 as B
import qualified Control.Applicative as CA
import Control.Monad (void)

-- Cabal
import qualified Data.Attoparsec.ByteString.Char8 as A
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB
import qualified Pipes.Group as PG
import qualified Pipes.Attoparsec as PA
import Control.Lens (view)
import qualified Control.Foldl as FL

-- Local
import Data.Fasta.ByteString.Types

eol :: Parsec B.ByteString u String
eol = choice . map (try . string) $ ["\n\r", "\r\n", "\n", "\r"]

eoe :: Parsec B.ByteString u ()
eoe = lookAhead (void $ char '>') <|> eof

fasta :: Parsec B.ByteString u FastaSequence
fasta = do
    spaces
    char '>'
    header <- manyTill (satisfy (/= '>')) eol
    fseq <- manyTill anyChar eoe
    return (FastaSequence { fastaHeader = B.pack header
                          , fastaSeq = B.pack . removeWhitespace $ fseq } )
  where
    removeWhitespace = filter (`notElem` ("\n\r " :: String))

fastaFile :: Parsec B.ByteString u [FastaSequence]
fastaFile = do
    spaces
    many fasta

fastaCLIP :: Parsec B.ByteString u (FastaSequence, [FastaSequence])
fastaCLIP = do
    spaces
    char '>'
    germline <- fasta
    clones <- many $ try fasta
    return (germline, clones)

fastaCLIPFile :: Parsec B.ByteString u [(FastaSequence, [FastaSequence])]
fastaCLIPFile = do
    spaces
    many fastaCLIP

-- | Parse a standard fasta file into text sequences
parsecFasta :: B.ByteString -> [FastaSequence]
parsecFasta = eToV . parse fastaFile "error"
  where
    eToV (Right x) = x
    eToV (Left x)  = error ("Unable to parse fasta file\n" ++ show x)

-- | Parse a CLIP fasta file into text sequences
parsecCLIPFasta :: B.ByteString -> CloneMap
parsecCLIPFasta = Map.fromList
                . map (\(!x, (!y, !z)) -> ((x, y), z))
                . zip [0..]
                . eToV
                . parse fastaCLIPFile "error"
  where
    eToV (Right x) = x
    eToV (Left x)  = error ("Unable to parse fasta file\n" ++ show x)

-- | attopares any char but space
anyButSpace :: A.Parser Char
anyButSpace = do
    A.skipSpace
    x <- A.letter_ascii
    A.skipSpace
    return x

-- | attoparsec parser for a fasta type
fasta' :: A.Parser FastaSequence
fasta' = do
    header <- A.takeWhile (\x -> x /= '\n' && x /= '\r')
    A.endOfLine
    fseq <- A.manyTill anyButSpace (void (A.char '>') CA.<|> A.endOfInput)
    return FastaSequence { fastaHeader = header
                         , fastaSeq = B.pack fseq }

-- | attoparsec parser for a fasta file
fastaFile' :: A.Parser [FastaSequence]
fastaFile' = do
    A.skipSpace
    A.char '>'
    A.many' fasta'

-- | attoparsec parser for a CLIP fasta sequence
fastaCLIP' :: A.Parser FastaSequence
fastaCLIP' = do
    header <- A.takeWhile (\x -> x /= '\n' && x /= '\r')
    A.endOfLine
    fseq <- A.manyTill anyButSpace (void (A.char '>') CA.<|> A.endOfInput)
    return FastaSequence { fastaHeader = header
                         , fastaSeq = B.pack fseq }

clone' :: A.Parser (Germline, [FastaSequence])
clone' = do
    A.skipSpace
    germline <- fastaCLIP'
    fseqs <- A.manyTill fasta' (void (A.char '>') CA.<|> A.endOfInput)
    return (germline, fseqs)

-- | attoparsec parser for a fasta file
fastaCLIPFile' :: A.Parser [(Germline, [FastaSequence])]
fastaCLIPFile' = do
    A.skipSpace
    A.string ">>"
    A.many' clone'

-- | Parse a standard fasta file
attoFasta :: B.ByteString -> [FastaSequence]
attoFasta = eToV . A.parseOnly fastaFile'
  where
    eToV (Right x) = x
    eToV (Left x)  = error ("Unable to parse fasta file\n" ++ show x)

-- | Parse a CLIP fasta file into text sequences
attoCLIPFasta :: B.ByteString -> [(Germline, [FastaSequence])]
attoCLIPFasta = eToV . A.parseOnly fastaCLIPFile'
  where
    eToV (Right x) = x
    eToV (Left x)  = error ("Unable to parse fasta file\n" ++ show x)

-- | Parse a standard fasta file into strict text sequences for pipes. This is
-- the highly recommeded way of parsing, as it is computationally fast and
-- uses memory based on line length
pipesFasta :: (MonadIO m) => Producer B.ByteString m ()
                          -> Producer FastaSequence m ()
pipesFasta p = FL.purely
               PG.folds
               FL.mconcat
               ( view (PB.splits (fromIntegral $ ord '>'))
               . PB.drop (1 :: Int)
               $ p )
           >-> P.map toFasta
  where
    toFasta x = FastaSequence { fastaHeader = head . B.lines $ x
                              , fastaSeq    = B.concat . tail . B.lines $ x }

-- | Parse a CLIP fasta file into strict text sequences for pipes.
pipesCLIPFasta :: (MonadIO m)
               => Producer B.ByteString m ()
               -> Producer (Germline, [FastaSequence]) m (Either (PA.ParsingError, Producer B.ByteString m ()) ())
pipesCLIPFasta = PA.parsed clone'
               . PB.drop 2
               . PB.dropWhile (`BW.elem` "\n\r\t ")

-- | Remove Ns from a collection of sequences
removeNs :: [FastaSequence] -> [FastaSequence]
removeNs = map (\x -> x { fastaSeq = noN . fastaSeq $ x })
  where
    noN = B.map (\y -> if y /= 'N' && y /= 'n' then y else '-')

-- | Remove Ns from a sequence
removeN :: FastaSequence -> FastaSequence
removeN x = x { fastaSeq = noN . fastaSeq $ x }
  where
    noN = B.map (\y -> if y /= 'N' && y /= 'n' then y else '-')

-- | Remove Ns from a collection of CLIP fasta sequences
removeCLIPNs :: CloneMap -> CloneMap
removeCLIPNs = Map.fromList . map remove . Map.toList
  where
    remove   ((!x, !y), !z)    = ((x, newSeq y), map newSeq z)
    newSeq !x = x { fastaSeq = noN . fastaSeq $ x }
    noN = B.map (\y -> if y /= 'N' && y /= 'n' then y else '-')
