-- Parse module.
-- By Gregory W. Schwartz
--
{- | Collection of functions for the parsing of a fasta file. Uses the lazy
- ByteString type.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Data.Fasta.ByteString.Lazy.Parse ( parseFasta
                                        , parseCLIPFasta
                                        , pipesFasta
                                        , removeNs
                                        , removeN
                                        , removeCLIPNs ) where

-- Built-in
import Data.Char
import Control.Monad (void)
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy.Char8 as B

-- Cabal
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB
import qualified Pipes.Group as PG
import Control.Lens (view)
import qualified Control.Foldl as FL

-- Local
import Data.Fasta.ByteString.Lazy.Types

eol :: Parsec B.ByteString u String
eol = choice . map (try . string) $ ["\n\r", "\r\n", "\n", "\r"]

eoe :: Parsec B.ByteString u ()
eoe  = do
    lookAhead (void $ char '>') <|> eof

fasta :: Parsec B.ByteString u FastaSequence
fasta = do
    spaces
    char '>'
    header <- manyTill (satisfy (/= '>')) eol
    fseq <- manyTill anyChar eoe
    return (FastaSequence { fastaHeader = B.pack header
                          , fastaSeq    = B.pack . removeWhitespace $ fseq } )
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

-- | Parse a standard fasta file into lazy text sequences
parseFasta :: B.ByteString -> [FastaSequence]
parseFasta = eToV . parse fastaFile "error"
  where
    eToV (Right x) = x
    eToV (Left x)  = error ("Unable to parse fasta file\n" ++ show x)

-- | Parse a CLIP fasta file into lazy text sequences
parseCLIPFasta :: B.ByteString -> CloneMap
parseCLIPFasta = Map.fromList
               . map (\(!x, (!y, !z)) -> ((x, y), z))
               . zip [0..]
               . eToV
               . parse fastaCLIPFile "error"
  where
    eToV (Right x) = x
    eToV (Left x)  = error ("Unable to parse fasta file\n" ++ show x)

-- | Parse a standard fasta file into strict text sequences for pipes. This is
-- the highly recommeded way of parsing, as it is computationally fast and
-- uses memory based on line length
pipesFasta :: (MonadIO m)
           => Producer SB.ByteString m ()
           -> Producer FastaSequence m ()
pipesFasta p = FL.purely
               PG.folds
               FL.mconcat
               ( view (PB.splits (fromIntegral $ ord '>'))
               . PB.drop (1 :: Int)
               $ p )
           >-> P.map toFasta
  where
    toFasta x = FastaSequence { fastaHeader = B.fromChunks
                                            . take 1
                                            . SB.lines
                                            $ x
                              , fastaSeq    = B.fromChunks
                                            . tail
                                            . SB.lines
                                            $ x }

-- | Remove Ns from a collection of sequences
removeNs :: [FastaSequence] -> [FastaSequence]
removeNs = map (\x -> x { fastaSeq = noN . fastaSeq $ x })
  where
    noN = B.map (\y -> if (y /= 'N' && y /= 'n') then y else '-')

-- | Remove Ns from a sequence
removeN :: FastaSequence -> FastaSequence
removeN x = x { fastaSeq = noN . fastaSeq $ x }
  where
    noN = B.map (\y -> if (y /= 'N' && y /= 'n') then y else '-')

-- | Remove Ns from a collection of CLIP fasta sequences
removeCLIPNs :: CloneMap -> CloneMap
removeCLIPNs = Map.fromList . map remove . Map.toList
  where
    remove   ((!x, !y), !z)    = ((x, newSeq y), map newSeq z)
    newSeq !x = x { fastaSeq = noN . fastaSeq $ x }
    noN = B.map (\y -> if (y /= 'N' && y /= 'n') then y else '-')
