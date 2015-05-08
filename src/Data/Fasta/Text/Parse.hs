-- Parse module.
-- By G.W. Schwartz
--
{- | Collection of functions for the parsing of a fasta file. Uses the Text
type.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Data.Fasta.Text.Parse ( parseFasta
                             , parseCLIPFasta
                             , pipesFasta
                             , removeNs
                             , removeN
                             , removeCLIPNs ) where

-- Built-in
import Data.Char
import Text.Parsec
import Text.Parsec.Text
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

-- Cabal
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Text as PT
import qualified Pipes.Group as PG
import Control.Lens (view)
import qualified Control.Foldl as FL

-- Local
import Data.Fasta.Text.Types

eol :: Parsec T.Text u String
eol = choice . map (try . string) $ ["\n\r", "\r\n", "\n", "\r"]

eoe :: Parsec T.Text u ()
eoe = lookAhead (void $ char '>') <|> eof

fasta :: Parsec T.Text u FastaSequence
fasta = do
    spaces
    char '>'
    header <- manyTill (satisfy (/= '>')) eol
    fseq <- manyTill anyChar eoe
    return (FastaSequence { fastaHeader = T.pack header
                          , fastaSeq = T.pack
                                     . map toUpper
                                     . removeWhitespace
                                     $ fseq } )
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

-- | Parse a standard fasta file into text sequences
parseFasta :: T.Text -> [FastaSequence]
parseFasta = eToV . parse fastaFile "error"
  where
    eToV (Right x) = x
    eToV (Left x)  = error ("Unable to parse fasta file\n" ++ show x)

-- | Parse a CLIP fasta file into text sequences
parseCLIPFasta :: T.Text -> CloneMap
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
pipesFasta :: (MonadIO m) => Producer T.Text m () -> Producer FastaSequence m ()
pipesFasta p = FL.purely PG.folds FL.mconcat ( view (PT.splits '>')
                                             . PT.drop (1 :: Int)
                                             $ p )
           >-> P.map toFasta
  where
    toFasta x = FastaSequence { fastaHeader = head . T.lines $ x
                              , fastaSeq    = T.concat . tail . T.lines $ x }

-- | Remove Ns from a collection of sequences
removeNs :: [FastaSequence] -> [FastaSequence]
removeNs = map (\x -> x { fastaSeq = noN . fastaSeq $ x })
  where
    noN = T.map (\y -> if y /= 'N' && y /= 'n' then y else '-')

-- | Remove Ns from a sequence
removeN :: FastaSequence -> FastaSequence
removeN x = x { fastaSeq = noN . fastaSeq $ x }
  where
    noN = T.map (\y -> if y /= 'N' && y /= 'n' then y else '-')

-- | Remove Ns from a collection of CLIP fasta sequences
removeCLIPNs :: CloneMap -> CloneMap
removeCLIPNs = Map.fromList . map remove . Map.toList
  where
    remove   ((!x, !y), !z)    = ((x, newSeq y), map newSeq z)
    newSeq !x = x { fastaSeq = noN . fastaSeq $ x }
    noN = T.map (\y -> if y /= 'N' && y /= 'n' then y else '-')
