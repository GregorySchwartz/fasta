-- Parse module.
-- By G.W. Schwartz
--
{- | Collection of functions for the parsing of a fasta file. Uses the Text
type.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Data.Fasta.Text.Parse ( parsecFasta
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
import Text.Parsec.Text
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Control.Applicative as CA
import Control.Monad (void)

-- Cabal
import qualified Data.Attoparsec.Text as A
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Text as PT
import qualified Pipes.Group as PG
import qualified Pipes.Attoparsec as PA
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
                          , fastaSeq = T.pack . removeWhitespace $ fseq } )
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

-- | Parse a standard fasta file
parsecFasta :: T.Text -> [FastaSequence]
parsecFasta = eToV . parse fastaFile "error"
  where
    eToV (Right x) = x
    eToV (Left x)  = error ("Unable to parse fasta file\n" ++ show x)

-- | Parse a CLIP fasta file
parsecCLIPFasta :: T.Text -> CloneMap
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
    x <- A.letter
    A.skipSpace
    return x

-- | attoparsec parser for a fasta type
fasta' :: A.Parser FastaSequence
fasta' = do
    header <- A.takeWhile (not . A.isEndOfLine)
    A.endOfLine
    fseq <- A.manyTill anyButSpace (void (A.char '>') CA.<|> A.endOfInput)
    return FastaSequence { fastaHeader = header
                         , fastaSeq = T.pack fseq }

-- | attoparsec parser for a fasta file
fastaFile' :: A.Parser [FastaSequence]
fastaFile' = do
    A.skipSpace
    A.char '>'
    A.many' fasta'

-- | attoparsec parser for a CLIP fasta sequence
fastaCLIP' :: A.Parser FastaSequence
fastaCLIP' = do
    header <- A.takeWhile (not . A.isEndOfLine)
    A.endOfLine
    fseq <- A.manyTill anyButSpace (void (A.char '>') CA.<|> A.endOfInput)
    return FastaSequence { fastaHeader = header
                         , fastaSeq = T.pack fseq }

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
attoFasta :: T.Text -> [FastaSequence]
attoFasta = eToV . A.parseOnly fastaFile'
  where
    eToV (Right x) = x
    eToV (Left x)  = error ("Unable to parse fasta file\n" ++ show x)

-- | Parse a CLIP fasta file
attoCLIPFasta :: T.Text -> [(Germline, [FastaSequence])]
attoCLIPFasta = eToV . A.parseOnly fastaCLIPFile'
  where
    eToV (Right x) = x
    eToV (Left x)  = error ("Unable to parse fasta file\n" ++ show x)

-- | Parse a standard fasta file into a pipe
pipesFasta :: (MonadIO m) => Producer T.Text m () -> Producer FastaSequence m ()
pipesFasta p = FL.purely PG.folds FL.mconcat ( view (PT.splits '>')
                                             . PT.drop (1 :: Int)
                                             $ p )
           >-> P.map toFasta
  where
    toFasta x = FastaSequence { fastaHeader = head . T.lines $ x
                              , fastaSeq    = T.concat . tail . T.lines $ x }

-- | Parse a CLIP fasta file into a pipe
pipesCLIPFasta :: (MonadIO m)
               => Producer T.Text m ()
               -> Producer (Germline, [FastaSequence]) m (Either (PA.ParsingError, Producer T.Text m ()) ())
pipesCLIPFasta = PA.parsed clone' . PT.drop 2 . (>-> PT.stripStart)

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
