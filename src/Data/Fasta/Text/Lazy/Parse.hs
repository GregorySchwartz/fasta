-- Parse module.
-- By G.W. Schwartz
--
-- | Collection of functions for the parsing of a fasta file. Uses the Text.Lazy
-- type.

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Fasta.Text.Lazy.Parse (parseFasta, removeNs) where

-- Built-in
import Text.Parsec
import Text.Parsec.Text.Lazy
import qualified Data.Text.Lazy as T

-- Local
import Data.Fasta.Text.Lazy

eol  = choice . map (try . string) $ ["\n\r", "\r\n", "\n", "\r"]

eoe  = do
    try (eof >> return '>') <|> (char '>')

entry = do
    spaces
    info <- manyTill anyChar eol
    fseq <- manyTill anyChar (lookAhead eoe)
    return ( FastaSequence { fastaInfo = T.pack info
                           , fastaSeq = T.pack . removeWhitespace $ fseq } )
  where
    removeWhitespace = filter (\x -> not . elem x $ "\n\r ")

fasta = do
    spaces
    char '>'
    endBy entry eoe

parseFasta :: T.Text -> [FastaSequence]
parseFasta = eToV . parse fasta "error"
  where
    eToV (Right x) = x
    eToV (Left _)  = error "Unable to parse fasta file"

removeNs :: [FastaSequence] -> [FastaSequence]
removeNs = map (\x -> x { fastaSeq = noN . fastaSeq $ x })
  where
    noN = T.map (\y -> if (y /= 'N' && y /= 'n') then y else '-')
