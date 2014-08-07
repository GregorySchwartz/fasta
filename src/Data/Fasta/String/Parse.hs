-- Parse module.
-- By G.W. Schwartz
--
-- | Collection of functions for the parsing of a fasta file. Uses the string
-- type.

{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Fasta.String.Parse (parseFasta, removeNs) where

-- Built-in
import Text.Parsec

-- Local
import Data.Fasta.String.Types

eol  = choice . map (try . string) $ ["\n\r", "\r\n", "\n", "\r"]

eoe  = do
    try (eof >> return '>') <|> (char '>')

entry = do
    spaces
    info <- manyTill anyChar eol
    fseq <- manyTill anyChar (lookAhead eoe)
    return (FastaSequence { fastaInfo = info, fastaSeq = removeWhitespace fseq })
  where
    removeWhitespace = filter (\x -> not . elem x $ "\n\r ")

fasta = do
    spaces
    char '>'
    endBy entry eoe

parseFasta :: String -> [FastaSequence]
parseFasta = eToV . parse fasta "error"
  where
    eToV (Right x) = x
    eToV (Left _)  = error "Unable to parse fasta file"

removeNs :: [FastaSequence] -> [FastaSequence]
removeNs = map (\x -> x { fastaSeq = noN . fastaSeq $ x })
  where
    noN = map (\y -> if (y /= 'N' && y /= 'n') then y else '-')
