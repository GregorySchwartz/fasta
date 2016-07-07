-- Utility Module
-- By Gregory W. Schwartz

{- | Collects all helpful random functions.
-}

module Data.Fasta.Utility ( complRules
                          ) where

-- Built in

-- Cabal

-- Local

-- | Defines the rules for complement. Unknown characters are left
-- untouched.
complRules :: Char -> Char
complRules 'A' = 'T'
complRules 'C' = 'G'
complRules 'G' = 'C'
complRules 'T' = 'A'
complRules 'a' = 't'
complRules 'c' = 'g'
complRules 'g' = 'c'
complRules 't' = 'a'
complRules x   = x
