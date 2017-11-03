module Data.Binary.Extra.Endian where

data Endian = L | B
  deriving (Eq, Ord, Show)

endian :: r -> r -> Endian -> r
endian r _ L = r
endian _ r B = r
