module Data.UUID.Versions where

data UUIDVersion = V1 | V4 | V5 | V7
  deriving (Eq, Ord, Bounded, Enum)
