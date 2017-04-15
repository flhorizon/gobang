{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Using dirty template Haskell to automagically instantiate
-- Unboxed Vector/MVector for our custom Ownership type
-- with Word8 primitive representation.

module Data.Ownership (
        Ownership (Nil, Black, White),
        flipOwner,
        owned
    ) where

import Data.Default
import Data.Vector.Unboxed.Deriving
import Data.Word

data Ownership = Nil | Black | White
    deriving (Enum, Bounded, Ord, Eq, Show, Read)

instance Default Ownership where
    def = Nil

ownershipToWord8 :: Ownership -> Word8
ownershipToWord8 pos = case pos of  Nil -> 0
                                    Black -> 1
                                    White -> 2
{-# INLINE ownershipToWord8 #-}

word8ToOwnership :: Word8 -> Ownership
word8ToOwnership c  | c == 0 = Nil
                    | c == 1 = Black
                    | c > 1 = White
{-# INLINE word8ToOwnership #-}

derivingUnbox "Ownership"
    [t| Ownership -> Word8 |]
    [| ownershipToWord8 |]
    [| word8ToOwnership |]


flipOwner :: Ownership -> Ownership
flipOwner Nil = error "Nothing to flip"
flipOwner pos = case pos `compare` Black of GT -> Black
                                            _ -> White
{-# INLINE flipOwner #-}

owned :: Ownership -> Bool
owned = (/=) Nil
{-# INLINE owned #-}
