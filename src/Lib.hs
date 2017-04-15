{-# LANGUAGE TypeOperators #-}

module Lib
    (
        Grid,
        makeGrid,
        gridFrom
    ) where

import Data.Array.Repa
import Data.Array.Repa.Index (DIM2, ix2)
import Data.Ownership

type Grid a = Array U DIM2 a

makeGrid :: Int -> Int -> Grid Ownership
makeGrid w h = gridFrom w h $ replicate (w * h) Nil
{-# INLINE makeGrid #-}

gridFrom :: Int -> Int -> [Ownership] -> Grid Ownership
gridFrom w h = fromListUnboxed (ix2 w h)
{-# INLINE gridFrom #-}

