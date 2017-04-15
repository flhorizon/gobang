module Main
(
    main
) where

import Lib (Grid, makeGrid)

-- import qualified System.Environment as Env

main :: IO ()
main = do
    let grid = makeGrid 19 19
        in print grid
    return ()

