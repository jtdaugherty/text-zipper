module Data.Text.Zipper.Vector
    ( vecLines
    )
where

import qualified Data.Vector as V

vecLines :: V.Vector Char -> [V.Vector Char]
vecLines v | V.null v  = []
           | otherwise = case V.elemIndex '\n' v of
               Nothing -> [v]
               Just i -> let (h, t) = V.splitAt i v
                         in h : vecLines t

