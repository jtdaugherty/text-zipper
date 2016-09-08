module Data.Text.Zipper.Vector where

import           Prelude hiding ( lines )
import qualified Data.Vector as V

lines :: V.Vector Char -> [V.Vector Char]
lines v | V.null v  = []
        | otherwise = case V.elemIndex '\n' v of
            Nothing -> [v]
            Just i -> let (h, t) = V.splitAt i v
                      in h : lines t

