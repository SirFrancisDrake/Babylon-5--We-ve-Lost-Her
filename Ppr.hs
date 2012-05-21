
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Ppr where

import Control.Monad (join)
import Data.Function (on)
import Data.List (intersperse, sort, sortBy)

import Stock
import Wrappers

class Ppr a where
    pprShow :: a -> String
    pprPrint :: a -> IO ()

    pprPrint = putStrLn . pprShow

deShowple (a,b,c,d,e) = [show a, show b, show c, show d, show e]

instance Ppr Stock where
    pprShow stock = pprShow $ 
        [ "Ware"
        , "Amount"
        , "Buying price"
        , "Selling price"
        , "Desired amount"
        ]: (map deShowple stock)

instance Ppr [[String]] where
    pprShow matr
     | not . and $ map (\t -> length t == length (head matr)) matr =
        let errmsg = "Matrix ppr from Ppr.hs: that ain't exactly a matrix"
        in error errmsg
     | otherwise =
        let makeLength ln = (take ln) . ( flip (++) (repeat ' ') )
            uniLn = length . head $ matr
            lnth x matr = (last . sort $ map (length . (!! x)) matr) + 1
            lns = map (\x -> lnth x matr) [0..(length (head matr) - 1)]
            joinStr strs = join $ intersperse " | " strs
            header = joinStr $ zipWith makeLength lns (head matr)
            breaker = "\n\t" ++ (take (length header) $ repeat '-')
            newMatr = [""]: (sortBy ( on compare (!! 0) ) (tail matr))
            makeMatrix = join $ intersperse "\n\t" 
                                    (map (joinStr . (zipWith makeLength lns)) 
                                         newMatr)
        in "\n\t" ++ header ++ breaker ++ makeMatrix ++ "\n"
