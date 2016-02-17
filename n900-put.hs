#!/usr/bin/runhaskell
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString.Lazy as B hiding (zip, map, pack)
import Data.ByteString.Lazy.Char8 as BC8
import Data.List
import Data.Map hiding (map)
import Prelude as P hiding (interact)
import Text.VCard.Format.Directory
import Text.VCard.Query as Q

gv n v = BC8.unpack (printValue (maybe (error ("No field " ++ show n ++ " in vcard " ++ show v)) P.head (Q.lookup n v)))

n900ify :: (Integer, VCard) -> VCard
n900ify (n, vc) = Q.insert (Prop (Type Nothing "UID") [] (Integer n)) vc

main = interact $ \x -> do
	BC8.unlines $ P.map (writeVCard . n900ify) $ P.zip [0..] $ sortOn (gv "fn") $ readVCards "stdin" x
