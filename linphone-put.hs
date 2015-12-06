#!/usr/bin/runhaskell
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString.Lazy as B hiding (zip, map, pack)
import Data.ByteString.Lazy.Char8 as BC8
import Data.List
import Data.Map hiding (map)
import Prelude as P hiding (interact)
import Text.VCard.Format.Directory
import Text.VCard.Query

gv n v = BC8.unpack (printValue (lookup' n v))

linphonify :: (Int, VCard) -> ByteString
--linphonify (n, (VCard _ map)) = pack $ "[friend_" ++ show n ++ "]\n" ++
linphonify (n, vc) = pack $ "[friend_" ++ show n ++ "]\n" ++
	"url=\"" ++ gv "fn" vc ++ "\" <sip:" ++ gv "tel" vc ++ "@multifon.ru>\n" ++
	"pol=accept\n" ++
	"subscribe=1\n\n"

main = interact $ \x -> do
	B.concat $ P.map linphonify $ P.zip [0..] $ sortOn (gv "fn") $ readVCards "stdin" x
