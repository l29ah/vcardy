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

linphonify :: (Int, VCard) -> ByteString
--linphonify (n, (VCard _ map)) = pack $ "[friend_" ++ show n ++ "]\n" ++
linphonify (n, vc) = pack $ "[friend_" ++ show n ++ "]\n" ++
	"url=\"" ++ gv "fn" vc ++ "\" <sip:" ++ P.tail (gv "tel" vc) ++ "@multifon.ru>\n" ++
	"pol=accept\n" ++
	"subscribe=1\n\n"

main = interact $ \x -> do
	B.concat $ P.map linphonify $ P.zip [0..] $ sortOn (gv "fn") $ readVCards "stdin" x
