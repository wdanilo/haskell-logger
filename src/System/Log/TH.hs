{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Log.Simple
-- Copyright   :  (C) 2015 Flowbox
-- License     :  Apache-2.0
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module System.Log.TH (
	module System.Log.TH,
	module X
)where

import System.Log.Level           as X
import System.Log.Log             as X
import System.Log.Data            as X
import System.Log.Format          as X
import System.Log.Logger.Base     as X
import System.Log.Logger.Handler  as X
import System.Log.Logger.Priority as X
import System.Log.Logger.Thread   as X
import System.Log.Logger.Drop     as X
import System.Log.Filter          as X
import System.Log.Logger.Writer   as X

import Language.Haskell.TH

getLoc = do
    loc <- location
    let locCons = TupE [ LitE . StringL . loc_filename $ loc
                       , LitE . StringL . loc_package  $ loc
                       , LitE . StringL . loc_module   $ loc
                       , TupE [ LitE . IntegerL . toInteger . fst . loc_start $ loc
                              , LitE . IntegerL . toInteger . snd . loc_start $ loc
                              ]
                       , TupE [ LitE . IntegerL . toInteger . fst . loc_end $ loc
                              , LitE . IntegerL . toInteger . snd . loc_end $ loc
                              ]
                       ]

    return $ AppE (VarE $ mkName "mkLoc") locCons

logN     = VarE (mkName "log")
emptyN   = VarE (mkName "empty")
appDataN = VarE (mkName "appData")
locN     = ConE (mkName "Loc")

debugN    = ConE (mkName "Debug")
infoN     = ConE (mkName "Info")
noticeN   = ConE (mkName "Notice")
warningN  = ConE (mkName "Warning")
errorN    = ConE (mkName "Error")
criticalN = ConE (mkName "Critical")
alertN    = ConE (mkName "Alert")
panicN    = ConE (mkName "Panic")

debug    = mkLog debugN
info     = mkLog infoN
notice   = mkLog noticeN
warning  = mkLog warningN
error    = mkLog errorN
critical = mkLog criticalN
alert    = mkLog alertN
panic    = mkLog panicN

mkLog eName msg = do
	d <- mkBaseData
	return $ AppE (AppE (AppE logN d) eName) (LitE $ StringL msg)

mkBaseData = do
	loc <- getLoc
	return $ AppE (AppE (AppE appDataN locN) loc) emptyN

--debug     = simpleLog Debug
--info      = simpleLog Info
--notice    = simpleLog Notice
--warning   = simpleLog Warning
--error     = simpleLog Error
--critical  = simpleLog Critical
--alert     = simpleLog Alert
--panic     = simpleLog Panic

--locatedError :: Loc -> Q Exp
--locatedError loc = do
--    let postfix = " at " ++ formatLoc loc
--    [| \msg -> error (msg ++ $(litE $ stringL postfix)) |]

--formatLoc :: Loc -> String
--formatLoc loc = let file = loc_filename loc
--                    (line, col) = loc_start loc
--                in concat [file, ":", show line, ":", show col]


