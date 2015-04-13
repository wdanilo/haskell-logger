{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Log.Format
-- Copyright   :  (C) 2015 Flowbox
-- License     :  Apache-2.0
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module System.Log.Format where

import System.Log.Log               (Log)
import System.Log.Data              (Lvl(Lvl), Msg(Msg), Loc(Loc), Time(Time), LocData(LocData), LevelData(LevelData), readData, DataOf, Lookup)
import Data.Time.Clock              (UTCTime)
import Data.Time.Format             (formatTime)
import Data.Time.Locale.Compat      (defaultTimeLocale)
import Text.PrettyPrint.ANSI.Leijen



----------------------------------------------------------------------
-- Formatter
----------------------------------------------------------------------

newtype Formatter a = Formatter { runFormatter :: Log a -> Doc }

mapFormatter f (Formatter a) = Formatter (f a)


instance Show (Formatter a) where
    show _ = "Formatter"


----------------------------------------------------------------------
-- FormatBuilder
----------------------------------------------------------------------

class FormatterBuilder a b where
    buildFormatter :: a -> Formatter b


(<:>) :: (FormatterBuilder a c, FormatterBuilder b c) => a -> b -> Formatter c
(<:>) a b = concatFormatters (buildFormatter a) (buildFormatter b)

concatFormatters :: Formatter a -> Formatter a -> Formatter a
concatFormatters (Formatter f) (Formatter g) = Formatter (\s -> f s <> g s)

-- === Instances ===

instance (PPrint (DataOf seg), Lookup seg (Log a)) => FormatterBuilder seg a where
    buildFormatter a = Formatter $ pprint . readData a

instance (a~b) => FormatterBuilder (Formatter a) b where
    buildFormatter = id

instance FormatterBuilder String a where
    buildFormatter a = Formatter $ const (text a)

instance FormatterBuilder Doc a where
    buildFormatter a = Formatter $ const a

----------------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------------

class PPrint a where
    pprint :: a -> Doc

instance PPrint String where
    pprint = text

instance Pretty a => PPrint a where
    pprint = pretty

instance Pretty LevelData where
    pretty (LevelData _ name) = text name

instance Pretty LocData where
    pretty (LocData _ _ m (l,_) _) = text (m ++ ":" ++ show l)

instance Pretty UTCTime where
    pretty = text . formatTime defaultTimeLocale "%c"

----------------------------------------------------------------------
-- Basic formatters
----------------------------------------------------------------------

defaultFormatter     = colorLvlFormatter ("[" <:> Lvl <:> "] ") <:> Msg
defaultTimeFormatter = colorLvlFormatter ("[" <:> Lvl <:> "] ") <:> Time <:> ": " <:> Msg
defaultFormatterTH   = colorLvlFormatter ("[" <:> Lvl <:> "] ") <:> Loc <:> ": " <:> Msg

-- Color formatter

colorLvlFormatter f = Formatter (\s -> let (LevelData pr _) = readData Lvl s in lvlColor pr $ runFormatter f s)

lvlColor lvl
    | lvl == 0  = id
    | lvl <= 2  = green
    | lvl == 3  = yellow
    | otherwise = red

