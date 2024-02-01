{-| 
Module: DataTypes
Description: This module provides essential data types used across the program.
-}

module DataTypes (module DataTypes) where

data LengthU = Km | Hm | Dam | M | Dm | Cm | Mm | Ft | Mi | Yd deriving Show

data WeightU = T | Kg | G | Mg | Lb deriving Show

data TemperatureU = C | F | K deriving Show

data SpeedU = Mps | Kmph | Miph | Cmps | Mips deriving Show

data TimeU = S | Min | H | D | W | Mo | Y deriving Show

data AreaU = Yd2 | Cm2 | Mi2 | Ft2 | Mm2 | Km2 | M2 deriving Show

data VolumeU = Km3 | M3 | Dm3 | Mm3 | Ft3 | Yd3 | Gal | In3 | L deriving Show

data EnergyU = J | Kj | Kwh | Cal | Kcal deriving Show

data ByteU = B | Kb | Mb | Gb | Tb deriving Show

type Value = Double