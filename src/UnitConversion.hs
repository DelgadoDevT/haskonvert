{-| 
Module: UnitConversion
Description: Implementations of functions to convert measurements between various units.
-}

{-# OPTIONS_GHC -Wno-type-defaults #-}

module UnitConversion
  ( convertLength
  , convertWeigth
  , convertTemperature
  , convertSpeed
  , convertTime
  , convertArea
  , convertVolume
  , convertEnergy
  , convertByte
  ) where 

import DataTypes

round2Cen :: Value -> Value
round2Cen x = fromIntegral (round (x * 100)) / 100

convertLength :: Value -> LengthU -> LengthU -> Value
convertLength v to tn = round2Cen (toLength (toMeter v to) tn)

toMeter :: Value -> LengthU -> Value
toMeter v u =
    case u of 
        Km  -> v * 1000
        Hm  -> v * 100
        Dam -> v * 10
        M   -> v 
        Dm  -> v * 0.1 
        Cm  -> v * 0.01
        Mm  -> v * 0.001
        Ft  -> v * 0.3048   
        Mi  -> v * 1609.344
        Yd  -> v * 0.9144

toLength :: Value -> LengthU -> Value
toLength v u = 
    case u of 
        Km  -> v / 1000
        Hm  -> v / 100
        Dam -> v / 10
        M   -> v 
        Dm  -> v * 10 
        Cm  -> v * 100
        Mm  -> v * 1000
        Ft  -> v / 0.3048   
        Mi  -> v / 1609.344
        Yd  -> v / 0.9144

convertWeigth :: Value -> WeightU -> WeightU -> Value
convertWeigth v to tn = round2Cen (toWeight (toGram v to) tn )

toGram :: Value -> WeightU -> Value
toGram v u = 
    case u of 
        T  -> v * 1e6
        Kg -> v * 1000
        G  -> v 
        Mg -> v / 0.001
        Lb  -> v * 453.592

toWeight :: Value -> WeightU -> Value
toWeight v u = 
    case u of 
        T  -> v / 1e6
        Kg -> v / 1000
        G  -> v 
        Mg -> v * 0.001
        Lb  -> v / 453.592

convertTemperature :: Value -> TemperatureU -> TemperatureU -> Value
convertTemperature v to tn = round2Cen (toTemperature (toCelsius v to) tn)

toCelsius :: Value -> TemperatureU -> Value
toCelsius v u =
    case u of 
        C -> v
        F -> (v - 32) * 5/9
        K -> v - 273.15

toTemperature :: Value -> TemperatureU -> Value
toTemperature v u =
    case u of 
        C -> v
        F -> v * 1.8 + 32
        K -> v + 273.15

convertSpeed :: Value -> SpeedU -> SpeedU -> Value
convertSpeed v to tn = round2Cen (toSpeed (toMps v to) tn)

toMps :: Value -> SpeedU -> Value
toMps v u = 
    case u of 
        Mps  -> v 
        Kmph -> v / 3.6
        Miph -> v / 2.23694
        Cmps -> v * 100
        Mips -> v * 1609.344

toSpeed :: Value -> SpeedU -> Value
toSpeed v u = 
    case u of 
        Mps  -> v 
        Kmph -> v * 3.6
        Miph -> v * 2.23694
        Cmps -> v / 100
        Mips -> v / 1609.344

convertTime :: Value -> TimeU -> TimeU -> Value
convertTime v to tn = round2Cen (toTime (toSec v to) tn)

toSec :: Value -> TimeU -> Value
toSec v u =
    case u of 
        S   -> v 
        Min -> v * 60
        H   -> v * 3600
        D   -> v * 86400
        W   -> v * 604800
        Mo  -> v * 2628000
        Y   -> v * 31536000

toTime :: Value -> TimeU -> Value
toTime v u =
    case u of 
        S   -> v 
        Min -> v / 60
        H   -> v / 3600
        D   -> v / 86400
        W   -> v / 604800
        Mo  -> v / 2628000
        Y   -> v / 31536000

convertArea :: Value -> AreaU -> AreaU -> Value
convertArea v to tn = round2Cen (toArea (toM2 v to) tn)

toM2 :: Value -> AreaU -> Value
toM2 v u =
    case u of 
        M2  -> v
        Yd2 -> v * 0.836127
        Cm2 -> v * 0.0001
        Mi2 -> v * 2589988.110336
        Ft2 -> v * 0.092903
        Mm2 -> v * 1.0e-6
        Km2 -> v * 1.0e6

toArea :: Value -> AreaU -> Value
toArea v u =
    case u of 
        M2  -> v
        Yd2 -> v / 0.836127
        Cm2 -> v / 0.0001
        Mi2 -> v / 2589988.110336
        Ft2 -> v / 0.092903
        Mm2 -> v / 1.0e-6
        Km2 -> v / 1.0e6

convertVolume :: Value -> VolumeU -> VolumeU -> Value
convertVolume v to tn = round2Cen (toVolume (toM3 v to) tn)

toM3 :: Value -> VolumeU -> Value
toM3 v u =
    case u of  
        M3  -> v 
        Km3 -> v * 1.0e9
        Dm3 -> v * 0.001
        Mm3 -> v * 1.0e-9
        Ft3 -> v * 0.0283168
        Yd3 -> v * 0.764555
        Gal -> v * 0.00378541
        In3 -> v * 0.000016387
        L   -> v * 0.001

toVolume :: Value -> VolumeU -> Value
toVolume v u = 
    case u of  
        M3  -> v 
        Km3 -> v / 1.0e9
        Dm3 -> v / 0.001
        Mm3 -> v / 1.0e-9
        Ft3 -> v / 0.0283168
        Yd3 -> v / 0.764555
        Gal -> v / 0.00378541
        In3 -> v / 0.000016387
        L   -> v / 0.001

convertEnergy :: Value -> EnergyU -> EnergyU -> Value
convertEnergy v to tn = round2Cen (toEnergy (toJoule v to) tn)

toJoule :: Value -> EnergyU -> Value
toJoule v u =
    case u of
        J    -> v
        Kj   -> v * 1000
        Kwh  -> v * 3.6e6
        Cal  -> v * 4.184
        Kcal -> v * 4184

toEnergy :: Value -> EnergyU -> Value
toEnergy v u =
    case u of
        J    -> v
        Kj   -> v / 1000
        Kwh  -> v / 3.6e6
        Cal  -> v / 4.184
        Kcal -> v / 4184

convertByte :: Value -> ByteU -> ByteU -> Value
convertByte v to tn = round2Cen (fromByte (toByte v to) tn)

toByte :: Value -> ByteU -> Value
toByte v u =
    case u of
        B  -> v
        Kb -> v * 1024
        Mb -> v * 1048576
        Gb -> v * 1073741824
        Tb -> v * 1099511627776

fromByte :: Value -> ByteU -> Value
fromByte v u =
    case u of
        B  -> v
        Kb -> v / 1024
        Mb -> v / 1048576
        Gb -> v / 1073741824
        Tb -> v / 1099511627776