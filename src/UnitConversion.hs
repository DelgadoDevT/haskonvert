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
  , intToLength
  , intToWeigth
  , intToTemperature
  , intToSpeed
  , intToTime
  , intToArea
  , intToVolume
  , intToEnergy
  , intToByte
  ) where 

import DataTypes

{-| 
The functions within this module are tailored for the meticulous conversion of values within designated scales. 
Their purpose is to facilitate seamless and accurate unit conversions, enhancing the overall precision and efficiency of the codebase.

== Examples of use:

>>> convertLength 1 Km M
1000

>>> convertTime 1 W D
7

>>> convertEnergy 1 Cal Kcal
1.0e-3
-}

-- | Converts a value from one length unit to another
convertLength :: Value -> LengthU -> LengthU -> Value
convertLength v to tn = toLength (toMeter v to) tn

-- | Converts a value to meters
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

-- | Converts a value from meters to the specified unit
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

-- | Convert a number to the corresponding length unit
intToLength :: Int -> LengthU
intToLength i = case i of
    0 -> Km
    1 -> Hm
    2 -> Dam
    3 -> M
    4 -> Dm
    5 -> Cm
    6 -> Mm
    7 -> Ft
    8 -> Mi
    9 -> Yd
    _ -> error "Invalid Unit"

-- | Converts a value from one weight unit to another
convertWeigth :: Value -> WeightU -> WeightU -> Value
convertWeigth v to tn = toWeight (toGram v to) tn 

-- | Converts a value to grams
toGram :: Value -> WeightU -> Value
toGram v u = 
    case u of 
        T  -> v * 1e6
        Kg -> v * 1000
        G  -> v 
        Mg -> v * 0.001
        Lb  -> v * 453.592

-- | Converts a value from grams to the specified unit
toWeight :: Value -> WeightU -> Value
toWeight v u = 
    case u of 
        T  -> v / 1e6
        Kg -> v / 1000
        G  -> v 
        Mg -> v / 0.001
        Lb  -> v / 453.592

-- | Converts a number to the corresponding weight unit
intToWeigth :: Int -> WeightU
intToWeigth i = case i of
    0 -> T
    1 -> Kg
    2 -> G
    3 -> Mg
    4 -> Lb
    _ -> error "Invalid Unit"

-- | Converts a value from one temperature unit to another
convertTemperature :: Value -> TemperatureU -> TemperatureU -> Value
convertTemperature v to tn = toTemperature (toCelsius v to) tn

-- | Converts a value to degrees celsius
toCelsius :: Value -> TemperatureU -> Value
toCelsius v u =
    case u of 
        C -> v
        F -> (v - 32) * 5/9
        K -> v - 273.15

-- | Converts a value from degrees celsius to the specified unit
toTemperature :: Value -> TemperatureU -> Value
toTemperature v u =
    case u of 
        C -> v
        F -> v * 1.8 + 32
        K -> v + 273.15

-- | Converts a number to the corresponding temperature unit
intToTemperature :: Int -> TemperatureU
intToTemperature i = case i of
    0 -> C
    1 -> F
    2 -> K
    _ -> error "Invalid Unit"

-- | Converts a value from one speed unit to another
convertSpeed :: Value -> SpeedU -> SpeedU -> Value
convertSpeed v to tn = toSpeed (toMps v to) tn

-- | Converts a value to meters per second 
toMps :: Value -> SpeedU -> Value
toMps v u = 
    case u of 
        Mps  -> v 
        Kmph -> v / 3.6
        Miph -> v / 2.23694
        Cmps -> v * 100
        Mips -> v * 1609.344

-- | Converts a value from meters per second to the specified unit
toSpeed :: Value -> SpeedU -> Value
toSpeed v u = 
    case u of 
        Mps  -> v 
        Kmph -> v * 3.6
        Miph -> v * 2.23694
        Cmps -> v / 100
        Mips -> v / 1609.344

-- | Converts a number to the corresponding speed unit
intToSpeed :: Int -> SpeedU
intToSpeed i = case i of
    0 -> Mps
    1 -> Kmph
    2 -> Miph 
    3 -> Cmps 
    4 -> Mips
    _ -> error "Invalid Unit"

-- | Converts a value from one time unit to another
convertTime :: Value -> TimeU -> TimeU -> Value
convertTime v to tn = toTime (toSec v to) tn

-- | Converts a value to seconds
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

-- | Converts a value from seconds to the specified unit
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

-- | Converts a number to the corresponding time unit
intToTime :: Int -> TimeU
intToTime i = case i of 
    0 -> S 
    1 -> Min 
    2 -> H 
    3 -> D 
    4 -> W 
    5 -> Mo 
    6 -> Y 
    _ -> error "Invalid Unit"

-- | Converts a value from one area unit to another
convertArea :: Value -> AreaU -> AreaU -> Value
convertArea v to tn = toArea (toM2 v to) tn

-- | Converts a value to square meters
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

-- | Converts a value from square meters to the specified unit
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

-- | Converts a number to the corresponding area unit
intToArea :: Int -> AreaU
intToArea i = case i of 
    0 -> Yd2
    1 -> Cm2 
    2 -> Mi2 
    3 -> Ft2
    4 -> Mm2 
    5 -> Km2 
    6 -> M2 
    _ -> error "Invalid Unit"

-- | Converts a value from one volume unit to another
convertVolume :: Value -> VolumeU -> VolumeU -> Value
convertVolume v to tn = toVolume (toM3 v to) tn

-- | Converts a value to cubic meters
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

-- | Converts a value from cubic meters to the specified unit
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

-- | Converts a number to the corresponding volume unit
intToVolume :: Int -> VolumeU
intToVolume i = case i of 
    0 -> Km3 
    1 -> M3 
    2 -> Dm3 
    3 -> Mm3 
    4 -> Ft3 
    5 -> Yd3 
    6 -> Gal 
    7 -> In3 
    8 -> L 
    _ -> error "Invalid Unit"

-- | Converts a value from one energy unit to another
convertEnergy :: Value -> EnergyU -> EnergyU -> Value
convertEnergy v to tn = toEnergy (toJoule v to) tn

-- | Converts a value to joules
toJoule :: Value -> EnergyU -> Value
toJoule v u =
    case u of
        J    -> v
        Kj   -> v * 1000
        Kwh  -> v * 3.6e6
        Cal  -> v * 4.184
        Kcal -> v * 4184

-- | Converts a value from joules to the specified unit
toEnergy :: Value -> EnergyU -> Value
toEnergy v u =
    case u of
        J    -> v
        Kj   -> v / 1000
        Kwh  -> v / 3.6e6
        Cal  -> v / 4.184
        Kcal -> v / 4184

-- | Converts a number to the corresponding energy unit
intToEnergy :: Int -> EnergyU 
intToEnergy i = case i of 
    0 -> J 
    1 -> Kj 
    2 -> Kwh 
    3 -> Cal 
    4 -> Kcal 
    _ -> error "Invalid Unit"

-- | Converts a value from one byte unit to another
convertByte :: Value -> ByteU -> ByteU -> Value
convertByte v to tn = fromByte (toByte v to) tn

-- | Converts a value to bytes
toByte :: Value -> ByteU -> Value
toByte v u =
    case u of
        B  -> v
        Kb -> v * 1000
        Mb -> v * 1000000
        Gb -> v * 1000000000
        Tb -> v * 1000000000000

-- | Converts a value from bytes to the specified unit
fromByte :: Value -> ByteU -> Value
fromByte v u =
    case u of
        B  -> v
        Kb -> v / 1000
        Mb -> v / 1000000
        Gb -> v / 1000000000
        Tb -> v / 1000000000000

-- | Converts a number to the corresponding byte unit
intToByte :: Int -> ByteU 
intToByte i = case i of 
    0 -> B
    1 -> Kb 
    2 -> Mb
    3 -> Gb
    4 -> Tb
    _ -> error "Invalid Unit"