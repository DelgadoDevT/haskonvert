{-| 
Module: UnitConversion
Description: Implementations of functions to convert measurements between various units.
-}

module UnitConversion where 

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