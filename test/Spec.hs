{-| 
Module: Spec
Description: Unit tests of the used functions
-}

import Test.HUnit 

import DataTypes

import UnitConversion

test_length :: Test 
test_length = test [
    "KmtM"  ~: 1000                ~=? convertLength 1 Km M,  -- Convert 1 kilometer to meters
    "MitKm" ~: 1.609344            ~=? convertLength 1 Mi Km, -- Convert 1 mile to kilometers
    "FttYd" ~: 0.33333333333333337 ~=? convertLength 1 Ft Yd  -- convert 1 foot to yards
    ]

test_weigth :: Test
test_weigth = test [
    "GtoKg" ~: 0.001   ~=? convertWeigth 1 G Kg, -- Convert 1 gram to kilograms
    "LbtG"  ~: 453.592 ~=? convertWeigth 1 Lb G, -- Convert 1 pound to grams
    "MgtKg" ~: 1.0e-6  ~=? convertWeigth 1 Mg Kg -- Convert 1 miligram to kilograms
    ]

test_temperature :: Test
test_temperature = test [
    "CtF" ~: 33.8               ~=? convertTemperature 1 C F, -- Convert 1 degree Celsius to degrees Fahrenheit
    "FtK" ~: 255.92777777777775 ~=? convertTemperature 1 F K, -- Convert 1 degree fahrenheit to kelvin
    "KtC" ~: -272.15            ~=? convertTemperature 1 K C  -- Convert 1 kelvin to degrees celsius
    ]

test_speed :: Test 
test_speed = test [
    "MpstMiph"  ~: 2.23694            ~=? convertSpeed 1 Mps Miph, -- Convert 1 meter per second to miles per hour
    "KmphtMps"  ~: 0.2777777777777778 ~=? convertSpeed 1 Kmph Mps, -- Convert 1 kilometer per hour to meters per second
    "MipstCmps" ~: 16.09344           ~=? convertSpeed 1 Mips Cmps -- Convert 1 mile per second to centimeters per second
    ]

test_time :: Test 
test_time = test [
    "StMin" ~: 1.6666666666666666e-2 ~=? convertTime 1 S Min, -- Convert 1 second to minutes
    "MotY"  ~: 1                     ~=? convertTime 12 Mo Y, -- Convert 12 months to years
    "WtD"   ~: 7                     ~=? convertTime 1 W D    -- Convert 1 weak to days
    ]

test_area :: Test 
test_area = test [
    "Yd2tCm2" ~: 8361.269999999999 ~=? convertArea 1 Yd2 Cm2, -- Convert 1 square yard to square centimeters
    "Mm2tM2"  ~: 1.0e-6            ~=? convertArea 1 Mm2 M2,  -- Convert 1 square milimeter to square meters
    "Ft2tM2"  ~: 9.2903e-2         ~=? convertArea 1 Ft2 M2   -- Convert 1 square foot to square meters
    ]

test_volume :: Test 
test_volume = test [
    "Dm3tL"  ~: 1.0        ~=? convertVolume 1 Dm3 L,  -- Convert 1 cubic decimeter to litres
    "Mm3tM3" ~: 1.0e-9     ~=? convertVolume 1 Mm3 M3, -- Convert 1 cubic milimeter to cubic meters
    "Ft3tM3" ~: 2.83168e-2 ~=? convertVolume 1 Ft3 M3  -- Convert 1 cubic foot to cubic meters
    ]

test_energy :: Test 
test_energy = test [
    "JtKj"     ~: 0.001   ~=? convertEnergy 1 J Kj,     -- Convert 1 joul to kilojoules
    "KcaltCal" ~: 1.0e-3  ~=? convertEnergy 1 Cal Kcal, -- Convert 1 kilocalorie to calories
    "KwhtJ"    ~: 3600000 ~=? convertEnergy 1 Kwh J     -- Convert 1 kilowatt per hour to joules
    ]

test_byte :: Test 
test_byte = test [
    "MbtKb" ~: 1024        ~=? convertByte 1 Mb Kb, -- Convert 1 megabyte to kilobytes
    "GbtTb" ~: 9.765625e-4 ~=? convertByte 1 Gb Tb, -- Convert 1 gigabyte to terabytes
    "KbtB"  ~: 1024        ~=? convertByte 1 Kb B   -- Convert 1 kilobyte to bytes
    ] 

main :: IO ()
main = do runTestTTAndExit $ test [test_length, test_weigth, test_temperature, test_speed, test_time, test_area, test_volume, test_energy, test_byte]