{-| 
Module: Spec
Description: Unit tests of the used functions
-}

import Test.HUnit 

import DataTypes

import UnitConversion

test_length :: Test 
test_length = test [
    "KmtM"  ~: 1000                ~=? convertLength 1 Km M,
    "MitKm" ~: 1.609344            ~=? convertLength 1 Mi Km, 
    "FttYd" ~: 0.33333333333333337 ~=? convertLength 1 Ft Yd
    ]

test_weigth :: Test
test_weigth = test [
    "GtoKg" ~: 0.001   ~=? convertWeigth 1 G Kg,
    "LbtG"  ~: 453.592 ~=? convertWeigth 1 Lb G,
    "MgtKg" ~: 1.0e-6  ~=? convertWeigth 1 Mg Kg
    ]

test_temperature :: Test
test_temperature = test [
    "CtF" ~: 33.8               ~=? convertTemperature 1 C F,
    "FtK" ~: 255.92777777777775 ~=? convertTemperature 1 F K,
    "KtC" ~: -272.15            ~=? convertTemperature 1 K C 
    ]

test_speed :: Test 
test_speed = test [
    "MpstMiph"  ~: 2.23694            ~=? convertSpeed 1 Mps Miph, 
    "KmphtMps"  ~: 0.2777777777777778 ~=? convertSpeed 1 Kmph Mps, 
    "MipstCmps" ~: 16.09344           ~=? convertSpeed 1 Mips Cmps
    ]

test_time :: Test 
test_time = test [
    "StMin" ~: 1.6666666666666666e-2 ~=? convertTime 1 S Min, 
    "MotY"  ~: 1                     ~=? convertTime 12 Mo Y,
    "WtD"   ~: 7                     ~=? convertTime 1 W D 
    ]

test_area :: Test 
test_area = test [
    "Yd2tCm2" ~: 8361.269999999999 ~=? convertArea 1 Yd2 Cm2, 
    "Mm2tM2"  ~: 1.0e-6            ~=? convertArea 1 Mm2 M2,
    "Ft2tM2"  ~: 9.2903e-2         ~=? convertArea 1 Ft2 M2
    ]

test_volume :: Test 
test_volume = test [
    "Dm3tL"  ~: 1.0        ~=? convertVolume 1 Dm3 L, 
    "Mm3tM3" ~: 1.0e-9     ~=? convertVolume 1 Mm3 M3,
    "Ft3tM3" ~: 2.83168e-2 ~=? convertVolume 1 Ft3 M3
    ]

test_energy :: Test 
test_energy = test [
    "JtKj"     ~: 0.001   ~=? convertEnergy 1 J Kj, 
    "KcaltCal" ~: 1.0e-3  ~=? convertEnergy 1 Cal Kcal,
    "KwhtJ"    ~: 3600000 ~=? convertEnergy 1 Kwh J
    ]

test_byte :: Test 
test_byte = test [
    "MbtKb" ~: 1024        ~=? convertByte 1 Mb Kb,
    "GbtTb" ~: 9.765625e-4 ~=? convertByte 1 Gb Tb,
    "KbtB"  ~: 1024        ~=? convertByte 1 Kb B 
    ] 

main :: IO ()
main = do runTestTTAndExit $ test [test_length, test_weigth, test_temperature, test_speed, test_time, test_area, test_volume, test_energy, test_byte]