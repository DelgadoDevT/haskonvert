{-| 
Module: DataTypes
Description: This module provides essential data types used across the program.
-}

module DataTypes (module DataTypes) where

{-| 
To create the program, nine different data were defined, including length, weight, temperature, speed, time, area, volume, energy, and bytes. 
Each type corresponds to a specific scale, providing a comprehensive range for accurate and efficient data transformations within diverse contexts.
-}

data LengthU = Km  -- ^ Kilometers
             | Hm  -- ^ Hectometers
             | Dam -- ^ Dekameters
             | M   -- ^ Meters
             | Dm  -- ^ Decimeters
             | Cm  -- ^ Centimeters
             | Mm  -- ^ Millimeters
             | Ft  -- ^ Feet
             | Mi  -- ^ Miles
             | Yd  -- ^ Yards
             deriving Show

data WeightU = T   -- ^ Tonnes
             | Kg  -- ^ Kilograms
             | G   -- ^ Grams
             | Mg  -- ^ Milligrams
             | Lb  -- ^ Pounds
             deriving Show

data TemperatureU = C  -- ^ Celsius
                  | F  -- ^ Fahrenheit
                  | K  -- ^ Kelvin
                  deriving Show

data SpeedU = Mps  -- ^ Meters per second
            | Kmph -- ^ Kilometers per hour
            | Miph -- ^ Miles per hour
            | Cmps -- ^ Centimeters per second
            | Mips -- ^ Miles per second
            deriving Show

data TimeU = S   -- ^ Seconds
           | Min -- ^ Minutes
           | H   -- ^ Hours
           | D   -- ^ Days
           | W   -- ^ Weeks
           | Mo  -- ^ Months
           | Y   -- ^ Years
           deriving Show

data AreaU = Yd2 -- ^ Square Yards
           | Cm2 -- ^ Square Centimeters
           | Mi2 -- ^ Square Miles
           | Ft2 -- ^ Square Feet
           | Mm2 -- ^ Square Millimeters
           | Km2 -- ^ Square Kilometers
           | M2  -- ^ Square Meters
           deriving Show

data VolumeU = Km3  -- ^ Cubic Kilometers
             | M3   -- ^ Cubic Meters
             | Dm3  -- ^ Cubic Decimeters
             | Mm3  -- ^ Cubic Millimeters
             | Ft3  -- ^ Cubic Feet
             | Yd3  -- ^ Cubic Yards
             | Gal  -- ^ Gallons
             | In3  -- ^ Cubic Inches
             | L    -- ^ Liters
             deriving Show

data EnergyU = J   -- ^ Joules
             | Kj  -- ^ Kilojoules
             | Kwh -- ^ Kilowatt-hours
             | Cal -- ^ Calories
             | Kcal-- ^ Kilocalories
             deriving Show

data ByteU = B   -- ^ Bytes
           | Kb  -- ^ Kilobytes
           | Mb  -- ^ Megabytes
           | Gb  -- ^ Gigabytes
           | Tb  -- ^ Terabytes
           deriving Show

type Value = Double -- ^ Corresponds to all the numerical values used in conversions