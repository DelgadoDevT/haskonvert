module Main (main) where

import DataTypes ()

import UnitConversion ()

import Graphics.UI.Gtk

main :: IO ()
main = do
    _ <- initGUI

    window1 <- windowNew
    windowSetPosition window1 WinPosCenter
    set window1 [ containerBorderWidth := 15
                , windowTitle          := "Haskonvert"
                , windowResizable      := False
                , windowDefaultWidth   := 800
                , windowDefaultHeight  := 800
                ]
    _ <- on window1 objectDestroy mainQuit

    table1 <- tableNew 4 3 True
    tableSetColSpacing table1 0 10
    tableSetColSpacing table1 1 10
    tableSetRowSpacing table1 0 10
    tableSetRowSpacing table1 1 10
    tableSetRowSpacing table1 2 10

    image1 <- imageNewFromFile "src/Resources/logo.png"

    button1 <- buttonNewWithLabel "Length"
    _ <- on button1 buttonActivated $ do
       putStrLn "Hello, World!"

    button2 <- buttonNewWithLabel "Weigth"
    _ <- on button2 buttonActivated $ do
       putStrLn "Hello, World!"

    button3 <- buttonNewWithLabel "Temperature"
    _ <- on button3 buttonActivated $ do
       putStrLn "Hello, World!"

    button4 <- buttonNewWithLabel "Speed"
    _ <- on button4 buttonActivated $ do
       putStrLn "Hello, World!"

    button5 <- buttonNewWithLabel "Time"
    _ <- on button5 buttonActivated $ do
       putStrLn "Hello, World!"

    button6 <- buttonNewWithLabel "Area"
    _ <- on button6 buttonActivated $ do
       putStrLn "Hello, World!"

    button7 <- buttonNewWithLabel "Volume"
    _ <- on button7 buttonActivated $ do
       putStrLn "Hello, World!"

    button8 <- buttonNewWithLabel "Energy"
    _ <- on button8 buttonActivated $ do
       putStrLn "Hello, World!"

    button9 <- buttonNewWithLabel "Byte"
    _ <- on button9 buttonActivated $ do
       putStrLn "Hello, World!"

    tableAttachDefaults table1 image1  0 3 0 1
    tableAttachDefaults table1 button1 0 1 1 2
    tableAttachDefaults table1 button2 1 2 1 2
    tableAttachDefaults table1 button3 2 3 1 2
    tableAttachDefaults table1 button4 0 1 2 3
    tableAttachDefaults table1 button5 1 2 2 3
    tableAttachDefaults table1 button6 2 3 2 3
    tableAttachDefaults table1 button7 0 1 3 4
    tableAttachDefaults table1 button8 1 2 3 4
    tableAttachDefaults table1 button9 2 3 3 4

    containerAdd window1 table1

    widgetShowAll window1
    mainGUI