module Main (main) where

import DataTypes

import UnitConversion

import Graphics.UI.Gtk

import Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T

createcoB1 :: IO ComboBox
createcoB1 = do
    combo <- comboBoxNewText
    mapM_ (comboBoxAppendText combo . T.pack . show) [Km, Hm, Dam, M, Dm, Cm, Mm, Ft, Mi, Yd]
    return combo

createcoB2 :: IO ComboBox
createcoB2 = do
    combo <- comboBoxNewText
    mapM_ (comboBoxAppendText combo . T.pack . show) [T , Kg , G, Mg, Lb]
    return combo

createcoB3 :: IO ComboBox
createcoB3 = do
    combo <- comboBoxNewText
    mapM_ (comboBoxAppendText combo . T.pack . show) [C, F, K]
    return combo

createcoB4 :: IO ComboBox
createcoB4 = do
    combo <- comboBoxNewText
    mapM_ (comboBoxAppendText combo . T.pack . show) [Mps, Kmph, Miph, Cmps, Mips]
    return combo

createcoB5 :: IO ComboBox
createcoB5 = do
    combo <- comboBoxNewText
    mapM_ (comboBoxAppendText combo . T.pack . show) [S, Min, H, D, W, Mo, Y]
    return combo

createcoB6 :: IO ComboBox
createcoB6 = do
    combo <- comboBoxNewText
    mapM_ (comboBoxAppendText combo . T.pack . show) [Yd2,Cm2, Mi2, Ft2, Mm2, Km2, M2]
    return combo

createcoB7 :: IO ComboBox
createcoB7 = do
    combo <- comboBoxNewText
    mapM_ (comboBoxAppendText combo . T.pack . show) [Km3, M3, Dm3, Mm3, Ft3, Yd3, Gal, In3, L]
    return combo

createcoB8 :: IO ComboBox
createcoB8 = do
    combo <- comboBoxNewText
    mapM_ (comboBoxAppendText combo . T.pack . show) [J, Kj, Kwh, Cal, Kcal]
    return combo

createcoB9 :: IO ComboBox
createcoB9 = do
    combo <- comboBoxNewText
    mapM_ (comboBoxAppendText combo . T.pack . show) [B, Kb, Mb, Gb, Tb]
    return combo

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

    window2 <- windowNew
    windowSetPosition window2 WinPosCenter
    set window2 [ containerBorderWidth := 15
                , windowTitle          := "Haskonvert"
                , windowResizable      := False
                , windowDefaultWidth   := 800
                , windowDefaultHeight  := 800
                ]

    window3 <- windowNew
    windowSetPosition window3 WinPosCenter
    set window3 [ containerBorderWidth := 15
                , windowTitle          := "Haskonvert"
                , windowResizable      := False
                , windowDefaultWidth   := 800
                , windowDefaultHeight  := 800
                ]

    window4 <- windowNew
    windowSetPosition window4 WinPosCenter
    set window4 [ containerBorderWidth := 15
                , windowTitle          := "Haskonvert"
                , windowResizable      := False
                , windowDefaultWidth   := 800
                , windowDefaultHeight  := 800
                ]

    window5 <- windowNew
    windowSetPosition window5 WinPosCenter
    set window5 [ containerBorderWidth := 15
                , windowTitle          := "Haskonvert"
                , windowResizable      := False
                , windowDefaultWidth   := 800
                , windowDefaultHeight  := 800
                ]

    window6 <- windowNew
    windowSetPosition window6 WinPosCenter
    set window6 [ containerBorderWidth := 15
                , windowTitle          := "Haskonvert"
                , windowResizable      := False
                , windowDefaultWidth   := 800
                , windowDefaultHeight  := 800
                ]

    window7 <- windowNew
    windowSetPosition window7 WinPosCenter
    set window7 [ containerBorderWidth := 15
                , windowTitle          := "Haskonvert"
                , windowResizable      := False
                , windowDefaultWidth   := 800
                , windowDefaultHeight  := 800
                ]

    window8 <- windowNew
    windowSetPosition window8 WinPosCenter
    set window8 [ containerBorderWidth := 15
                , windowTitle          := "Haskonvert"
                , windowResizable      := False
                , windowDefaultWidth   := 800
                , windowDefaultHeight  := 800
                ]

    window9 <- windowNew
    windowSetPosition window9 WinPosCenter
    set window9 [ containerBorderWidth := 15
                , windowTitle          := "Haskonvert"
                , windowResizable      := False
                , windowDefaultWidth   := 800
                , windowDefaultHeight  := 800
                ]

    window10 <- windowNew
    windowSetPosition window10 WinPosCenter
    set window10 [ containerBorderWidth := 15
                 , windowTitle          := "Haskonvert"
                 , windowResizable      := False
                 , windowDefaultWidth   := 800
                 , windowDefaultHeight  := 800
                 ]

    _ <- on window1 deleteEvent $ do
        liftIO mainQuit
        return False

    _ <- on window2 deleteEvent $ do
        liftIO mainQuit
        return False

    _ <- on window3 deleteEvent $ do 
        liftIO mainQuit
        return False

    _ <- on window4 deleteEvent $ do 
        liftIO mainQuit
        return False

    _ <- on window5 deleteEvent $ do 
        liftIO mainQuit
        return False

    _ <- on window6 deleteEvent $ do 
        liftIO mainQuit
        return False

    _ <- on window7 deleteEvent $ do 
        liftIO mainQuit
        return False

    _ <- on window8 deleteEvent $ do 
        liftIO mainQuit
        return False

    _ <- on window9 deleteEvent $ do 
        liftIO mainQuit
        return False

    _ <- on window10 deleteEvent $ do 
        liftIO mainQuit
        return False

    table1 <- tableNew 4 3 True
    tableSetColSpacing table1 0 10
    tableSetColSpacing table1 1 10
    tableSetRowSpacing table1 0 10
    tableSetRowSpacing table1 1 10
    tableSetRowSpacing table1 2 10

    table2 <- tableNew 3 3 True
    tableSetColSpacing table2 1 10
    tableSetRowSpacing table2 0 10
    tableSetRowSpacing table2 1 10
    tableSetRowSpacing table2 2 10

    table3 <- tableNew 3 3 True
    tableSetColSpacing table3 1 10
    tableSetRowSpacing table3 0 10
    tableSetRowSpacing table3 1 10
    tableSetRowSpacing table3 2 10

    table4 <- tableNew 3 3 True
    tableSetColSpacing table4 1 10
    tableSetRowSpacing table4 0 10
    tableSetRowSpacing table4 1 10
    tableSetRowSpacing table4 2 10

    table5 <- tableNew 3 3 True
    tableSetColSpacing table5 1 10
    tableSetRowSpacing table5 0 10
    tableSetRowSpacing table5 1 10
    tableSetRowSpacing table5 2 10

    table6 <- tableNew 3 3 True
    tableSetColSpacing table6 1 10
    tableSetRowSpacing table6 0 10
    tableSetRowSpacing table6 1 10
    tableSetRowSpacing table6 2 10

    table7 <- tableNew 3 3 True
    tableSetColSpacing table7 1 10
    tableSetRowSpacing table7 0 10
    tableSetRowSpacing table7 1 10
    tableSetRowSpacing table7 2 10

    table8 <- tableNew 3 3 True
    tableSetColSpacing table8 1 10
    tableSetRowSpacing table8 0 10
    tableSetRowSpacing table8 1 10
    tableSetRowSpacing table8 2 10

    table9 <- tableNew 3 3 True
    tableSetColSpacing table9 1 10
    tableSetRowSpacing table9 0 10
    tableSetRowSpacing table9 1 10
    tableSetRowSpacing table9 2 10

    table10 <- tableNew 3 3 True
    tableSetColSpacing table10 1 10
    tableSetRowSpacing table10 0 10
    tableSetRowSpacing table10 1 10
    tableSetRowSpacing table10 2 10

    image1 <- imageNewFromFile "src/Resources/logo.png"

    button1 <- buttonNewWithLabel "Length"
    _ <- on button1 buttonActivated $ do
       widgetHide window1
       widgetShowAll window2

    button2 <- buttonNewWithLabel "Weigth"
    _ <- on button2 buttonActivated $ do
       widgetHide window1
       widgetShowAll window3

    button3 <- buttonNewWithLabel "Temperature"
    _ <- on button3 buttonActivated $ do
       widgetHide window1
       widgetShowAll window4

    button4 <- buttonNewWithLabel "Speed"
    _ <- on button4 buttonActivated $ do
       widgetHide window1
       widgetShowAll window5

    button5 <- buttonNewWithLabel "Time"
    _ <- on button5 buttonActivated $ do
       widgetHide window1
       widgetShowAll window6

    button6 <- buttonNewWithLabel "Area"
    _ <- on button6 buttonActivated $ do
       widgetHide window1
       widgetShowAll window7

    button7 <- buttonNewWithLabel "Volume"
    _ <- on button7 buttonActivated $ do
       widgetHide window1
       widgetShowAll window8

    button8 <- buttonNewWithLabel "Energy"
    _ <- on button8 buttonActivated $ do
       widgetHide window1
       widgetShowAll window9

    button9 <- buttonNewWithLabel "Byte"
    _ <- on button9 buttonActivated $ do
       widgetHide window1
       widgetShowAll window10

    button10 <- buttonNewWithLabel "Return"
    _ <- on button10 buttonActivated $ do 
        widgetHide window2
        widgetShowAll window1

    button11 <- buttonNewWithLabel "Return"
    _ <- on button11 buttonActivated $ do 
        widgetHide window3
        widgetShowAll window1

    button12 <- buttonNewWithLabel "Return"
    _ <- on button12 buttonActivated $ do 
        widgetHide window4
        widgetShowAll window1

    button13 <- buttonNewWithLabel "Return"
    _ <- on button13 buttonActivated $ do 
        widgetHide window5
        widgetShowAll window1

    button14 <- buttonNewWithLabel "Return"
    _ <- on button14 buttonActivated $ do 
        widgetHide window6
        widgetShowAll window1

    button15 <- buttonNewWithLabel "Return"
    _ <- on button15 buttonActivated $ do 
        widgetHide window7
        widgetShowAll window1

    button16 <- buttonNewWithLabel "Return"
    _ <- on button16 buttonActivated $ do 
        widgetHide window8
        widgetShowAll window1

    button17 <- buttonNewWithLabel "Return"
    _ <- on button17 buttonActivated $ do 
        widgetHide window9
        widgetShowAll window1

    button18 <- buttonNewWithLabel "Return"
    _ <- on button18 buttonActivated $ do 
        widgetHide window10
        widgetShowAll window1

    entryi1 <- entryNew
    set entryi1 [entryText := "0", entryXalign := 0.5]

    entryf1 <- entryNew
    set entryf1 [entryEditable := False, entryText := "", entryXalign := 0.5]

    entryi2 <- entryNew
    set entryi2 [entryText := "0", entryXalign := 0.5]

    entryf2 <- entryNew
    set entryf2 [entryEditable := False, entryText := "", entryXalign := 0.5]

    entryi3 <- entryNew
    set entryi3 [entryText := "0", entryXalign := 0.5]

    entryf3 <- entryNew
    set entryf3 [entryEditable := False, entryText := "", entryXalign := 0.5]

    entryi4 <- entryNew
    set entryi4 [entryText := "0", entryXalign := 0.5]

    entryf4 <- entryNew
    set entryf4 [entryEditable := False, entryText := "", entryXalign := 0.5]

    entryi5 <- entryNew
    set entryi5 [entryText := "0", entryXalign := 0.5]

    entryf5 <- entryNew
    set entryf5 [entryEditable := False, entryText := "", entryXalign := 0.5]

    entryi6 <- entryNew
    set entryi6 [entryText := "0", entryXalign := 0.5]

    entryf6 <- entryNew
    set entryf6 [entryEditable := False, entryText := "", entryXalign := 0.5]

    entryi7 <- entryNew
    set entryi7 [entryText := "0", entryXalign := 0.5]

    entryf7 <- entryNew
    set entryf7 [entryEditable := False, entryText := "", entryXalign := 0.5]

    entryi8 <- entryNew
    set entryi8 [entryText := "0", entryXalign := 0.5]

    entryf8 <- entryNew
    set entryf8 [entryEditable := False, entryText := "", entryXalign := 0.5]

    entryi9 <- entryNew
    set entryi9 [entryText := "0", entryXalign := 0.5]

    entryf9 <- entryNew
    set entryf9 [entryEditable := False, entryText := "", entryXalign := 0.5]

    co1 <- createcoB1
    _ <- comboBoxSetActive co1 0
    
    cn1 <- createcoB1
    _ <- comboBoxSetActive cn1 0

    co2 <- createcoB2
    _ <- comboBoxSetActive co2 0

    cn2 <- createcoB2
    _ <- comboBoxSetActive cn2 0

    co3 <- createcoB3
    _ <- comboBoxSetActive co3 0

    cn3 <- createcoB3
    _ <- comboBoxSetActive cn3 0

    co4 <- createcoB4
    _ <- comboBoxSetActive co4 0

    cn4 <- createcoB4
    _ <- comboBoxSetActive cn4 0

    co5 <- createcoB5
    _ <- comboBoxSetActive co5 0

    cn5 <- createcoB5
    _ <- comboBoxSetActive cn5 0

    co6 <- createcoB6
    _ <- comboBoxSetActive co6 0

    cn6 <- createcoB6
    _ <- comboBoxSetActive cn6 0

    co7 <- createcoB7
    _ <- comboBoxSetActive co7 0

    cn7 <- createcoB7
    _ <- comboBoxSetActive cn7 0

    co8 <- createcoB8
    _ <- comboBoxSetActive co8 0

    cn8 <- createcoB8
    _ <- comboBoxSetActive cn8 0

    co9 <- createcoB9
    _ <- comboBoxSetActive co9 0

    cn9 <- createcoB9
    _ <- comboBoxSetActive cn9 0

    buttona <- buttonNewWithLabel "Convert"
    _ <- on buttona buttonActivated $ do
        numStr <- entryGetText entryi1
        let n = read numStr :: Value
        sUP <- comboBoxGetActive co1
        sUN <- comboBoxGetActive cn1
        let uP = intToLength sUP
        let uN = intToLength sUN
        let r = convertLength n uP uN
        entrySetText entryf1 (show r)

    buttonb <- buttonNewWithLabel "Convert"
    _ <- on buttonb buttonActivated $ do
        numStr <- entryGetText entryi2
        let n = read numStr :: Value
        sUP <- comboBoxGetActive co2
        sUN <- comboBoxGetActive cn2
        let uP = intToWeigth sUP
        let uN = intToWeigth sUN
        let r = convertWeigth n uP uN
        entrySetText entryf2 (show r)

    buttonc <- buttonNewWithLabel "Convert"
    _ <- on buttonc buttonActivated $ do
        numStr <- entryGetText entryi3
        let n = read numStr :: Value
        sUP <- comboBoxGetActive co3
        sUN <- comboBoxGetActive cn3
        let uP = intToTemperature sUP
        let uN = intToTemperature sUN
        let r = convertTemperature n uP uN
        entrySetText entryf3 (show r)

    buttond <- buttonNewWithLabel "Convert"
    _ <- on buttond buttonActivated $ do
        numStr <- entryGetText entryi4
        let n = read numStr :: Value
        sUP <- comboBoxGetActive co4
        sUN <- comboBoxGetActive cn4
        let uP = intToSpeed sUP
        let uN = intToSpeed sUN
        let r = convertSpeed n uP uN
        entrySetText entryf4 (show r)

    buttone <- buttonNewWithLabel "Convert"
    _ <- on buttone buttonActivated $ do
        numStr <- entryGetText entryi5
        let n = read numStr :: Value
        sUP <- comboBoxGetActive co5
        sUN <- comboBoxGetActive cn5
        let uP = intToTime sUP
        let uN = intToTime sUN
        let r = convertTime n uP uN
        entrySetText entryf5 (show r)

    buttonf <- buttonNewWithLabel "Convert"
    _ <- on buttonf buttonActivated $ do
        numStr <- entryGetText entryi6
        let n = read numStr :: Value
        sUP <- comboBoxGetActive co6
        sUN <- comboBoxGetActive cn6
        let uP = intToArea sUP
        let uN = intToArea sUN
        let r = convertArea n uP uN
        entrySetText entryf6 (show r)

    buttong <- buttonNewWithLabel "Convert"
    _ <- on buttong buttonActivated $ do
        numStr <- entryGetText entryi7
        let n = read numStr :: Value
        sUP <- comboBoxGetActive co7
        sUN <- comboBoxGetActive cn7
        let uP = intToVolume sUP
        let uN = intToVolume sUN
        let r = convertVolume n uP uN
        entrySetText entryf7 (show r)
    
    buttonh <- buttonNewWithLabel "Convert"
    _ <- on buttonh buttonActivated $ do
        numStr <- entryGetText entryi8
        let n = read numStr :: Value
        sUP <- comboBoxGetActive co8
        sUN <- comboBoxGetActive cn8
        let uP = intToEnergy sUP
        let uN = intToEnergy sUN
        let r = convertEnergy n uP uN
        entrySetText entryf8 (show r)

    buttoni <- buttonNewWithLabel "Convert"
    _ <- on buttoni buttonActivated $ do
        numStr <- entryGetText entryi9
        let n = read numStr :: Value
        sUP <- comboBoxGetActive co9
        sUN <- comboBoxGetActive cn9
        let uP = intToByte sUP
        let uN = intToByte sUN
        let r = convertByte n uP uN
        entrySetText entryf9 (show r)

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

    tableAttachDefaults table2 entryi1  0 1 0 1
    tableAttachDefaults table2 co1      1 2 0 1
    tableAttachDefaults table2 cn1      1 2 1 2
    tableAttachDefaults table2 buttona  0 2 2 3
    tableAttachDefaults table2 entryf1  0 1 1 2
    tableAttachDefaults table2 button10 2 3 0 3

    tableAttachDefaults table3 entryi2  0 1 0 1
    tableAttachDefaults table3 co2      1 2 0 1
    tableAttachDefaults table3 cn2      1 2 1 2
    tableAttachDefaults table3 buttonb  0 2 2 3
    tableAttachDefaults table3 entryf2  0 1 1 2
    tableAttachDefaults table3 button11 2 3 0 3

    tableAttachDefaults table4 entryi3  0 1 0 1
    tableAttachDefaults table4 co3      1 2 0 1
    tableAttachDefaults table4 cn3      1 2 1 2
    tableAttachDefaults table4 buttonc  0 2 2 3
    tableAttachDefaults table4 entryf3  0 1 1 2
    tableAttachDefaults table4 button12 2 3 0 3

    tableAttachDefaults table5 entryi4  0 1 0 1
    tableAttachDefaults table5 co4      1 2 0 1
    tableAttachDefaults table5 cn4      1 2 1 2
    tableAttachDefaults table5 buttond  0 2 2 3
    tableAttachDefaults table5 entryf4  0 1 1 2
    tableAttachDefaults table5 button13 2 3 0 3

    tableAttachDefaults table6 entryi5  0 1 0 1
    tableAttachDefaults table6 co5      1 2 0 1
    tableAttachDefaults table6 cn5      1 2 1 2
    tableAttachDefaults table6 buttone  0 2 2 3
    tableAttachDefaults table6 entryf5  0 1 1 2
    tableAttachDefaults table6 button14 2 3 0 3

    tableAttachDefaults table7 entryi6  0 1 0 1
    tableAttachDefaults table7 co6      1 2 0 1
    tableAttachDefaults table7 cn6      1 2 1 2
    tableAttachDefaults table7 buttonf  0 2 2 3
    tableAttachDefaults table7 entryf6  0 1 1 2
    tableAttachDefaults table7 button15 2 3 0 3

    tableAttachDefaults table8 entryi7  0 1 0 1
    tableAttachDefaults table8 co7      1 2 0 1
    tableAttachDefaults table8 cn7      1 2 1 2
    tableAttachDefaults table8 buttong  0 2 2 3
    tableAttachDefaults table8 entryf7  0 1 1 2
    tableAttachDefaults table8 button16 2 3 0 3

    tableAttachDefaults table9 entryi8  0 1 0 1
    tableAttachDefaults table9 co8      1 2 0 1
    tableAttachDefaults table9 cn8      1 2 1 2
    tableAttachDefaults table9 buttonh  0 2 2 3
    tableAttachDefaults table9 entryf8  0 1 1 2
    tableAttachDefaults table9 button17 2 3 0 3

    tableAttachDefaults table10 entryi9  0 1 0 1
    tableAttachDefaults table10 co9      1 2 0 1
    tableAttachDefaults table10 cn9      1 2 1 2
    tableAttachDefaults table10 buttoni  0 2 2 3
    tableAttachDefaults table10 entryf9  0 1 1 2
    tableAttachDefaults table10 button18 2 3 0 3

    containerAdd window1 table1
    containerAdd window2 table2 
    containerAdd window3 table3
    containerAdd window4 table4
    containerAdd window5 table5 
    containerAdd window6 table6 
    containerAdd window7 table7
    containerAdd window8 table8
    containerAdd window9 table9
    containerAdd window10 table10   

    widgetShowAll window1
    mainGUI