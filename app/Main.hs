module Main (main) where

import DataTypes ()

import UnitConversion ()

import Graphics.UI.Gtk

main :: IO ()
main = do
    _ <- initGUI
    window1 <- windowNew
    set window1 [windowTitle := "Haskonvert"
               , windowDefaultWidth := 1400
               , windowDefaultHeight := 1200
               ]

    fixed <- fixedNew
    containerAdd window1 fixed

    label1 <- labelNew (Nothing :: Maybe String)
    set label1 [ labelLabel := "Haskonvert"
              , labelUseMarkup := True
              , widgetWidthRequest := 200
              ]
    font <- fontDescriptionNew
    fontDescriptionSetFamily font "Serif"
    fontDescriptionSetSize font 28
    widgetModifyFont label1 (Just font)
    fixedPut fixed label1 (510, 50)

    widgetShowAll window1
    mainGUI