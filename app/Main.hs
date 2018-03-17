{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text.Lazy
import Web.Scotty
import ChordAnalysis
import Notes
import Chords
import Scales
import Data.Monoid ((<>), mconcat)

main = do
    putStrLn "Starting Server..."
    scotty 3000 $ do
        get "/" $ do
            html $ mconcat ["<ul>",
                            itemLink "/notes" "notes",
                            itemLink "/:note/chords" "chords",
                            itemLink "/modes" "modes",
                            "</ul>"]
        get "/notes" $ do
            json $ allNotes
        get "/:note/chords" $ do
            note <- (jsonData :: ActionM Note)
            json $ note
        get "/modes" $ do
            json $ allModes

itemLink :: Text -> Text -> Text
itemLink dir nombre = "<li><a href='" <> dir <> "'>" <> nombre <> "</a></li>"
    