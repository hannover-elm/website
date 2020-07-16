{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Clay ((?), Css)
import qualified Clay as C
import Control.Monad (forM_)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import Data.Function ((&))
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Development.Shake
import Development.Shake.FilePath
import GHC.Generics (Generic)
import Lucid
import Main.Utf8
import qualified Rib
import Rib (IsRoute)
import Rib.Parser.Pandoc as Pandoc

data Route a where
  IndexRoute :: Route IndexData

data IndexData
  = IndexData
      { meetup :: Meetup,
        events :: [Event],
        currentTime :: UTCTime
      }

instance IsRoute Route where
  routeFile = \case
    IndexRoute ->
      pure "index.html"

main :: IO ()
main = withUtf8 $ do
  Rib.run "src" "public" generateSite

generateSite :: Action ()
generateSite = do
  Rib.buildStaticFiles ["static/**"]
  indexData <-
    IndexData
      <$> readJson "meetup.json"
      <*> readJson "events.json"
      <*> liftIO getCurrentTime
  writeHtmlRoute IndexRoute indexData
  compileElmModule "Logo.elm" "logo.js"

readJson :: FromJSON a => FilePath -> Action a
readJson filePath = do
  inputDir <- Rib.ribInputDir
  either fail pure =<< liftIO (Aeson.eitherDecodeFileStrict' (inputDir </> filePath))

data Meetup
  = Meetup
      { description :: Text
      }
  deriving (Generic, Show)

instance FromJSON Meetup

data Event
  = Event
      { name :: Text,
        local_date :: Text,
        local_time :: Text,
        yes_rsvp_count :: Int,
        description :: Text,
        link :: Text
      }
  deriving (Generic, Show)

instance FromJSON Event

writeHtmlRoute r = Rib.writeRoute r . Lucid.renderText . renderPage r

compileElmModule :: FilePath -> FilePath -> Action ()
compileElmModule inputFilePath outputFilePath = do
  inputDir <- Rib.ribInputDir
  outputDir <- Rib.ribOutputDir
  command
    []
    "elm"
    [ "make",
      "--optimize",
      inputDir </> inputFilePath,
      "--output",
      outputDir </> outputFilePath
    ]

renderPage _route (IndexData {..}) = do
  let asMarkdown =
        Pandoc.render . Pandoc.parsePure Pandoc.readMarkdown
      firstEvent =
        case events of
          (event : _) -> Just event
          _ -> Nothing
      currentDate = formatTime defaultTimeLocale "%Y-%m-%d" currentTime
  doctype_
  html_ [lang_ "en"] $ do
    head_ $ do
      title_ "Hannover Elm Language Meetup"
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      link_ [href_ "https://fonts.googleapis.com/css?family=Roboto", rel_ "stylesheet"]
      style_ [type_ "text/css"] $ C.render pageStyle
    body_ $ do
      header_ $ do
        div_ [id_ "logo"] ""
        script_ [src_ "logo.js"] ("" :: Text)
        script_ "Elm.Logo.init({node:document.querySelector(\"#logo\")})"
      main_ $ do
        section_ [class_ "section section--about"] $ do
          h1_ "Hannover Elm Language Meetup"
          p_ $
            meetup
              & (description :: Meetup -> Text)
              & asMarkdown
        firstEvent
          & fmap
            ( \event ->
                section_ [class_ "section section--next-event"] $ do
                  h2_ $ do
                    span_ "Next Meetup: "
                    small_ $ do
                      span_ (toHtml (local_date event))
                      span_ " "
                      span_ (toHtml (local_time event))
                  a_ [href_ (link event), target_ "blank"] "RSVP on Meetup"
            )
          & maybe (toHtml ("" :: Text)) id
        forM_ (maybe [] (: []) firstEvent) $ \event ->
          section_ [class_ "section section--events"] $ do
            article_ [class_ "event"] $ do
              h3_ $ do
                span_ (toHtml (name event))
                small_ $ do
                  span_ " "
                  span_ (toHtml (local_date event))
                  span_ " "
                  span_ (toHtml (local_time event))
              p_ $
                event
                  & (description :: Event -> Text)
                  & asMarkdown
        footer_ $ do
          span_ (toHtml ("last updated " ++ currentDate))
          span_ ", "
          a_
            [href_ "https://github.com/hannover-elm/website", target_ "blank"]
            "source code"
          span_ ", "
          a_
            [href_ "/static/imprint.txt", target_ "blank", rel_ "nofollow"]
            "imprint"
          span_ ", "
          a_
            [href_ "/static/datenschutz.txt", target_ "blank", rel_ "nofollow"]
            "datenschutz"

pageStyle :: Css
pageStyle = do
  C.html ? do
    C.fontFamily ["Roboto"] [C.sansSerif]
    C.color (C.rgba 0 0 0 0.87)
    C.backgroundColor (C.parse "#efefef")
  C.body ? do
    C.maxWidth (C.px 600)
    C.backgroundColor (C.white)
    C.padding (C.px 10) (C.px 10) (C.px 10) (C.px 10)
    C.boxSizing C.borderBox
  C.footer ? do
    C.fontSize (C.rem 0.875)
    C.marginTop (C.px 16)
    C.fontStyle C.italic
  C.header ? do
    C.borderRadius (C.px 2) (C.px 2) (C.px 2) (C.px 2)
    C.overflow C.hidden
    C.margin (C.px (-10)) (C.px (-10)) (C.px (-10)) (C.px (-10))
  "#logo" ? do
    C.backgroundColor (C.parse "#ccc")
