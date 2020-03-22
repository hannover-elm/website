{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Fail (MonadFail)
import Data.Aeson
import Data.Binary (Binary)
import Data.Function (on)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Hakyll
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)


main = do
  currentTime <- getCurrentTime
  hakyll $ do
    match "meetup.json" $ do
      route (constRoute "index.html")
      compile $ do
        events <- do
          fmap unfoldItem . withItemBody (pure . unEvents) =<< load "events.json"
        let
          upcomingEvents =
            take 1 (upcoming currentTime (closestFirst events))

          context =
            mconcat
              [ field "description" (pure . (description :: Meetup -> String) . itemBody)
              , listField "events" eventCtx (pure upcomingEvents) 
              , constField "currentDate" 
                  (formatTime defaultTimeLocale "%Y-%m-%d" currentTime)
              ]
        getResourceMeetup
          >>= loadAndApplyTemplate "src/index.html" context

    match "events.json" $ do
      compile getResourceEvents

    match "src/index.html" $ do
      compile templateCompiler

    match "index.css" $ do
      route idRoute
      compile copyFileCompiler

    match "src/imprint.txt" $ do
      route (constRoute "imprint.txt")
      compile copyFileCompiler

    match "src/datenschutz.txt" $ do
      route (constRoute "datenschutz.txt")
      compile copyFileCompiler

    match "src/robots.txt" $ do
      route (constRoute "robots.txt")
      compile copyFileCompiler


unfoldItem :: Item [a] -> [Item a]
unfoldItem item =
  map (Item (itemIdentifier item)) (itemBody item)


data Meetup = Meetup { description :: String }
  deriving (Generic, Show, Binary, Typeable)


instance Writable Meetup where
  write fn item =
    write fn (fmap encode item)


instance FromJSON Meetup


instance ToJSON Meetup


data Events = Events { unEvents :: [Event] }
  deriving (Generic, Show, Binary, Typeable)


instance Writable Events where
  write fn item =
    write fn (fmap encode item)


instance FromJSON Events where
  parseJSON = fmap Events . parseJSON


instance ToJSON Events where
  toJSON (Events events) = toJSON events


closestFirst :: [Item Event] -> [Item Event]
closestFirst =
  sortBy (comparing (getUTCTime . itemBody))


upcoming :: UTCTime -> [Item Event] -> [Item Event]
upcoming currentTime =
  filter ((<=) currentTime . getUTCTime . itemBody)


debug x =
  unsafePerformIO (putStrLn (show x) >> pure x)


data Event =
  Event
    { name :: String
    , local_date :: String
    , local_time :: String
    , description :: String
    , link :: String
    } deriving (Generic, Show, Binary, Typeable)


getUTCTime :: Event -> UTCTime
getUTCTime event =
  let
    dateString = local_date event ++ "T" ++ local_time event ++ ":00Z"
  in
  case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" dateString of
    Just utcTime ->
      utcTime

    Nothing ->
      UTCTime (ModifiedJulianDay 0) 0


instance FromJSON Event


instance ToJSON Event


eventCtx :: Context Event
eventCtx =
  mconcat
    [ field "name" (pure . name . itemBody)
    , field "local_date" (pure . local_date . itemBody)
    , field "local_time" (pure . local_time . itemBody)
    , field "description" (pure . (description :: Event -> String) . itemBody)
    , field "link" (pure . link . itemBody)
    ]


getResourceJson :: FromJSON a => Compiler (Item a)
getResourceJson = do
  item <- getResourceLBS
  withItemBody (\lbs ->
    case eitherDecode (itemBody item) of
      Right value -> return value
      Left err -> fail err
    )
    item


getResourceMeetup :: Compiler (Item Meetup)
getResourceMeetup = getResourceJson


getResourceEvents :: Compiler (Item Events)
getResourceEvents = getResourceJson
