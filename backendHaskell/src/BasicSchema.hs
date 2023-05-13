{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module BasicSchema where

import           Data.Aeson
import           Data.Aeson.Types
import           Database.Persist (Entity(..), Entity)
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import qualified Database.Persist.TH as PTH
import           Data.Text (Text)


PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|

  Artwork sql=artwork
    title Text
    formId FormId
    artistId ArtistId
    location Text
    periodId PeriodId
    dimensions Text
    mediumId MediumId
    picture Text
    description Text
    deriving Show Read Eq

  Artist sql=artist
    name Text
    yearOfBirth Int
    yearOfDeath Int
    nationality Text
    picture Text
    description Text
    deriving Show Read Eq

  Period sql=period
    name Text
    deriving Show Read Eq

  Medium sql=medium
    name Text
    deriving Show Read Eq

  Form sql=form
    name Text
    deriving Show Read Eq
|]

-------------------------------------------------------------------

instance ToJSON (Entity Artwork) where
  toJSON (Entity artid artwork) = object $
    "id" .= (fromSqlKey artid) : artworkPairs artwork

instance ToJSON Artwork where
  toJSON artwork = object (artworkPairs artwork)

artworkPairs :: Artwork -> [Pair]
artworkPairs artwork =
  [ "title" .= artworkTitle artwork
  , "formId" .= artworkFormId artwork
  , "artistId" .= artworkArtistId artwork
  , "location" .= artworkLocation artwork
  , "periodId" .= artworkPeriodId artwork
  , "dimensions" .= artworkDimensions artwork
  , "mediumId" .= artworkMediumId artwork
  , "picture" .= artworkPicture artwork
  , "description" .= artworkDescription artwork
  ]

instance FromJSON (Entity Artwork) where
  parseJSON = withObject "Artwork Entity" $ \o -> do
    artwork <- parseArtwork o
    artid <- o .: "id"
    return $ Entity (toSqlKey artid) artwork

instance FromJSON Artwork where
  parseJSON = withObject "Artwork" parseArtwork
  
parseArtwork :: Object -> Parser Artwork
parseArtwork o = do
  uTitle <- o .: "title"
  uFormId <- o .: "formId"
  uArtistId <- o .: "artistId"
  uLocation <- o .: "location"
  uPeriodId <- o .: "periodId"
  uDimensions <- o .: "dimensions"
  uMediumId <- o .: "mediumId"
  uPicture <- o .: "picture"
  uDescription <- o .: "description"
  return Artwork
    { artworkTitle = uTitle
    , artworkFormId = uFormId
    , artworkArtistId = uArtistId
    , artworkLocation = uLocation
    , artworkPeriodId = uPeriodId
    , artworkDimensions = uDimensions
    , artworkMediumId = uMediumId
    , artworkPicture = uPicture
    , artworkDescription = uDescription
    }

--------------------------------------------------------------

instance ToJSON (Entity Medium) where
  toJSON (Entity mid medium) = object $
    "id" .= (fromSqlKey mid) : mediumPairs medium

instance ToJSON Medium where
  toJSON medium = object (mediumPairs medium)

mediumPairs :: Medium -> [Pair]
mediumPairs medium =
  [ "name" .= mediumName medium
  ]

instance FromJSON (Entity Medium) where
  parseJSON = withObject "Medium Entity" $ \o -> do
    medium <- parseMedium o
    mid <- o .: "id"
    return $ Entity (toSqlKey mid) medium

instance FromJSON Medium where
  parseJSON = withObject "Medium" parseMedium

parseMedium :: Object -> Parser Medium
parseMedium o = do
  uName <- o .: "name"
  return Medium
    { mediumName = uName
    }

-----------------------------------------------------------------

instance ToJSON (Entity Artist) where
  toJSON (Entity aid artist) = object $
    "id" .= (fromSqlKey aid) : artistPairs artist

instance ToJSON Artist where
  toJSON artist = object (artistPairs artist)

artistPairs :: Artist -> [Pair]
artistPairs artist =
  [ "name" .= artistName artist
  , "yearOfBirth" .= artistYearOfBirth artist
  , "yearOfDeath" .= artistYearOfDeath artist
  , "nationality" .= artistNationality artist
  , "picture" .= artistPicture artist
  , "description" .= artistDescription artist
  ]

instance FromJSON (Entity Artist) where
  parseJSON = withObject "Artist Entity" $ \o -> do
    artist <- parseArtist o
    aid <- o .: "id"
    return $ Entity (toSqlKey aid) artist

instance FromJSON Artist where
  parseJSON = withObject "Artist" parseArtist

parseArtist :: Object -> Parser Artist
parseArtist o = do
  uName <- o .: "name"
  uYearOfBirth <- o .: "yearOfBirth"
  uYearOfDeath <- o .: "yearOfDeath"
  uNationality <- o .: "nationality"
  uPicture <- o .: "picture"
  uDescription <- o .: "description"
  return Artist
    { artistName = uName
    , artistYearOfBirth = uYearOfBirth
    , artistYearOfDeath = uYearOfDeath
    , artistNationality = uNationality
    , artistPicture = uPicture
    , artistDescription = uDescription
    }

-----------------------------------------------------------------

instance ToJSON (Entity Period) where
  toJSON (Entity pid period) = object $
    "id" .= (fromSqlKey pid) : periodPairs period

instance ToJSON Period where
  toJSON period = object (periodPairs period)

periodPairs :: Period -> [Pair]
periodPairs period =
  [ "name" .= periodName period
  ]

instance FromJSON (Entity Period) where
  parseJSON = withObject "Period Entity" $ \o -> do
    period <- parsePeriod o
    pid <- o .: "id"
    return $ Entity (toSqlKey pid) period

instance FromJSON Period where
  parseJSON = withObject "Period" parsePeriod

parsePeriod :: Object -> Parser Period
parsePeriod o = do
  uName <- o .: "name"
  return Period
    { periodName = uName
    }

--------------------------------------------------------------

instance ToJSON (Entity Form) where
  toJSON (Entity fid form) = object $
    "id" .= (fromSqlKey fid) : formPairs form

instance ToJSON Form where
  toJSON form = object (formPairs form)

formPairs :: Form -> [Pair]
formPairs form =
  [ "name" .= formName form
  ]

instance FromJSON (Entity Form) where
  parseJSON = withObject "Form Entity" $ \o -> do
    form <- parseForm o
    fid <- o .: "id"
    return $ Entity (toSqlKey fid) form

instance FromJSON Form where
  parseJSON = withObject "Form" parseForm

parseForm :: Object -> Parser Form
parseForm o = do
  uName <- o .: "name"
  return Form
    { formName = uName
    }