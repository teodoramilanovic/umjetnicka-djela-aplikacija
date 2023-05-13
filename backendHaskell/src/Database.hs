{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Data.Aeson
import           Data.Aeson.Types
import           Control.Monad.Logger
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Int (Int64)
import           Data.Text (Text, length, drop)
import           Prelude hiding (length, drop)
import           Database.Persist hiding ((==.))
import           Database.Persist.Postgresql hiding ((==.))
import           Database.Esqueleto (select, from, where_, (^.), val, (==.), on,
                                     InnerJoin(..), limit, orderBy, desc, asc)

import BasicSchema


type PGInfo = ConnectionString

localConnString :: PGInfo
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=art password=postgres"

runAction :: PGInfo -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connectionString $ \backend ->
   runReaderT action backend

migrateDB :: PGInfo -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False

-----------------------------------------------------

getArtworkByIdPG :: PGInfo -> Int64 -> IO (Maybe Artwork)
getArtworkByIdPG connString aid = runAction connString (get (toSqlKey aid))

getArtistByIdPG :: PGInfo -> Int64 -> IO (Maybe Artist)
getArtistByIdPG connString aid = runAction connString (get (toSqlKey aid))

getArtworkPG :: PGInfo -> Text -> IO [(Entity Artwork, Entity Artist, Entity Period)]
getArtworkPG connString title = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [(Entity Artwork, Entity Artist, Entity Period)]
    fetchAction = select . from $ \(artwork `InnerJoin` artist `InnerJoin` period `InnerJoin` medium) -> do
      on (artist ^. ArtistId  ==. artwork ^. ArtworkArtistId)
      on (period ^. PeriodId  ==. artwork ^. ArtworkPeriodId)
      on (medium ^. MediumId  ==. artwork ^. ArtworkMediumId)
      where_ (artwork ^. ArtworkTitle ==. val title)
      return (artwork, artist, period)


getArtistPG :: PGInfo -> Text -> IO [Entity Artist]
getArtistPG connString name = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Artist]
    fetchAction = select . from $ \artist -> do
      where_ (artist ^. ArtistName ==. val name)
      return artist

getMediumByIdPG :: PGInfo -> Int64 -> IO (Maybe Medium)
getMediumByIdPG connString mid = runAction connString (get (toSqlKey mid))

getAllArtworksPG :: PGInfo -> IO [(Entity Artwork, Entity Artist, Entity Period)]
getAllArtworksPG connString = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [(Entity Artwork, Entity Artist, Entity Period)]
    fetchAction = select . from $ \(artwork `InnerJoin` artist `InnerJoin` period `InnerJoin` medium) -> do
      on (artist ^. ArtistId  ==. artwork ^. ArtworkArtistId)
      on (period ^. PeriodId  ==. artwork ^. ArtworkPeriodId)
      on (medium ^. MediumId  ==. artwork ^. ArtworkMediumId)
      return (artwork, artist, period)

getAllArtworksByFormIdPG :: PGInfo -> Int64  -> IO [(Entity Artwork, Entity Artist, Entity Period)]
getAllArtworksByFormIdPG connString fid = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [(Entity Artwork, Entity Artist, Entity Period)]
    fetchAction = select . from $ \(artwork `InnerJoin` artist `InnerJoin` period `InnerJoin` medium) -> do
      on (artist ^. ArtistId  ==. artwork ^. ArtworkArtistId)
      on (period ^. PeriodId  ==. artwork ^. ArtworkPeriodId)
      on (medium ^. MediumId  ==. artwork ^. ArtworkMediumId)
      where_ (artwork ^. ArtworkFormId ==. val (toSqlKey fid))
      return (artwork, artist, period)

sortArtworksByFormIdPG :: PGInfo -> Int64  -> IO [(Entity Artwork, Entity Artist, Entity Period)]
sortArtworksByFormIdPG connString fid = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [(Entity Artwork, Entity Artist, Entity Period)]
    fetchAction = select . from $ \(artwork `InnerJoin` artist `InnerJoin` period `InnerJoin` medium) -> do
      on (artist ^. ArtistId  ==. artwork ^. ArtworkArtistId)
      on (period ^. PeriodId  ==. artwork ^. ArtworkPeriodId)
      on (medium ^. MediumId  ==. artwork ^. ArtworkMediumId)
      where_ (artwork ^. ArtworkFormId ==. val (toSqlKey fid))
      orderBy [asc (artwork ^. ArtworkTitle)]
      return (artwork, artist, period)

getAllArtistsPG :: PGInfo -> IO [Entity Artist]
getAllArtistsPG connString = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Artist]
    fetchAction = select . from $ \artist -> do
      return artist
  
getAllMediumsPG :: PGInfo -> IO [Entity Medium]
getAllMediumsPG connString = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Medium]
    fetchAction = select . from $ \medium -> do
      return medium

getAllPeriodsPG :: PGInfo -> IO [Entity Period]
getAllPeriodsPG connString = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Period]
    fetchAction = select . from $ \period -> do
      return period

getAllFormsPG :: PGInfo -> IO [Entity Form]
getAllFormsPG connString = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Form]
    fetchAction = select . from $ \form -> do
      return form

filter1PPG :: PGInfo -> Text -> Text -> Text -> IO [(Entity Artwork, Entity Artist, Entity Period)]
filter1PPG connString period1 artist1 medium1 = runAction connString fetchAction
    where
      fetchAction :: SqlPersistT (LoggingT IO) [(Entity Artwork, Entity Artist, Entity Period)]
      fetchAction = select . from $ \(artwork `InnerJoin` artist `InnerJoin` period `InnerJoin` medium) -> do
        on (artist ^. ArtistId  ==. artwork ^. ArtworkArtistId)
        on (period ^. PeriodId  ==. artwork ^. ArtworkPeriodId)
        on (medium ^. MediumId  ==. artwork ^. ArtworkMediumId)
        where_ (period ^. PeriodName ==. val (drop 7 period1))
        return (artwork, artist, period)

filter1APG :: PGInfo -> Text -> Text -> Text -> IO [(Entity Artwork, Entity Artist, Entity Period)]
filter1APG connString period1 artist1 medium1 = runAction connString fetchAction
    where
      fetchAction :: SqlPersistT (LoggingT IO) [(Entity Artwork, Entity Artist, Entity Period)]
      fetchAction = select . from $ \(artwork `InnerJoin` artist `InnerJoin` period `InnerJoin` medium) -> do
        on (artist ^. ArtistId  ==. artwork ^. ArtworkArtistId)
        on (period ^. PeriodId  ==. artwork ^. ArtworkPeriodId)
        on (medium ^. MediumId  ==. artwork ^. ArtworkMediumId)
        where_ (artist ^. ArtistName ==. val (drop 7 artist1))
        return (artwork, artist, period)

filter1MPG :: PGInfo -> Text -> Text -> Text -> IO [(Entity Artwork, Entity Artist, Entity Period)]
filter1MPG connString period1 artist1 medium1 = runAction connString fetchAction
    where
      fetchAction :: SqlPersistT (LoggingT IO) [(Entity Artwork, Entity Artist, Entity Period)]
      fetchAction = select . from $ \(artwork `InnerJoin` artist `InnerJoin` period `InnerJoin` medium) -> do
        on (artist ^. ArtistId  ==. artwork ^. ArtworkArtistId)
        on (period ^. PeriodId  ==. artwork ^. ArtworkPeriodId)
        on (medium ^. MediumId  ==. artwork ^. ArtworkMediumId)
        where_ (medium ^. MediumName ==. val (drop 7 medium1))
        return (artwork, artist, period)

filter1PG :: PGInfo -> Text -> Text -> Text -> IO [(Entity Artwork, Entity Artist, Entity Period)]
filter1PG connString period1 artist1 medium1 = if (length period1 > 7 ) then (filter1PPG connString period1 artist1 medium1) 
else if (length artist1 > 7) then (filter1APG connString period1 artist1 medium1) 
else (filter1MPG connString period1 artist1 medium1)

filter2APPG :: PGInfo -> Text -> Text -> Text -> IO [(Entity Artwork, Entity Artist, Entity Period)]
filter2APPG connString period1 artist1 medium1 = runAction connString fetchAction
    where
      fetchAction :: SqlPersistT (LoggingT IO) [(Entity Artwork, Entity Artist, Entity Period)]
      fetchAction = select . from $ \(artwork `InnerJoin` artist `InnerJoin` period `InnerJoin` medium) -> do
        on (artist ^. ArtistId  ==. artwork ^. ArtworkArtistId)
        on (period ^. PeriodId  ==. artwork ^. ArtworkPeriodId)
        on (medium ^. MediumId  ==. artwork ^. ArtworkMediumId)
        where_ (period ^. PeriodName ==. val (drop 7 period1))
        where_ (artist ^. ArtistName ==. val (drop 7 artist1))
        return (artwork, artist, period)

filter2AMPG :: PGInfo -> Text -> Text -> Text -> IO [(Entity Artwork, Entity Artist, Entity Period)]
filter2AMPG connString period1 artist1 medium1 = runAction connString fetchAction
    where
      fetchAction :: SqlPersistT (LoggingT IO) [(Entity Artwork, Entity Artist, Entity Period)]
      fetchAction = select . from $ \(artwork `InnerJoin` artist `InnerJoin` period `InnerJoin` medium) -> do
        on (artist ^. ArtistId  ==. artwork ^. ArtworkArtistId)
        on (period ^. PeriodId  ==. artwork ^. ArtworkPeriodId)
        on (medium ^. MediumId  ==. artwork ^. ArtworkMediumId)
        where_ (artist ^. ArtistName ==. val (drop 7 artist1))
        where_ (medium ^. MediumName ==. val (drop 7 medium1))
        return (artwork, artist, period)

filter2PMPG :: PGInfo -> Text -> Text -> Text -> IO [(Entity Artwork, Entity Artist, Entity Period)]
filter2PMPG connString period1 artist1 medium1 = runAction connString fetchAction
    where
      fetchAction :: SqlPersistT (LoggingT IO) [(Entity Artwork, Entity Artist, Entity Period)]
      fetchAction = select . from $ \(artwork `InnerJoin` artist `InnerJoin` period `InnerJoin` medium) -> do
        on (artist ^. ArtistId  ==. artwork ^. ArtworkArtistId)
        on (period ^. PeriodId  ==. artwork ^. ArtworkPeriodId)
        on (medium ^. MediumId  ==. artwork ^. ArtworkMediumId)
        where_ (medium ^. MediumName ==. val (drop 7 medium1))
        where_ (period ^. PeriodName ==. val (drop 7 period1))
        return (artwork, artist, period)

filter2PG :: PGInfo -> Text -> Text -> Text -> IO [(Entity Artwork, Entity Artist, Entity Period)]
filter2PG connString period1 artist1 medium1 = if (length period1 > 7 && length artist1 > 7 ) then (filter2APPG connString period1 artist1 medium1) 
else if (length artist1 > 7 && length medium1 > 7) then (filter2AMPG connString period1 artist1 medium1) 
else (filter2PMPG connString period1 artist1 medium1)

filter3PG :: PGInfo -> Text -> Text -> Text -> IO [(Entity Artwork, Entity Artist, Entity Period)]
filter3PG connString period1 artist1 medium1 = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [(Entity Artwork, Entity Artist, Entity Period)]
    fetchAction = select . from $ \(artwork `InnerJoin` artist `InnerJoin` period `InnerJoin` medium) -> do
      on (artist ^. ArtistId  ==. artwork ^. ArtworkArtistId)
      on (period ^. PeriodId  ==. artwork ^. ArtworkPeriodId)
      on (medium ^. MediumId  ==. artwork ^. ArtworkMediumId)
      where_ (period ^. PeriodName ==. val (drop 7 period1))
      where_ (artist ^. ArtistName ==. val (drop 7 artist1))
      where_ (medium ^. MediumName ==. val (drop 7 medium1))
      return (artwork, artist, period)


checkNumber :: [Text] -> Int
checkNumber xs = sum [1 | x <- xs, (length x) > 7]

filterPG :: PGInfo -> Text -> Text -> Text -> IO [(Entity Artwork, Entity Artist, Entity Period)]
filterPG connString period artist medium = let listOfValues = [period,artist,medium] in
  if (checkNumber listOfValues == 1) then filter1PG connString period artist medium
  else if (checkNumber listOfValues == 2) then filter2PG connString period artist medium
  else if (checkNumber listOfValues == 3) then filter3PG connString period artist medium
  else getAllArtworksPG connString

addArtworkPG :: PGInfo -> Artwork -> IO Int64 
addArtworkPG connString artwork = fromSqlKey <$> runAction connString (insert artwork)

deleteArtworkPG :: PGInfo -> Int64 -> IO ()
deleteArtworkPG connString artid = runAction connString (delete artworkKey)
  where
    artworkKey :: Key Artwork
    artworkKey = toSqlKey artid

addArtistPG :: PGInfo -> Artist -> IO Int64 
addArtistPG connString artist = fromSqlKey <$> runAction connString (insert artist)

deleteArtistPG :: PGInfo -> Int64 -> IO ()
deleteArtistPG connString aid = runAction connString (delete artistKey)
  where
    artistKey :: Key Artist
    artistKey = toSqlKey aid

addMediumPG :: PGInfo -> Medium -> IO Int64 
addMediumPG connString medium = fromSqlKey <$> runAction connString (insert medium)

deleteMediumPG :: PGInfo -> Int64 -> IO ()
deleteMediumPG connString mid = runAction connString (delete mediumKey)
  where
    mediumKey :: Key Medium
    mediumKey = toSqlKey mid

addPeriodPG :: PGInfo -> Period -> IO Int64 
addPeriodPG connString period = fromSqlKey <$> runAction connString (insert period)

deletePeriodPG :: PGInfo -> Int64 -> IO ()
deletePeriodPG connString pid = runAction connString (delete periodKey)
  where
    periodKey :: Key Period
    periodKey = toSqlKey pid

addFormPG :: PGInfo -> Form -> IO Int64 
addFormPG connString form = fromSqlKey <$> runAction connString (insert form)

deleteFormPG :: PGInfo -> Int64 -> IO ()
deleteFormPG connString fid = runAction connString (delete formKey)
  where
    formKey :: Key Form
    formKey = toSqlKey fid

sortArtworksPG :: PGInfo -> IO [(Entity Artwork , Entity Artist, Entity Period)]
sortArtworksPG connString = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [(Entity Artwork , Entity Artist, Entity Period)]
    fetchAction = select . from $ \(artwork `InnerJoin` artist `InnerJoin` period `InnerJoin` medium) -> do
      on (artist ^. ArtistId  ==. artwork ^. ArtworkArtistId)
      on (period ^. PeriodId  ==. artwork ^. ArtworkPeriodId)
      on (medium ^. MediumId  ==. artwork ^. ArtworkMediumId)
      orderBy [asc (artwork ^. ArtworkTitle)]
      return (artwork, artist, period)

sortArtistsPG :: PGInfo -> IO [Entity Artist]
sortArtistsPG connString = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Artist]
    fetchAction = select . from $ \artist -> do
      orderBy [asc (artist ^. ArtistName)]
      return artist