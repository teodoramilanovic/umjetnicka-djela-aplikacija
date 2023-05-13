{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module BasicServer where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int (Int64)
import           Data.Text
import           Data.Proxy (Proxy(..))
import           Database.Persist (Key, Entity)
import           Database.Persist.Postgresql (ConnectionString)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Client
import           Servant.Server

import           Database
import           BasicSchema


type ArtAPI = 
       "artworks" :> Capture "artworksid" Int64 :> Get '[JSON] Artwork
  :<|> "artists" :> Capture "artistid" Int64 :> Get '[JSON] Artist    
  :<|> "artworks" :> Capture "artworktitle" Text :> Get '[JSON] [(Entity Artwork, Entity Artist, Entity Period)]
  :<|> "artists" :> Capture "artistname" Text :> Get '[JSON] [Entity Artist]
  :<|> "artworks" :> ReqBody '[JSON] Artwork :> Post '[JSON] Int64
  :<|> "artists" :> ReqBody '[JSON] Artist :> Post '[JSON] Int64
  :<|> "mediums" :> ReqBody '[JSON] Medium :> Post '[JSON] Int64
  :<|> "periods" :> ReqBody '[JSON] Period :> Post '[JSON] Int64
  :<|> "forms" :> ReqBody '[JSON] Form :> Post '[JSON] Int64
  :<|> "artworks" :> Get '[JSON] [(Entity Artwork, Entity Artist, Entity Period)]
  :<|> "paintings" :> Get '[JSON] [(Entity Artwork, Entity Artist, Entity Period)]
  :<|> "sculptures" :> Get '[JSON] [(Entity Artwork, Entity Artist, Entity Period)]
  :<|> "artists" :> Get '[JSON] [Entity Artist]
  :<|> "mediums" :> Get '[JSON] [Entity Medium]
  :<|> "periods" :> Get '[JSON] [Entity Period]
  :<|> "forms" :> Get '[JSON] [Entity Form]
  :<|> "artworks" :> "filter" :> Capture "period" Text :> Capture "artist" Text :> Capture "medium" Text :> Get '[JSON] [(Entity Artwork, Entity Artist, Entity Period)]
  :<|> "artworks" :> Capture "artworkid" Int64 :> Delete '[JSON] ()
  :<|> "artists" :> Capture "artistid" Int64 :> Delete '[JSON] ()
  :<|> "mediums" :> Capture "mediumid" Int64 :> Delete '[JSON] ()
  :<|> "periods" :> Capture "periodid" Int64 :> Delete '[JSON] ()
  :<|> "forms" :> Capture "formid" Int64 :> Delete '[JSON] ()
  :<|> "artworks" :> "sort" :> Capture "random" Int64 :> Get '[JSON] [(Entity Artwork, Entity Artist, Entity Period)]
  :<|> "artists" :> "sort" :> Capture "random" Int64 :> Get '[JSON] [Entity Artist]
  :<|> "paintings" :> "sort" :> Capture "random" Int64 :> Get '[JSON] [(Entity Artwork, Entity Artist, Entity Period)]
  :<|> "sculptures" :> "sort" :> Capture "random" Int64 :> Get '[JSON] [(Entity Artwork, Entity Artist, Entity Period)]
  :<|> "mediums" :> Capture "mediumid" Int64 :> Get '[JSON] Medium

artAPI :: Proxy ArtAPI
artAPI = Proxy :: Proxy ArtAPI

getArtworkByIdHandler :: ConnectionString -> Int64 -> Handler Artwork
getArtworkByIdHandler connString aid = do
  maybeArtwork <- liftIO $ getArtworkByIdPG connString aid
  case maybeArtwork of
    Just artwork -> return artwork
    Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find artwork with that ID" })
  
getArtistByIdHandler :: ConnectionString -> Int64 -> Handler Artist
getArtistByIdHandler connString aid = do
  maybeArtist <- liftIO $ getArtistByIdPG connString aid
  case maybeArtist of
    Just artist -> return artist
    Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find artist with that ID" })

getMediumByIdHandler :: ConnectionString -> Int64 -> Handler Medium
getMediumByIdHandler connString mid = do
  maybeMedium <- liftIO $ getMediumByIdPG connString mid
  case maybeMedium of
    Just medium -> return medium
    Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find medium with that ID" })

getArtworkHandler :: ConnectionString -> Text -> Handler [(Entity Artwork, Entity Artist, Entity Period)]
getArtworkHandler connString title = liftIO $ getArtworkPG connString title

getAllArtworksHandler :: ConnectionString -> Handler [(Entity Artwork, Entity Artist, Entity Period)]
getAllArtworksHandler connString = liftIO $ getAllArtworksPG connString

getAllPaintingsHandler :: ConnectionString -> Handler [(Entity Artwork, Entity Artist, Entity Period)]
getAllPaintingsHandler connString = liftIO $ getAllArtworksByFormIdPG connString 1

getAllSculpturesHandler :: ConnectionString -> Handler [(Entity Artwork, Entity Artist, Entity Period)]
getAllSculpturesHandler connString = liftIO $ getAllArtworksByFormIdPG connString 2

getArtistHandler :: ConnectionString -> Text -> Handler [Entity Artist]
getArtistHandler connString name = liftIO $ getArtistPG connString name

getAllArtistsHandler :: ConnectionString -> Handler [Entity Artist]
getAllArtistsHandler connString = liftIO $ getAllArtistsPG connString

getAllPeriodsHandler :: ConnectionString -> Handler [Entity Period]
getAllPeriodsHandler connString = liftIO $ getAllPeriodsPG connString

getAllMediumsHandler :: ConnectionString -> Handler [Entity Medium]
getAllMediumsHandler connString = liftIO $ getAllMediumsPG connString

getAllFormsHandler :: ConnectionString -> Handler [Entity Form]
getAllFormsHandler connString = liftIO $ getAllFormsPG connString

sortArtworksHandler :: ConnectionString -> Int64 -> Handler [(Entity Artwork, Entity Artist, Entity Period)]
sortArtworksHandler connString random = liftIO $ sortArtworksPG connString

sortArtistsHandler :: ConnectionString -> Int64 -> Handler [Entity Artist]
sortArtistsHandler connString random = liftIO $ sortArtistsPG connString

sortPaintingsHandler :: ConnectionString -> Int64 -> Handler [(Entity Artwork, Entity Artist, Entity Period)]
sortPaintingsHandler connString random = liftIO $ sortArtworksByFormIdPG connString 1

sortSculpturesHandler :: ConnectionString -> Int64 -> Handler [(Entity Artwork, Entity Artist, Entity Period)]
sortSculpturesHandler connString random = liftIO $ sortArtworksByFormIdPG connString 2

addArtworkHandler :: ConnectionString -> Artwork -> Handler Int64
addArtworkHandler connString artwork = liftIO $ addArtworkPG connString artwork

addArtistHandler :: ConnectionString -> Artist -> Handler Int64
addArtistHandler connString artist = liftIO $ addArtistPG connString artist

addPeriodHandler :: ConnectionString -> Period -> Handler Int64
addPeriodHandler connString period = liftIO $ addPeriodPG connString period

addMediumHandler :: ConnectionString -> Medium -> Handler Int64
addMediumHandler connString medium = liftIO $ addMediumPG connString medium

addFormHandler :: ConnectionString -> Form -> Handler Int64
addFormHandler connString form = liftIO $ addFormPG connString form

filterHandler :: ConnectionString -> Text -> Text -> Text -> Handler [(Entity Artwork, Entity Artist, Entity Period)]
filterHandler connString period artist medium = liftIO $ filterPG localConnString period artist medium

deleteArtworkHandler :: ConnectionString -> Int64 -> Handler ()
deleteArtworkHandler connString artid = liftIO $ deleteArtworkPG connString artid

deleteArtistHandler :: ConnectionString -> Int64 -> Handler ()
deleteArtistHandler connString aid = liftIO $ deleteArtistPG connString aid

deletePeriodHandler :: ConnectionString -> Int64 -> Handler ()
deletePeriodHandler connString pid = liftIO $ deletePeriodPG connString pid

deleteMediumHandler :: ConnectionString -> Int64 -> Handler ()
deleteMediumHandler connString mid = liftIO $ deleteMediumPG connString mid

deleteFormHandler :: ConnectionString -> Int64 -> Handler ()
deleteFormHandler connString fid = liftIO $ deleteFormPG connString fid

artServer :: ConnectionString -> Server ArtAPI
artServer connString = 
  (getArtworkByIdHandler connString) :<|>
  (getArtistByIdHandler connString) :<|>
  (getArtworkHandler connString) :<|>
  (getArtistHandler connString) :<|>
  (addArtworkHandler connString) :<|>
  (addArtistHandler connString) :<|>
  (addMediumHandler connString) :<|>
  (addPeriodHandler connString) :<|>
  (addFormHandler connString) :<|>
  (getAllArtworksHandler connString) :<|>
  (getAllPaintingsHandler connString) :<|>
  (getAllSculpturesHandler connString) :<|>
  (getAllArtistsHandler connString) :<|>
  (getAllMediumsHandler connString) :<|>
  (getAllPeriodsHandler connString) :<|>
  (getAllFormsHandler connString) :<|>
  (filterHandler connString) :<|>
  (deleteArtworkHandler connString) :<|>
  (deleteArtistHandler connString) :<|>
  (deleteMediumHandler connString) :<|>
  (deletePeriodHandler connString) :<|>
  (deleteFormHandler connString) :<|>
  (sortArtworksHandler connString) :<|>
  (sortArtistsHandler connString) :<|>
  (sortPaintingsHandler connString) :<|>
  (sortSculpturesHandler connString) :<|>
  (getMediumByIdHandler connString) 

runServer :: IO ()
runServer = run 5000 (serve artAPI (artServer localConnString))