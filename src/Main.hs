{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Lucid
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import System.Random (randomRIO)
import qualified Data.Map as M
import qualified Data.Text.Lazy as LT

-- Main entry point
main :: IO ()
main = do
    urlMap <- newMVar M.empty
    scotty 3000 $ app urlMap

-- Scotty application
app :: MVar (M.Map LT.Text LT.Text) -> ScottyM ()
app urlMap = do
    get "/" $ html $ renderText homePage
    post "/shorten" $ shortenHandler urlMap
    get "/list" $ listHandler urlMap
    get "/:shortId" $ redirectHandler urlMap

-- Simple HTML form using Lucid
homePage :: Html ()
homePage = html_ $ body_ $ do
    h1_ "Simple Haskell URL Shortener"
    p_ "Enter a URL to shorten:"
    form_ [method_ "post", action_ "/shorten"] $ do
        input_ [type_ "text", name_ "url"]
        button_ [type_ "submit"] "Shorten"

-- URL shortening handler
shortenHandler :: MVar (M.Map LT.Text LT.Text) -> ActionM ()
shortenHandler urlMap = do
    url <- formParam "url"
    shortId <- liftIO generateShortId
    liftIO $ modifyMVar_ urlMap (return . M.insert shortId url)
    text $ "Shortened URL: http://localhost:3000/" <> shortId

-- List all shortened URLs
listHandler :: MVar (M.Map LT.Text LT.Text) -> ActionM ()
listHandler urlMap = do
    db <- liftIO $ readMVar urlMap
    json $ M.toList db

-- Redirect handler
redirectHandler :: MVar (M.Map LT.Text LT.Text) -> ActionM ()
redirectHandler urlMap = do
    shortId <- param "shortId"
    db <- liftIO $ readMVar urlMap
    case M.lookup shortId db of
        Just url -> redirect url
        Nothing  -> text "URL not found."

-- Generate a short random ID
generateShortId :: IO LT.Text
generateShortId = LT.pack <$> replicateM 6 randomChar
  where
    chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    randomChar = (chars !!) <$> randomRIO (0, length chars - 1)
