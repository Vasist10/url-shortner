{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Lucid
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import System.Random (randomRIO)
import qualified Data.Map as M
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T


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
homePage = html_ $ do
    head_ $ do
        title_ "Haskell URL Shortener"
        link_ [ rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css" ]
    body_ [class_ "flex flex-col items-center justify-center min-h-screen bg-gray-100"] $ do
        div_ [class_ "bg-white p-8 rounded-lg shadow-md w-96"] $ do
            h1_ [class_ "text-2xl font-bold text-gray-800 mb-4 text-center"] "Haskell URL Shortener"
            p_ [class_ "text-gray-600 mb-4 text-center"] "Enter a URL to shorten:"
            form_ [method_ "post", action_ "/shorten", class_ "flex flex-col space-y-4"] $ do
                input_ [type_ "text", name_ "url", placeholder_ "Enter URL...", class_ "p-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"]
                button_ [type_ "submit", class_ "bg-blue-500 text-white py-2 rounded-md hover:bg-blue-600 transition"] "Shorten"

-- URL shortening handler
shortenHandler :: MVar (M.Map LT.Text LT.Text) -> ActionM ()
shortenHandler urlMap = do
    url <- formParam "url"
    shortId <- liftIO generateShortId
    liftIO $ modifyMVar_ urlMap (return . M.insert shortId url)
    html $ renderText $ html_ $ body_ [class_ "flex flex-col items-center justify-center min-h-screen bg-gray-100"] $ do
        div_ [class_ "bg-white p-6 rounded-lg shadow-md w-96 text-center"] $ do
            h2_ [class_ "text-lg font-semibold text-gray-700"] "Your Shortened URL:"
            p_ [class_ "mt-2 text-blue-500 font-mono"] $ 
                a_ [href_ (T.pack $ LT.unpack $ LT.append "http://localhost:3000/" shortId)] 
                   (toHtml $ "http://localhost:3000/" <> shortId)
            a_ [href_ "/", class_ "mt-4 inline-block text-gray-600 hover:text-blue-500"] "Shorten another URL"



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
