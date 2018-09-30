{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Web.Scotty
import Data.FileEmbed
import WaiAppStatic.Storage.Embedded
import Control.Monad.IO.Class
import Network.Wai.Handler.Warp
import Data.ByteString hiding (map)
import WaiAppStatic.Types
import Network.Wai.Application.Static
import Network.Wai
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as C

type FileLookup = [(FilePath, Data.ByteString.ByteString)]

staticFiles :: ByteString -> FileLookup -> Middleware
staticFiles prefix ds app req cb = do
  print (rawPathInfo req)
  if S.member path paths
    then dirApp req cb
    else app    req cb

  where
  path :: ByteString
  path = rawPathInfo req

  paths :: S.Set ByteString
  paths = S.fromList (map ((prefix <>) . C.pack . fst) ds)

  dirSettings :: StaticSettings
  dirSettings = embeddedSettings ds

  dirApp :: Application
  dirApp = staticApp dirSettings


myDir :: FileLookup
myDir = $(embedDir "testfiles")

main :: IO ()
main = do
  print myDir
  app <- scottyApp $ do
    middleware (staticFiles "/" myDir)
    get "/" $ do
      liftIO $ print myDir

  runEnv 3333 app
