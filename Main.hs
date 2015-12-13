{-# LANGUAGE OverloadedStrings #-}




module Main
    (
    main
    ) where

      import Web.Scotty(scotty,get,setHeader,text)
      import Network.Info(getNetworkInterfaces,NetworkInterface(..))
      import System.Directory(getAppUserDataDirectory,createDirectory,doesDirectoryExist)
      import Control.Monad(unless)
      import Control.Monad.IO.Class(liftIO)
      import Data.String(fromString)


      replace :: String -> String
      replace [] = []
      replace ('.':xs) = ' ':replace xs
      replace (x:xs) = x:replace xs
      getIp :: [[String]] -> Maybe String
      getIp [] = Nothing
      getIp (x:xs) = if (head.head) x== '2' then Just $ tail $ concatMap ("."++) x else getIp xs


      main :: IO ()
      main = do
        dataDir <- getAppUserDataDirectory "getip"
        doesDirectoryExist dataDir >>= (\x -> unless x $ createDirectory dataDir)
        putStrLn "GetIp server begin"
        scotty 7999 $
          get  "/" $ do
            setHeader "Content-Type" "application/json; charset=utf-8"
            netinfo <- liftIO getNetworkInterfaces
            let rt = getIp $ map (words.replace.show.ipv4) netinfo
            case rt of
              Nothing -> do
                liftIO $ putStrLn "CANNOT GET IPV$4"
                text $"{\"status\":\"failed\",\"reason\":\"CANNOT GET IPV4\"}"
              Just x -> do
                liftIO $ putStrLn x
                text $ fromString $ "{\"status\":\"success\",\"ipv4\":\""++x++"\"}"
