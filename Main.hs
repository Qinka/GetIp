{-# LANGUAGE OverloadedStrings #-}




module Main
    (
    main
    ) where

      import Web.Scotty(scotty,get,file,setHeader)
      import Network.Info(getNetworkInterfaces,NetworkInterface(..))
      import System.Directory(getAppUserDataDirectory,createDirectory,doesDirectoryExist)
      import Control.Monad(unless)
      import Data.Time(getCurrentTime)
      import Control.Concurrent(threadDelay,forkIO)


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
        _ <- forkIO write
        scotty 7999 $
          get  "/" $ do
            setHeader "Content-Type" "application/json; charset=utf-8"
            file $ dataDir ++ "/ipv4"

      write :: IO ()
      write = do
        dataDir <- getAppUserDataDirectory "getip"
        netinfo <- getNetworkInterfaces
        let rt = getIp $ map (words.replace.show.ipv4) netinfo
        case rt of
          Nothing -> writeF (dataDir ++ "/ipv4") "CANNOT GET IPv4"
          Just x -> writeF (dataDir ++ "/ipv4") x
        threadDelay 60000000
        write


      writeF :: String -> String -> IO()
      writeF fn text = do
        time <- getCurrentTime
        writeFile fn $ "{\"ipv4\":\"" ++ text ++ "\",\"refresh\":\"" ++ show time ++ "\"}"
