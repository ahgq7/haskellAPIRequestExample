{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Concurrent
import Data.Aeson
import Network.HTTP.Simple
import System.Console.ANSI

data LatiNLong = LatiNLong String String
instance FromJSON LatiNLong where
    parseJSON (Object v) = do
        iss <- v .: "iss_position" 
        
        latitude <- iss .: "latitude" 
        longitude <- iss .: "longitude"
        
        return (LatiNLong latitude longitude)
        

data AstroNumber = AstroNumber Int
instance FromJSON AstroNumber where
    parseJSON (Object v) = do
        number <- v.: "number"
        
        return (AstroNumber number)



getTheInfo :: IO ()
getTheInfo = do
    response <- httpJSON "http://api.open-notify.org/iss-now.json" :: IO (Response LatiNLong)
    response2 <- httpJSON "http://api.open-notify.org/astros.json" :: IO (Response AstroNumber)
    case getResponseBody response of
        LatiNLong latitude longitude -> do
            putStrLn $ id "ISS's current location  is:"
            putStrLn $ id ("Latitude: " ++ latitude ++ ", Longitude: " ++ longitude)
    case getResponseBody response2 of
        AstroNumber number ->
            putStrLn $ id ("Currently, " ++ show number ++ " humans are in space.")
            
printingFunc :: IO ()
printingFunc = do
    getTheInfo
    
    threadDelay 1000000
    
    clearFromCursorToScreenBeginning
    cursorUpLine(3)

main :: IO ()
main = do
    clearFromCursorToScreenBeginning
    cursorUpLine(222)
    
    forever $ printingFunc



