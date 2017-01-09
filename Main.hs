{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Aeson
import Data.String.Conversions
import Data.Map (Map (..), fromList)
import qualified Data.Map as M
import Data.Maybe
import System.Process

import MachinesDialog

{-
    The goal for this script is to create a curses interface to visualize and enable a custom bash script to be executed on a remote machine
    This script reads thet json response from sigmyps machines endpoint (support/machines_search) and uses jq to filter and format the json response. 
    It also uses the brick package for the curses display.

curl -s -X POST -F 'like=turnstat-cruz' 'http://10.1.2.11:5000/support/machines_search' | jq '[.result[] | {vpn: .vpn, hostname: .hostname, office: .office_name, mac: .mac}]' 
-}

(??) = flip fromMaybe

concatValuesForKeys :: (Ord k) => [k] -> Map k String -> String
concatValuesForKeys k l = foldr (\x acc -> M.lookup x l <> Just " " <> acc) Nothing k ?? "Not found"

getValueForKey k l = M.lookup k l ?? "Not found"

flattenMaybeList = maybe [] id

searchSigmyp :: FromJSON a => String -> IO (Maybe a)
searchSigmyp searchString = do
    let standardOutput = readProcess "./parsesigmypjson.sh" [searchString] ""
    res <- (decode . cs) <$> standardOutput
    return res

searchSigmyp' :: String -> IO [Map String String]
searchSigmyp' s = (maybe [fromList [("error","No parse for json")]] id) <$>  searchSigmyp s




main :: IO ()
main = do
    putStrLn "\n Starting DyD Executioner \n"
    let getJsonValues = map (concatValuesForKeys ["error","hostname", "vpn"])
    let datasource ss = getJsonValues <$> (searchSigmyp' ss)
    runRemoteExecutor datasource $ \state -> do 
        --putStrLn "Selected: "
        --print $ getSelectedListElement state
        return ()

main2 :: IO ()
main2 = do
    putStrLn "\n Starting DyD Executioner \n"
    putStrLn "Enter search query for machines: "
    searchString <- getLine
    res <- searchSigmyp searchString
    let getJsonValues = map (concatValuesForKeys ["hostname", "vpn"])
    case (res :: Maybe [Map String String]) of
        Nothing -> putStrLn "Couldn't decode data"
        Just x -> if null x then putStrLn "No machines found" 
                            else 
                                renderDialog (getJsonValues x) $ \state -> do 
                                    putStrLn "Selected: "
                                    --print $ getSelectedListElement state
                                    --putStrLn  $ "Final state is: " ++ show (state)

