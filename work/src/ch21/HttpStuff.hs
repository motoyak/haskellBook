module HttpStuff where

import Data.ByteString.Lazy hiding (map)
import Network.Wreq

urls :: [String]
urls = [ "https://httpbin.org/ip"
       , "https://httpbin.org/bytes/5"
       ]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls