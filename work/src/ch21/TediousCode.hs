module TediousCode where

data Query =
  Query

data SomeObj =
  SomeObj

data IoOnlyObj =
  IoOnlyObj

data Err =
  Err

-- decoder function that makes some object from String
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

-- query function that runs against the DB and returns array of String
fetchFn :: Query -> IO [String]
fetchFn = undefined

-- context initializer that has IO
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err) -> return $ Left err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a

pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' query = do
  a <- fetchFn query
  traverse makeIoOnlyObj (mapM decodeFn a)
  
pipelineFn'' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn'' = ((traverse makeIoOnlyObj . mapM decodeFn) =<<) . fetchFn

pipelineFn''' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn''' = ((traverse makeIoOnlyObj . traverse decodeFn) =<<) . fetchFn