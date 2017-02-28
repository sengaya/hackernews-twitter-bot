module Web.HnBot where

import           Control.Concurrent      (threadDelay)
import           Control.Monad           (unless)
import           Data.Aeson              (FromJSON (..))
import qualified Data.ByteString.Char8   as S8
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           System.Environment      (getEnv)
import           System.Exit             (exitFailure)
import           Web.HackerNews          (HackerNewsError, ItemId (..),
                                          TopStories (..), getItem,
                                          getTopStories)
import           Web.HnBot.Utils         (formatTweet, getSeenStories, seen,
                                          writeSeenStory)
import           Web.Twitter.Conduit     (Credential (..), OAuth (..),
                                          TWInfo (..), call, def, setCredential,
                                          twitterOAuth, update)

run :: IO ()
run = do
    mgr <- newManager tlsManagerSettings
    top_stories <- retry 10 5000000 $ getTopStories mgr
    let TopStories (s:_) = top_stories
    let story = s
    seen_stories <- getSeenStories
    unless (seen seen_stories story) $
        tweet story

retry :: FromJSON a => Int -> Int -> IO (Either HackerNewsError a) -> IO a
retry retries delay action = do
    request <- action
    case request of
        Left err -> if retries == 0
                    then print err >> exitFailure
                    else do
                        print ("Failed request: " ++ show err ++
                               " (retries left: " ++ show retries ++ ")")
                        threadDelay delay
                        retry (retries-1) delay action
        Right res -> return res

tweet :: ItemId -> IO ()
tweet story_id = do
    mgr <- newManager tlsManagerSettings
    story <- retry 10 5000000 (getItem mgr story_id)
    twInfo <- getTWInfoFromEnv
--    mgr <- newManager tlsManagerSettings
    _ <- call twInfo mgr $ update $ formatTweet story
    writeSeenStory story_id

getOAuthTokens :: IO (OAuth, Credential)
getOAuthTokens = do
    consumerKey <- getEnv' "OAUTH_CONSUMER_KEY"
    consumerSecret <- getEnv' "OAUTH_CONSUMER_SECRET"
    accessToken <- getEnv' "OAUTH_ACCESS_TOKEN"
    accessSecret <- getEnv' "OAUTH_ACCESS_SECRET"
    let oauth = twitterOAuth
            { oauthConsumerKey = consumerKey
            , oauthConsumerSecret = consumerSecret
            }
        cred = Credential
            [ (S8.pack "oauth_token", accessToken)
            , (S8.pack "oauth_token_secret", accessSecret)
            ]
    return (oauth, cred)
  where
    getEnv' = (S8.pack <$>) . getEnv

getTWInfoFromEnv :: IO TWInfo
getTWInfoFromEnv = do
    (oa, cred) <- getOAuthTokens
    return $ setCredential oa cred def
