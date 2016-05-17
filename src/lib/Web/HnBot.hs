module Web.HnBot where

import Data.Aeson (FromJSON(..))
import Control.Monad (unless)
import Control.Concurrent (threadDelay)
import System.Environment (getEnv)
import System.Exit (exitFailure)
import Web.HnBot.Utils (formatTweet, seen, getSeenStories, writeSeenStory)
import Web.HackerNews (getStory, getTopStories, hackerNews, HackerNews,
                       StoryId(..), TopStories(..))
import Web.Twitter.Conduit (call, def, newManager, setCredential,
                            tlsManagerSettings, twitterOAuth, update,
                            Credential(..), OAuth(..), TWInfo(..))
import qualified Data.ByteString.Char8 as S8

run :: IO ()
run = do
    top_stories <- retry 10 5000000 getTopStories
    let TopStories (s:_) = top_stories
    let story = StoryId s
    seen_stories <- getSeenStories
    unless (seen seen_stories story) $
        tweet story

retry :: FromJSON a => Int -> Int -> HackerNews a -> IO a
retry retries delay action = do
    request <- hackerNews action
    case request of
        Left err -> if retries == 0
                    then print err >> exitFailure
                    else do
                        print ("Failed request: " ++ show err ++
                               " (retries left: " ++ show retries ++ ")")
                        threadDelay delay
                        retry (retries-1) delay action
        Right res -> return res

tweet :: StoryId -> IO ()
tweet story_id = do
    story <- retry 10 5000000 (getStory story_id)
    twInfo <- getTWInfoFromEnv
    mgr <- newManager tlsManagerSettings
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
