module Web.HnBot.Utils where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.IO (IOMode(AppendMode), openFile, hClose, hPrint)
import Web.HackerNews (StoryId(..), Story(..))
import qualified Data.Text as T

-- tweet looks like this:
--   Story title <story_url> (<comment_url>)
-- or if it is a story on HN (e.g. "Ask HN", etc.):
--   Story title <comment_url>
formatTweet :: Story -> Text
formatTweet story
    | story_url == T.empty = T.append new_title $ T.pack $ ' ' : comment_url
    | otherwise = T.append new_title $ T.append (T.pack " ") $ T.append story_url $ T.pack $ " (" ++ comment_url ++ ")"
    where title = storyTitle story
          story_url = fromMaybe T.empty $ storyUrl story
          StoryId story_id = storyId story
          comment_url = "https://news.ycombinator.com/item?id=" ++ show story_id
          new_title = shortTitle title story_url

shortTitle :: Text -> Text -> Text
shortTitle title story_url
    | story_url == T.empty && title_length > needed_without_story_url = cut_title needed_without_story_url
    | story_url /= T.empty && title_length > needed_with_story_url = cut_title needed_with_story_url
    | otherwise = title
    where title_length = T.length title
          -- all URLs are (currently) counted as 23 characters: https://support.twitter.com/articles/78124#
          -- 140 - space - story_url - space - "(" - comment_url - ")"
          needed_with_story_url = 140 - 1 - 23- 2 - 23 - 1
          -- 140 - space - comment_url
          needed_without_story_url = 140 - 1 - 23
          -- " ..."
          dots = 4
          cut_title chars_needed = T.append (trim $ T.take (chars_needed - dots) title) (T.pack " ...")

trim :: Text -> Text
trim = T.reverse . T.dropWhile (== ' ') . T.reverse

seen :: [StoryId] -> StoryId -> Bool
seen seen_stories story_id = story_id `elem` seen_stories

getSeenStories :: IO [StoryId]
getSeenStories = do
    file <- readFile "seen.db"
    return $ map (StoryId . (\ line -> read line :: Int)) (lines file)

writeSeenStory :: StoryId -> IO ()
writeSeenStory (StoryId story_id) = do
    file <- openFile "seen.db" AppendMode
    hPrint file story_id
    hClose file
