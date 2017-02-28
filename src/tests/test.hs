import           Data.Text             as T
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Web.HackerNews        (Descendants (..), Item (..),
                                        ItemId (..), ItemType (..), Kids (..),
                                        Score (..), Time (..), Title (..),
                                        TopStories (..), URL (..),
                                        UserName (..))
import           Web.HnBot.Utils       (formatTweet, seen, shortTitle)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup "QuickCheck tests"
    [ QC.testProperty "Title needs to be <= 90 with story url" $
        -- 140 - 4 (spaces + parantheses) - 23 (story url) - 23 (comment url)
        \headline ->
            T.length (shortTitle (T.pack (headline :: String)) (T.pack "https://www.sengaya.de/"))
                <= 90
    , QC.testProperty "Title needs to be <= 116 without story url" $
        -- 140 - 1 (space) - 23 (comment url)
        \headline ->
            T.length (shortTitle (T.pack (headline :: String)) (T.pack ""))
                <= 116
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "formatTweet with story url" $ do
        let res = formatTweet $ story "Sengaya" "http://www.sengaya.de/"
        assertEqual "Formatted tweet" (T.pack "Sengaya http://www.sengaya.de/ (https://news.ycombinator.com/item?id=666)") res
    , testCase "formatTweet with story url, long title 1" $ do
        let res = formatTweet $ story lorem155 "http://sengaya.de/"
        assertEqual "Formatted tweet" (T.pack "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor ... http://sengaya.de/ (https://news.ycombinator.com/item?id=666)") res
    , testCase "formatTweet with story url, long title 2 (remove double spaces)" $ do
        let res = formatTweet $ story (Prelude.drop 1 lorem155) "http://www.sengaya.de/"
        assertEqual "Formatted tweet" (T.pack "orem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor ... http://www.sengaya.de/ (https://news.ycombinator.com/item?id=666)") res
    , testCase "formatTweet with no story url" $ do
        let res = formatTweet $ story "Sengaya" ""
        assertEqual "Formatted tweet" (T.pack "Sengaya https://news.ycombinator.com/item?id=666") res
    , testCase "formatTweet with no story url, long title" $ do
        let res = formatTweet $ story lorem155 ""
        assertEqual "Formatted tweet" (T.pack "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dol ... https://news.ycombinator.com/item?id=666") res
    , testCase "check if story id already posted (True)" $
        assertEqual "seen" True (seen someItemIds (ItemId 666))
    , testCase "check if story id already posted (False)" $
        assertEqual "seen" False (seen someItemIds (ItemId 667))
    ]

-- 155 random chars
lorem155 :: String
lorem155 = "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua."

someItemIds :: [ItemId]
someItemIds = [ItemId 1, ItemId 2, ItemId 666, ItemId 9999]

someTopStories :: TopStories
someTopStories = TopStories [ItemId 1, ItemId 2, ItemId 666, ItemId 1000]

story :: String -> String -> Item
story title url = Item { itemBy          = Just (UserName $ T.pack "storyBy")
                       , itemId          = Just (ItemId 666)
                       , itemKids        = Just (Kids [ItemId 77, ItemId 99])
                       , itemScore       = Just (Score 33)
                       , itemTime        = Just (Time 1172394646)
                       , itemTitle       = Just (Title $ T.pack title)
                       , itemType        = Story
                       , itemURL         = Just (URL $ T.pack url)
                       , itemDeleted     = Nothing
                       , itemDead        = Nothing
                       , itemText        = Nothing
                       , itemParent      = Nothing
                       , itemParts       = Nothing
                       , itemDescendants = Just (Descendants 2) }
