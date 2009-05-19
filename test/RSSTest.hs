module Main(main) where

import Text.RSS
import Test.HUnit
import Data.Time.Calendar
import Data.Time.LocalTime

testChannel :: RSSChannel
testChannel = RSSChannel { chTitle = "Example Channel"
                         , chLink = "http://example.com/"
                         , chDescription = "My example channel"
                         , chDC = [], chImage = Nothing }

testItems :: [RSSItem]
testItems = [ RSSItem { title = "News for September the First"
                      , link = "http://example.com/2002/09/01/"
                      , description = Just "other things happend today"
                      , content = Nothing
                      , dc = [DCDate sep1] }
            , RSSItem { title = "News for September the Second"
                      , link = "http://example.com/2002/09/02/"
                      , description = Nothing
                      , content = Nothing
                      , dc = [DCDate sep2] }
            ]
    where sep1 = ZonedTime (LocalTime (fromGregorian 2002 9 1) (TimeOfDay 0 0 0))
                           (minutesToTimeZone 0)
          sep2 = ZonedTime (LocalTime (fromGregorian 2002 9 2) (TimeOfDay 0 0 0))
                           (minutesToTimeZone 0)

testResult = 
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
    \<rdf:RDF xmlns=\"http://purl.org/rss/1.0/\"\n\
    \         xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n\
    \         xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\n\
    \         xmlns:content=\"http://purl.org/rss/1.0/modules/content/\">\n\
    \  <channel rdf:about=\"http://example.com/\">\n\
    \    <title>Example Channel</title><link>http://example.com/</link>\n\
    \    <description>My example channel</description>\n\
    \    <items>\n\
    \      <rdf:Seq>\n\
    \        <rdf:li rdf:resource=\"http://example.com/2002/09/01/\" />\n\
    \        <rdf:li rdf:resource=\"http://example.com/2002/09/02/\" />\n\
    \      </rdf:Seq>\n\
    \    </items>\n\
    \  </channel>\n\
    \  <item rdf:about=\"http://example.com/2002/09/01/\">\n\
    \    <title>News for September the First</title>\n\
    \    <link>http://example.com/2002/09/01/</link>\n\
    \    <description>other things happend today</description>\n\
    \    <dc:date>2002-09-01T00:00:00+00:00</dc:date>\n\
    \  </item>\n\
    \  <item rdf:about=\"http://example.com/2002/09/02/\">\n\
    \    <title>News for September the Second</title>\n\
    \    <link>http://example.com/2002/09/02/</link>\n\
    \    <dc:date>2002-09-02T00:00:00+00:00</dc:date>\n\
    \  </item>\n\
    \</rdf:RDF>"

main = runTestTT $ test $ renderRSS testChannel testItems ~=? testResult
