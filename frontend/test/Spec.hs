module Main (main) where

import           Control.Monad
import qualified Data.ByteString      as B
import           Data.Text
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           ENCOINS.Common.Utils (checkUrl)
import           Test.Hspec           (Spec, describe, hspec, it, shouldBe)


main :: IO ()
main = hspec $ do
  validateUrlWithPosix

validateUrlWithPosix :: Spec
validateUrlWithPosix = do
  describe "Valid urls should be parsed" $ do
    forM_ validUrls $ \url -> do
      it (T.unpack url) $ checkUrl url `shouldBe` True
  describe "Invalid urls should fail" $ do
    forM_ invalidUrls $ \url -> do
      it (T.unpack url) $ checkUrl url `shouldBe` False

validUrls :: [Text]
validUrls =
  [ "http://foo.com/blah_blah"
  , "http://foo.com/blah_blah/"
  , "http://foo.com/blah_blah_(wikipedia)"
  , "http://foo.com/blah_blah_(wikipedia)_(again)"
  , "http://www.example.com/wpstyle/?p=364"
  , "https://www.example.com/foo/?bar=baz&inga=42&quux"
  , "http://foo.com/blah_(wikipedia)#cite-1"
  , "http://foo.com/blah_(wikipedia)_blah#cite-1"
  , "http://foo.com/(something)?after=parens"
  , "http://code.google.com/events/#&product=browser"
  , "http://j.mp"
  , "http://foo.bar/?q=Test%20URL-encoded%20stuff"
  , "http://1337.net"
  , "http://a.b-c.de"
  , "http://223.255.255.254"
  , "http://142.42.1.1/"
  , "http://142.42.1.1:8080/"
  , "http://213.136.92.202"
  , "http://encoins-maria-relay.cardanistas.io"
  -- , "ftp://foo.bar/baz"
  -- , "http://foo.com/unicode_(✪)_in_parens"
  -- , "http://➡.ws/䨹"
  -- , "http://⌘.ws"
  -- , "http://⌘.ws/"
  -- , "http://☺.damowmow.com/"
  -- , "http://✪df.ws/123"
  -- , "http://مثال.إختبار"
  -- , "http://例子.测试"
  -- , "http://उदाहरण.परीक्षा"
  -- , "http://-.~_!$&'()*+,;=:%40:80%2f::::::@example.com"
  -- , "http://userid:password@example.com:8080"
  -- , "http://userid:password@example.com:8080/"
  -- , "http://userid@example.com"
  -- , "http://userid@example.com/"
  -- , "http://userid@example.com:8080"
  -- , "http://userid@example.com:8080/"
  -- , "http://userid:password@example.com"
  -- , "http://userid:password@example.com/"
  ]

invalidUrls :: [Text]
invalidUrls =
  [ "http://"
  , "http://."
  , "http://.."
  , "http://../"
  , "http://?"
  , "http://??"
  , "http://??/"
  , "http://#"
  , "http://##"
  , "http://##/"
  , "http://foo.bar?q=Spaces should be encoded"
  , "//"
  , "//a"
  , "///a"
  , "///"
  , "http:///a"
  , "foo.com"
  , "rdar://1234"
  , "h://test"
  , "http:// shouldfail.com"
  , ":// should fail"
  , "http://foo.bar/foo(bar)baz quux"
  , "ftps://foo.bar/"
  , "http://-error-.invalid/"
  , "http://a.b--c.de/"
  , "http://-a.b.co"
  , "http://a.b-.co"
  , "http://1.1.1.1.1"
  , "http://123.123.123"
  , "http://3628126748"
  , "http://.www.foo.bar/"
  , "http://www.foo.bar./"
  , "http://.www.foo.bar./"
  , "http://203.23.12.14.12"
  , "http://203.23.12"
  -- , "http://0.0.0.0"
  -- , "http://10.1.1.1"
  -- , "http://10.1.1.0"
  -- , "http://10.1.1.255"
  -- , "http://224.1.1.1"
  ]
