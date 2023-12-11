module Main (main) where

import           Control.Monad
import qualified Data.ByteString      as B
import           Data.Either          (isLeft)
import           Data.Text
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           ENCOINS.Common.Utils (checkUrl, stripHost)
import           Test.Hspec           (Spec, describe, hspec, it, shouldBe,
                                       shouldSatisfy)


main :: IO ()
main = hspec $ do
  validateUrlWithPosix
  parseUrlWithStrip

validateUrlWithPosix :: Spec
validateUrlWithPosix = do
  describe "Valid urls should be parsed" $ do
    forM_ validUrls $ \url -> do
      it (T.unpack url) $ checkUrl url `shouldBe` True
  describe "Invalid urls should fail" $ do
    forM_ invalidUrls $ \url -> do
      it (T.unpack url) $ checkUrl url `shouldBe` False

parseUrlWithStrip :: Spec
parseUrlWithStrip = do
  describe "Valid urls should be parsed" $ do
    forM_ validStripUrls $ \(url, response) -> do
      it (T.unpack url) $
        stripHost url `shouldSatisfy` (== Right response)
  describe "Invalid urls should fail" $ do
    forM_ invalidStripUrls $ \url -> do
      it (T.unpack url) $ stripHost url `shouldSatisfy` isLeft

validUrls :: [Text]
validUrls =
  [ "http://foo.com/"
  , "http://142.42.1.1/"
  , "http://www.example.com/"
  , "http://code.google.com/"
  , "http://1337.net/"
  , "http://223.255.255.254/"
  , "http://a.b-c.de/"
  , "http://encoins-maria-relay.cardanistas.io/"
  , "http://j.mp/"

  , "https://142.42.1.1/"
  , "https://www.example.com/"
  , "https://code.google.com/"
  , "https://1337.net/"
  , "https://223.255.255.254/"
  , "https://a.b-c.de/"
  , "https://encoins-maria-relay.cardanistas.io/"
  , "https://j.mp/"
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
  , "http://-a.b.co/"
  , "http://a.b-.co"
  , "http://a.b-.co/"
  , "http://0.0.0.0"
  , "http://10.1.1.0"
  , "http://10.1.1.255"
  , "http://224.1.1.1"
  , "http://1.1.1.1.1"
  , "http://1.1.1.1.1/"
  , "http://123.123.123"
  , "http://123.123.123/"
  , "http://3628126748"
  , "http://.www.foo.bar/"
  , "http://www.foo.bar./"
  , "http://.www.foo.bar./"
  , "http://10.1.1.1"
  , "http://203.23.12.14.12"
  , "http://203.23.12"

  , "ftp://foo.bar/baz"
  , "http://foo.com/unicode_(✪)_in_parens"
  , "http://➡.ws/䨹"
  , "http://⌘.ws"
  , "http://⌘.ws/"
  , "http://☺.damowmow.com/"
  , "http://✪df.ws/123"
  , "http://مثال.إختبار"
  , "http://例子.测试"
  , "http://उदाहरण.परीक्षा"
  , "http://-.~_!$&'()*+,;=:%40:80%2f::::::@example.com"
  , "http://userid:password@example.com:8080"
  , "http://userid:password@example.com:8080/"
  , "http://userid@example.com"
  , "http://userid@example.com/"
  , "http://userid@example.com:8080"
  , "http://userid@example.com:8080/"
  , "http://userid:password@example.com"
  , "http://userid:password@example.com/"

  , "http://0.0.0.0"
  , "http://10.1.1.1"
  , "http://10.1.1.0"
  , "http://10.1.1.255"
  , "http://224.1.1.1"
  ]

validStripUrls :: [(Text, Text)]
validStripUrls =
  [ ("http://foo.com/", "foo.com")
  , ("http://142.42.1.1/", "142.42.1.1")
  , ("http://www.example.com/", "www.example.com")
  , ("http://code.google.com/", "code.google.com")
  , ("http://1337.net/", "1337.net")
  , ("http://223.255.255.254/", "223.255.255.254")
  , ("http://a.b-c.de/", "a.b-c.de")
  , ("http://encoins-maria-relay.cardanistas.io/", "encoins-maria-relay.cardanistas.io")
  , ("http://j.mp/", "j.mp")
  , ("https://142.42.1.1/", "142.42.1.1")
  , ("https://www.example.com/", "www.example.com")
  , ("https://code.google.com/", "code.google.com")
  , ("https://1337.net/", "1337.net")
  , ("https://223.255.255.254/", "223.255.255.254")
  , ("https://a.b-c.de/", "a.b-c.de")
  , ("https://encoins-maria-relay.cardanistas.io/", "encoins-maria-relay.cardanistas.io")
  , ("https://j.mp/", "j.mp")
  , ("http://foo.com/blah_blah", "foo.com")
  , ("http://223.255.255.254", "223.255.255.254")
  , ("http://213.136.92.202", "213.136.92.202")
  , ("http://142.42.1.1:8080/", "142.42.1.1")
  , ("http://localhost:8080/", "localhost")

  , ("http://73.23.36.140/", "73.23.36.140")
  , ("http://61.77.248.249/", "61.77.248.249")
  , ("http://128.199.195.27/", "128.199.195.27")
  , ("https://136.243.152.100/", "136.243.152.100")
  , ("http://encoins.bladepool.com/", "encoins.bladepool.com")
  , ("http://146.190.63.130/", "146.190.63.130")
  , ("https://5.189.164.41/", "5.189.164.41")
  , ("http://82.165.230.141/", "82.165.230.141")
  , ("http://146.190.34.49/", "146.190.34.49")
  , ("http://encoins-relay.cardanistas.io/", "encoins-relay.cardanistas.io")
  , ("http://encoins.reservoir.network/", "encoins.reservoir.network")
  , ("http://213.136.92.202/", "213.136.92.202")
  , ("http://167.71.10.20/", "167.71.10.20")
  , ("http://38.242.237.17/", "38.242.237.17")
  , ("http://161.97.73.124/", "161.97.73.124")
  , ("http://encs.chunkymonkey.us/", "encs.chunkymonkey.us")
  , ("http://142.132.189.114/", "142.132.189.114")
  , ("http://161.97.172.86/", "161.97.172.86")
  , ("http://135.148.53.149/", "135.148.53.149")
  , ("http://135.148.53.148/", "135.148.53.148")
  , ("http://125.250.255.197/", "125.250.255.197")
  , ("http://encoins-maria-relay.cardanistas.io/", "encoins-maria-relay.cardanistas.io")
  , ("http://95.179.167.197/", "95.179.167.197")
  , ("http://2.59.156.182/", "2.59.156.182")
  , ("http://81.56.175.1/", "81.56.175.1")
  , ("http://144.91.80.26/", "144.91.80.26")
  , ("http://encoins02.bladepool.com/", "encoins02.bladepool.com")
  , ("http://75.119.155.25/", "75.119.155.25")
  , ("http://117.50.199.89/", "117.50.199.89")
  , ("http://172.205.248.34/", "172.205.248.34")
  , ("http://135.148.53.151/", "135.148.53.151")
  , ("http://161.97.88.36/", "161.97.88.36")
  , ("http://136.244.19.126/", "136.244.19.126")
  , ("http://23.105.171.80/", "23.105.171.80")
  , ("https://178.119.158.164/", "178.119.158.164")
  , ("https://encoins.cardanode.com.au/", "encoins.cardanode.com.au")
  , ("http://encoins.congeepool.com/", "encoins.congeepool.com")
  , ("http://154.38.171.77/", "154.38.171.77")
  , ("http://149.102.156.213/", "149.102.156.213")
  , ("http://176.57.184.115/", "176.57.184.115")
  , ("a.b-c.de", "a.b-c.de")
  , ("a.b-c.de/", "a.b-c.de")
  , ("a.b-c.de:3000/", "a.b-c.de")
  , ("a.b-c.de:3000`", "a.b-c.de")
  , ("142.42.1.1", "142.42.1.1")
  , ("142.42.1.1/", "142.42.1.1")
  , ("142.42.1.1:3000/", "142.42.1.1")
  , ("142.42.1.1:3000", "142.42.1.1")
  , ("httpexample.com", "httpexample.com")
  , ("httpexample.com/", "httpexample.com")
  , ("http://localhost:8080/", "localhost")
  ]

invalidStripUrls :: [Text]
invalidStripUrls =
  [ "http://.com/"
  , "ftp://142.42.1.1/"
  , "http://f/"
  ]
