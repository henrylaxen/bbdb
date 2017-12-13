module BBDBSpec where

import Foundation  hiding ((<|>))
import Foundation.IO (readFile)
import Foundation.String ( replace, Encoding(UTF8), fromBytes  )
import Foundation.VFS.FilePath (FilePath)

import Database.BBDB
import Test.Hspec

spec :: Spec
spec = do
  w8a <- runIO $ readFile "test/sampleData.txt"
  let (bbdbString, _, _) = fromBytes UTF8 w8a
  describe "Read BBDB file" $ do
    let
      Right bbdb = parseBBDB bbdbString
      b :: [BBDB]
      b = justEntries bbdb
      me :: BBDB
      me = head . fromMaybe (error "is empty")  . nonEmpty $ b
    it "can parse the sample file as a String" $ do
      (length bbdbString) `shouldBe` 942
      (length bbdb) `shouldBe` 4
    it "can write it back as a lisp string" $ do
      let w = asLisp bbdb
      w == bbdbString `shouldBe` True
    it "found my first name" $ do
      firstName me `shouldBe` (Just "Henry")
    it "found my email"  $ do 
      net me `shouldBe` (Just ["nadine.and.henry@pobox.com"])
    it "found the tennis note" $ do
      getNote "tennis" me `shouldBe` (Just "chapala")
    it "did not find the xyzzy note" $ do
      getNote "xyzzy" me `shouldBe` Nothing
    it "found the generic record" $ do
      let
        wanted x = lastName x == Just "lastname"
        r = filterBBDB wanted bbdb
      length r `shouldBe` 1

