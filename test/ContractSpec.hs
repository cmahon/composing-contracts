module ContractSpec where

import Test.Hspec

import Contract

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Contract" $ do
    it "works" $ do
      tests `shouldBe` True
