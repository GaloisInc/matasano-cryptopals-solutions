module Set2.C13Spec where

import Data.List.Split ( chunksOf )
import Test.Hspec
import Test.QuickCheck as QC

import Set1
import Set2.C13

spec :: Spec
spec = do
    describe "parseKV" $ do
      it "works on example input" $ do
        parseKV exampleInput `shouldBe` Just exampleOutput

    describe "encodeKV" $ do
      it "works on example output" $ do
        encodeKV exampleOutput `shouldBe` exampleInput

      it "works for simple profile" $ do
         encodeKV (profile_for "foo@bar.com") `shouldBe` "email=foo@bar.com&uid=10&role=user"


    describe "adminAccount" $ do
      it "can escalate to admin" $ do
        (lookup "role" =<< adminAccount) `shouldBe` Just "admin"

exampleInput :: String
exampleInput = "foo=bar&baz=qux&zap=zazzle"

exampleOutput :: [(String,String)]
exampleOutput = [ ("foo","bar")
                 , ("baz","qux")
                 , ("zap","zazzle")
                 ]

adminAccount :: Maybe [(String,String)]
adminAccount = parseKV (asciify (ecbDec frankenstein))
  where
  -- email is 13 characters long, which puts "role=" at the end
  -- of a block boundary.
  email = "buggr@foo.com"
  regularBlocks = chunksOf 16 (oracle email)

  -- We get this block by making a long email string, which pushes
  -- over a block boundary and ends with "admin".
  adminBlocks :: [[Byte]]
  adminBlocks = drop 1 (chunksOf 16 ciphertext)
    where
    -- adminBlockEmail is of length (10 + length "admin")
    adminBlockEmail = replicate 10 'x' ++ "admin"
    ciphertext = oracle adminBlockEmail

  frankenstein :: [Byte]
  frankenstein = concat (init regularBlocks ++ adminBlocks)
