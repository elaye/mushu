{-# LANGUAGE OverloadedStrings #-}
module MIME.Parser.Test
( tests
, Arbitrary
, arbitrary
) where

import ClassyPrelude

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Text.Arbitrary (Text)
import Test.QuickCheck.Utf8 (oneByte)

import Data.Attoparsec.Text (parseOnly)
import Data.Text (replace)

import qualified MIME.Parser as MP
import Mail (Target(..))

asciiGen :: Gen Text
asciiGen = decodeUtf8 . concat <$> (QC.listOf oneByte)

data TargetText = TargetText Target Text deriving Show

instance Arbitrary Target where
  arbitrary = do
    -- TODO: what if we have "\\nn"?
    address <- QC.suchThat (rmNewlines <$> asciiGen) (\t -> t /= "")
    name <- QC.suchThat (rmNewlines <$> asciiGen) (\t -> t /= "")
    maybeName <- elements [Nothing, Just name]
    return $ Target { _name = traceShowId maybeName, _address = address }
      where rmNewlines t = replace "\r\n" "" t
  shrink t = do
    let
      name = _name t
      address = _address t
      addressOnlyTarget = Target { _name = Nothing, _address = address }
    case name of
      Nothing -> [addressOnlyTarget]
      Just _ -> [addressOnlyTarget, t]

instance Arbitrary TargetText where
  arbitrary = do
    target <- arbitrary :: Gen Target
    -- spaces/tabs/newlines?
    return $ TargetText target (targetToText target)

tests :: TestTree
tests = testGroup "MIME Parser"
  [ parseTarget
  , parseTargets
  ] 

targetToText :: Target -> Text
targetToText t = case _name t of
  Just name -> name ++ " <" ++ address ++ ">"
  Nothing -> address
  where address = _address t

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

{-parseTargets = QC.testProperty "Parse targets" $ \t -> isRight (parseOnly MP.targetsParser (targetToText (t :: Target)))-}
parseTargets = QC.testProperty "Parse targets" $ \t -> isRight (parseOnly MP.targetsParser (targetToText (t :: Target)))

{-parseTarget = QC.testProperty "Parse target" $ \t -> isRight (parseOnly MP.targetParser (targetToText (t :: Target)))-}
{-parseTarget = QC.testProperty "Parse target" $ \t -> isRight (parseOnly MP.targetParser (targetToText (t :: Target)))-}
parseTarget = QC.testProperty "Parse target" test
  where
    test (TargetText target txt) = case parseOnly MP.targetParser txt of
      Left err -> traceShow ("error " ++ err) False
      Right parsedTarget -> (traceShowId parsedTarget) == target
