{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Spec.AllTypesParsing (spec) where

import Control.Monad (forM_)
import qualified Data.Text as T
import MCP.Server
import MCP.Server.Derive
import Test.Hspec
import TestTypes
import TestData

allTypesHandlers :: (ToolListHandler IO, ToolCallHandler IO)
allTypesHandlers = $(deriveToolHandler ''AllTypesTool 'handleAllTypesTool)

spec :: Spec
spec = describe "All types parsing" $ do
  let (_, callHandler) = allTypesHandlers

  forM_ allTypesTestCases $ \tc ->
    it (T.unpack $ atTestName tc) $ do
      result <- callHandler "all_types_tool" (atTestArgs tc)
      case atTestResult tc of
        Right expected ->
          case result of
            Right (ContentText content) -> content `shouldBe` expected
            other -> expectationFailure $ "Expected Right ContentText but got: " ++ show other
        Left (errCon, expectedMsg) ->
          case result of
            Left (InvalidParams msg) | errCon == "InvalidParams" ->
              msg `shouldBe` expectedMsg
            Left (MissingRequiredParams msg) | errCon == "MissingRequiredParams" ->
              msg `shouldBe` expectedMsg
            other -> expectationFailure $
              "Expected " ++ T.unpack errCon ++ " with '" ++ T.unpack expectedMsg ++ "' but got: " ++ show other
