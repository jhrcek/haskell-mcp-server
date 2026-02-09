{-# LANGUAGE OverloadedStrings #-}

module TestData
  ( -- * Prompt Test Data
    promptTestCases
  , PromptTestCase(..)

    -- * Resource Test Data
  , resourceTestCases
  , ResourceTestCase(..)

    -- * Tool Test Data
  , toolTestCases
  , ToolTestCase(..)

    -- * Schema Test Data
  , schemaTestCases
  , SchemaTestCase(..)

    -- * Advanced Derivation Test Data
  , separateParamsTestCases
  , recursiveParamsTestCases
  , SeparateParamsTestCase(..)
  , RecursiveParamsTestCase(..)

    -- * All-types parsing test data
  , allTypesTestCases
  , AllTypesTestCase(..)
  ) where

import Data.Text (Text)

-- | Test case for prompt derivation
data PromptTestCase = PromptTestCase
  { promptName :: Text
  , promptArgs :: [(Text, Text)]
  , expectedResult :: Either Text Text  -- Left for expected error message, Right for expected content
  , testDescription :: Text
  } deriving (Show, Eq)

-- | Test case for resource derivation
data ResourceTestCase = ResourceTestCase
  { resourceUri :: Text
  , resourceExpectedContent :: Text
  , useSubstringMatch :: Bool  -- True for substring match, False for exact match
  , resourceTestDescription :: Text
  } deriving (Show, Eq)

-- | Test case for tool derivation
data ToolTestCase = ToolTestCase
  { toolName :: Text
  , toolArgs :: [(Text, Text)]
  , toolExpectedResult :: Text
  , toolTestDescription :: Text
  } deriving (Show, Eq)

-- | Test case for schema validation
data SchemaTestCase = SchemaTestCase
  { schemaToolName :: Text
  , expectedProperties :: [(Text, Text)] -- (property name, expected type)
  , requiredFields :: [Text]
  , schemaTestDescription :: Text
  } deriving (Show, Eq)

-- | Test case for separate parameter types
data SeparateParamsTestCase = SeparateParamsTestCase
  { sepToolName :: Text
  , sepArgs :: [(Text, Text)]
  , sepExpectedResult :: Text
  , sepExpectedProperties :: [Text]
  , sepTestDescription :: Text
  } deriving (Show, Eq)

-- | Test case for recursive parameter types
data RecursiveParamsTestCase = RecursiveParamsTestCase
  { recToolName :: Text
  , recArgs :: [(Text, Text)]
  , recExpectedResult :: Text
  , recExpectedProperties :: [Text]
  , recTestDescription :: Text
  } deriving (Show, Eq)

-- | Prompt test cases
-- Note: Comprehensive type parsing (Int/Double/Float/Bool/Text, required/optional, success/failure)
-- is covered by AllTypesTestCase. These tests focus on prompt-specific behavior.
promptTestCases :: [PromptTestCase]
promptTestCases =
  [ PromptTestCase
      { promptName = "simple_prompt"
      , promptArgs = [("message", "hello")]
      , expectedResult = Right "Simple prompt: hello"
      , testDescription = "handles simple prompts correctly"
      }
  ]

-- | Resource test cases
resourceTestCases :: [ResourceTestCase]
resourceTestCases =
  [ ResourceTestCase
      { resourceUri = "resource://config_file"
      , resourceExpectedContent = "Config file contents"
      , useSubstringMatch = True
      , resourceTestDescription = "handles simple resources correctly"
      }
  , ResourceTestCase
      { resourceUri = "resource://database_connection"
      , resourceExpectedContent = "Database at localhost:5432"
      , useSubstringMatch = False
      , resourceTestDescription = "handles parameterized resources correctly"
      }
  ]

-- | Tool test cases
toolTestCases :: [ToolTestCase]
toolTestCases =
  [ ToolTestCase
      { toolName = "calculate"
      , toolArgs = [("operation", "add"), ("x", "10"), ("y", "5")]
      , toolExpectedResult = "15"
      , toolTestDescription = "handles simple tool calls correctly"
      }
  , ToolTestCase
      { toolName = "calculate"
      , toolArgs = [("operation", "multiply"), ("x", "7"), ("y", "6")]
      , toolExpectedResult = "42"
      , toolTestDescription = "handles tool calls with different operations"
      }
  ]

-- | Schema test cases
schemaTestCases :: [SchemaTestCase]
schemaTestCases =
  [ SchemaTestCase
      { schemaToolName = "calculate"
      , expectedProperties = [("x", "integer"), ("y", "integer"), ("operation", "string")]
      , requiredFields = ["operation", "x", "y"]
      , schemaTestDescription = "generates correct schema for Calculate tool"
      }
  , SchemaTestCase
      { schemaToolName = "echo"
      , expectedProperties = [("text", "string")]
      , requiredFields = ["text"]
      , schemaTestDescription = "generates correct schema for Echo tool"
      }
  ]

-- | Separate parameter types test cases
separateParamsTestCases :: [SeparateParamsTestCase]
separateParamsTestCases =
  [ SeparateParamsTestCase
      { sepToolName = "get_value"
      , sepArgs = [("_gvpKey", "mykey")]
      , sepExpectedResult = "Getting value for key: mykey"
      , sepExpectedProperties = ["_gvpKey"]
      , sepTestDescription = "executes GetValue with separate parameters correctly"
      }
  , SeparateParamsTestCase
      { sepToolName = "set_value"
      , sepArgs = [("_svpKey", "mykey"), ("_svpValue", "myvalue")]
      , sepExpectedResult = "Setting mykey = myvalue"
      , sepExpectedProperties = ["_svpKey", "_svpValue"]
      , sepTestDescription = "executes SetValue with separate parameters correctly"
      }
  ]

-- | Recursive parameter types test cases
recursiveParamsTestCases :: [RecursiveParamsTestCase]
recursiveParamsTestCases =
  [ RecursiveParamsTestCase
      { recToolName = "process_data"
      , recArgs = [("_ipName", "Alice"), ("_ipAge", "30")]
      , recExpectedResult = "Processing data for Alice (age 30)"
      , recExpectedProperties = ["_ipName", "_ipAge"]
      , recTestDescription = "executes recursive parameter tools correctly"
      }
  ]

-- | Test case for all-types tool (covers every type x required/optional x success/failure)
data AllTypesTestCase = AllTypesTestCase
  { atTestName   :: Text
  , atTestArgs   :: [(Text, Text)]
  , atTestResult :: Either (Text, Text) Text
    -- ^ Left (errorConstructor, message) for expected errors, Right for expected content
  } deriving (Show, Eq)

allTypesTestCases :: [AllTypesTestCase]
allTypesTestCases =
  let
    -- Base valid args for all required fields
    validBase =
      [ ("atInt", "42"), ("atInteger", "100"), ("atDouble", "3.14"), ("atFloat", "2.5")
      , ("atBool", "true"), ("atText", "hello")
      ]
    -- All fields provided (required + optional)
    allValid = validBase ++
      [ ("atOptInt", "7"), ("atOptInteger", "99"), ("atOptDouble", "1.1"), ("atOptFloat", "0.5")
      , ("atOptBool", "false"), ("atOptText", "world")
      ]
  in
  -- === Success cases ===
  [ AllTypesTestCase
      { atTestName = "all required + all optional provided"
      , atTestArgs = allValid
      , atTestResult = Right "42,100,3.14,2.5,True,hello,Just 7,Just 99,Just 1.1,Just 0.5,Just False,Just \"world\""
      }
  , AllTypesTestCase
      { atTestName = "all required, no optional"
      , atTestArgs = validBase
      , atTestResult = Right "42,100,3.14,2.5,True,hello,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing"
      }

  -- === Required field missing ===
  , AllTypesTestCase
      { atTestName = "missing required Int"
      , atTestArgs = filter ((/= "atInt") . fst) validBase
      , atTestResult = Left ("MissingRequiredParams", "field 'atInt' is missing")
      }
  , AllTypesTestCase
      { atTestName = "missing required Integer"
      , atTestArgs = filter ((/= "atInteger") . fst) validBase
      , atTestResult = Left ("MissingRequiredParams", "field 'atInteger' is missing")
      }
  , AllTypesTestCase
      { atTestName = "missing required Double"
      , atTestArgs = filter ((/= "atDouble") . fst) validBase
      , atTestResult = Left ("MissingRequiredParams", "field 'atDouble' is missing")
      }
  , AllTypesTestCase
      { atTestName = "missing required Float"
      , atTestArgs = filter ((/= "atFloat") . fst) validBase
      , atTestResult = Left ("MissingRequiredParams", "field 'atFloat' is missing")
      }
  , AllTypesTestCase
      { atTestName = "missing required Bool"
      , atTestArgs = filter ((/= "atBool") . fst) validBase
      , atTestResult = Left ("MissingRequiredParams", "field 'atBool' is missing")
      }
  , AllTypesTestCase
      { atTestName = "missing required Text"
      , atTestArgs = filter ((/= "atText") . fst) validBase
      , atTestResult = Left ("MissingRequiredParams", "field 'atText' is missing")
      }

  -- === Required field parse failure ===
  , AllTypesTestCase
      { atTestName = "invalid required Int"
      , atTestArgs = ("atInt", "notanint") : filter ((/= "atInt") . fst) validBase
      , atTestResult = Left ("InvalidParams", "Failed to parse field 'atInt' from: notanint")
      }
  , AllTypesTestCase
      { atTestName = "invalid required Integer"
      , atTestArgs = ("atInteger", "nope") : filter ((/= "atInteger") . fst) validBase
      , atTestResult = Left ("InvalidParams", "Failed to parse field 'atInteger' from: nope")
      }
  , AllTypesTestCase
      { atTestName = "invalid required Double"
      , atTestArgs = ("atDouble", "xyz") : filter ((/= "atDouble") . fst) validBase
      , atTestResult = Left ("InvalidParams", "Failed to parse field 'atDouble' from: xyz")
      }
  , AllTypesTestCase
      { atTestName = "invalid required Float"
      , atTestArgs = ("atFloat", "abc") : filter ((/= "atFloat") . fst) validBase
      , atTestResult = Left ("InvalidParams", "Failed to parse field 'atFloat' from: abc")
      }
  , AllTypesTestCase
      { atTestName = "invalid required Bool"
      , atTestArgs = ("atBool", "notbool") : filter ((/= "atBool") . fst) validBase
      , atTestResult = Left ("InvalidParams", "Failed to parse field 'atBool' from: notbool")
      }

  -- === Optional field parse failure ===
  , AllTypesTestCase
      { atTestName = "invalid optional Int"
      , atTestArgs = validBase ++ [("atOptInt", "bad")]
      , atTestResult = Left ("InvalidParams", "Failed to parse field 'atOptInt' from: bad")
      }
  , AllTypesTestCase
      { atTestName = "invalid optional Integer"
      , atTestArgs = validBase ++ [("atOptInteger", "bad")]
      , atTestResult = Left ("InvalidParams", "Failed to parse field 'atOptInteger' from: bad")
      }
  , AllTypesTestCase
      { atTestName = "invalid optional Double"
      , atTestArgs = validBase ++ [("atOptDouble", "bad")]
      , atTestResult = Left ("InvalidParams", "Failed to parse field 'atOptDouble' from: bad")
      }
  , AllTypesTestCase
      { atTestName = "invalid optional Float"
      , atTestArgs = validBase ++ [("atOptFloat", "bad")]
      , atTestResult = Left ("InvalidParams", "Failed to parse field 'atOptFloat' from: bad")
      }
  , AllTypesTestCase
      { atTestName = "invalid optional Bool"
      , atTestArgs = validBase ++ [("atOptBool", "bad")]
      , atTestResult = Left ("InvalidParams", "Failed to parse field 'atOptBool' from: bad")
      }

  -- === Optional field parse success (selective) ===
  , AllTypesTestCase
      { atTestName = "optional Int provided"
      , atTestArgs = validBase ++ [("atOptInt", "7")]
      , atTestResult = Right "42,100,3.14,2.5,True,hello,Just 7,Nothing,Nothing,Nothing,Nothing,Nothing"
      }
  , AllTypesTestCase
      { atTestName = "optional Bool provided"
      , atTestArgs = validBase ++ [("atOptBool", "TRUE")]
      , atTestResult = Right "42,100,3.14,2.5,True,hello,Nothing,Nothing,Nothing,Nothing,Just True,Nothing"
      }
  , AllTypesTestCase
      { atTestName = "optional Text provided"
      , atTestArgs = validBase ++ [("atOptText", "hi")]
      , atTestResult = Right "42,100,3.14,2.5,True,hello,Nothing,Nothing,Nothing,Nothing,Nothing,Just \"hi\""
      }

  -- === Bool case insensitivity ===
  , AllTypesTestCase
      { atTestName = "Bool parses TRUE"
      , atTestArgs = ("atBool", "TRUE") : filter ((/= "atBool") . fst) validBase
      , atTestResult = Right "42,100,3.14,2.5,True,hello,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing"
      }
  , AllTypesTestCase
      { atTestName = "Bool parses False"
      , atTestArgs = ("atBool", "False") : filter ((/= "atBool") . fst) validBase
      , atTestResult = Right "42,100,3.14,2.5,False,hello,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing"
      }
  ]
