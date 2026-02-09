{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module MCP.Server.Derive
  ( -- * Template Haskell Derivation
    derivePromptHandler
  , derivePromptHandlerWithDescription
  , deriveResourceHandler
  , deriveResourceHandlerWithDescription
  , deriveToolHandler
  , deriveToolHandlerWithDescription
  ) where

import qualified Data.Text           as T
import           Language.Haskell.TH
import           Text.Read           (readMaybe)
import qualified Data.Char           as Char

import           MCP.Server.Types

-- Helper function to convert PascalCase/camelCase to snake_case
toSnakeCase :: String -> String
toSnakeCase [] = []
toSnakeCase (x:xs) = Char.toLower x : go xs
  where
    go [] = []
    go (c:cs)
      | Char.isUpper c = '_' : Char.toLower c : go cs
      | otherwise = c : go cs

-- | Check if a type is @Maybe t@, returning the inner type if so.
isOptionalType :: Type -> Bool
isOptionalType (AppT (ConT n) _) = nameBase n == "Maybe"
isOptionalType _                 = False

-- | Unwrap @Maybe t@ to @t@, or return the type unchanged.
unwrapMaybe :: Type -> Type
unwrapMaybe (AppT (ConT n) inner) | nameBase n == "Maybe" = inner
unwrapMaybe t = t

-- | Map a Haskell type to its JSON schema type string.
jsonTypeOf :: Type -> String
jsonTypeOf (ConT n) = case nameBase n of
    "Int" -> "integer"
    "Integer" -> "integer"
    "Double" -> "number"
    "Float" -> "number"
    "Bool" -> "boolean"
    _ -> "string"
jsonTypeOf _ = "string"

-- | Extract constructor name, description, and fields from a constructor.
getConInfo :: [(String, String)] -> String -> Con -> Q (String, String, [(Name, Bang, Type)])
getConInfo descriptions defaultDescPrefix con = case con of
  NormalC name [] -> do
    let constructorName = nameBase name
    let snakeName = toSnakeCase constructorName
    let description = case lookup constructorName descriptions of
          Just desc -> desc
          Nothing   -> defaultDescPrefix ++ constructorName
    return (snakeName, description, [])
  RecC name fields -> do
    let constructorName = nameBase name
    let snakeName = toSnakeCase constructorName
    let description = case lookup constructorName descriptions of
          Just desc -> desc
          Nothing   -> defaultDescPrefix ++ constructorName
    return (snakeName, description, fields)
  NormalC name [(_bang, paramType)] -> do
    let constructorName = nameBase name
    let snakeName = toSnakeCase constructorName
    let description = case lookup constructorName descriptions of
          Just desc -> desc
          Nothing   -> defaultDescPrefix ++ constructorName
    fields <- extractFieldsFromParamType paramType
    return (snakeName, description, fields)
  _ -> fail "Unsupported constructor type"

-- | Unified handler case generation for prompts and tools.
-- Returns a Match that dispatches on the snake_case constructor name.
mkHandlerCase :: Name -> Con -> Q Match
mkHandlerCase handlerName (NormalC name []) = do
  let snakeName = toSnakeCase . nameBase $ name
  body <- [| do
      content <- $(varE handlerName) $(conE name)
      pure $ Right content |]
  return $ Match (LitP $ StringL snakeName) (NormalB body) []
mkHandlerCase handlerName (RecC name fields) = do
  let snakeName = toSnakeCase . nameBase $ name
  body <- mkRecordCase name handlerName fields
  return $ Match (LitP $ StringL snakeName) (NormalB body) []
mkHandlerCase handlerName (NormalC name [(_bang, paramType)]) = do
  let snakeName = toSnakeCase . nameBase $ name
  body <- mkSeparateParamsCase name handlerName paramType
  return $ Match (LitP $ StringL snakeName) (NormalB body) []
mkHandlerCase _ _ = fail "Unsupported constructor type"

-- | Derive prompt handlers from a data type with custom descriptions
-- Usage: $(derivePromptHandlerWithDescription ''MyPrompt 'handlePrompt [("Constructor", "Description")])
derivePromptHandlerWithDescription :: Name -> Name -> [(String, String)] -> Q Exp
derivePromptHandlerWithDescription typeName handlerName descriptions = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ constructors _) -> do
      promptDefs <- mapM (mkPromptDefWithDescription descriptions) constructors

      listHandlerExp <- [| pure $(return $ ListE promptDefs) |]

      matches <- mapM (mkHandlerCase handlerName) constructors
      defaultCase <- [| pure $ Left $ InvalidPromptName $ "Unknown prompt: " <> name |]
      let defaultMatch = Match WildP (NormalB defaultCase) []

      getHandlerExp <- return $ LamE [VarP (mkName "name"), VarP (mkName "args")] $
        CaseE (AppE (VarE 'T.unpack) (VarE (mkName "name")))
          (matches ++ [defaultMatch])

      return $ TupE [Just listHandlerExp, Just getHandlerExp]
    _ -> fail $ "derivePromptHandlerWithDescription: " ++ show typeName ++ " is not a data type"

-- | Derive prompt handlers from a data type
-- Usage: $(derivePromptHandler ''MyPrompt 'handlePrompt)
derivePromptHandler :: Name -> Name -> Q Exp
derivePromptHandler typeName handlerName =
  derivePromptHandlerWithDescription typeName handlerName []

mkPromptDefWithDescription :: [(String, String)] -> Con -> Q Exp
mkPromptDefWithDescription descriptions con = do
  (snakeName, description, fields) <- getConInfo descriptions "Handle " con
  args <- case con of
    NormalC _ [(_bang, paramType)] -> extractFieldsFromType descriptions paramType
    _ -> mapM (mkArgDef descriptions) fields
  [| PromptDefinition
      { promptDefinitionName = $(litE $ stringL snakeName)
      , promptDefinitionDescription = $(litE $ stringL description)
      , promptDefinitionArguments = $(return $ ListE args)
      , promptDefinitionTitle = Nothing
      } |]

-- Extract field definitions from a parameter type recursively
extractFieldsFromType :: [(String, String)] -> Type -> Q [Exp]
extractFieldsFromType descriptions paramType = do
  case paramType of
    ConT typeName -> do
      info <- reify typeName
      case info of
        TyConI (DataD _ _ _ _ [RecC _ fields] _) ->
          mapM (mkArgDef descriptions) fields
        TyConI (DataD _ _ _ _ [NormalC _ [(_bang, innerType)]] _) ->
          extractFieldsFromType descriptions innerType
        _ -> fail $ "Parameter type " ++ show typeName ++ " must be a record type or single-parameter constructor"
    _ -> fail $ "Parameter type must be a concrete type, got: " ++ show paramType

mkArgDef :: [(String, String)] -> (Name, Bang, Type) -> Q Exp
mkArgDef descriptions (fieldName, _, fieldType) = do
  let fieldNameStr = nameBase fieldName
  let description = case lookup fieldNameStr descriptions of
        Just desc -> desc
        Nothing   -> fieldNameStr
  [| ArgumentDefinition
      { argumentDefinitionName = $(litE $ stringL fieldNameStr)
      , argumentDefinitionDescription = $(litE $ stringL description)
      , argumentDefinitionRequired = $(if isOptionalType fieldType then [| False |] else [| True |])
      } |]

mkSeparateParamsCase :: Name -> Name -> Type -> Q Exp
mkSeparateParamsCase conName handlerName paramType = do
  fields <- extractFieldsFromParamType paramType
  let mkBase vars = do
        paramApp <- buildParameterConstructor paramType vars
        [| do
            content <- $(varE handlerName) ($(conE conName) $(return paramApp))
            pure $ Right content |]
  buildFieldValidation mkBase fields 0

-- Extract field information from parameter type
extractFieldsFromParamType :: Type -> Q [(Name, Bang, Type)]
extractFieldsFromParamType paramType = do
  case paramType of
    ConT typeName -> do
      info <- reify typeName
      case info of
        TyConI (DataD _ _ _ _ [RecC _ fields] _) ->
          return fields
        TyConI (DataD _ _ _ _ [NormalC _ [(_bang, innerType)]] _) ->
          extractFieldsFromParamType innerType
        _ -> fail $ "Parameter type " ++ show typeName ++ " must be a record type or single-parameter constructor"
    _ -> fail $ "Parameter type must be a concrete type, got: " ++ show paramType

-- Build the parameter constructor application recursively
buildParameterConstructor :: Type -> [Name] -> Q Exp
buildParameterConstructor paramType fieldVars = do
  case paramType of
    ConT typeName -> do
      info <- reify typeName
      case info of
        TyConI (DataD _ _ _ _ [RecC conName _] _) -> do
          return $ foldl AppE (ConE conName) (map VarE fieldVars)
        TyConI (DataD _ _ _ _ [NormalC conName [(_bang, innerType)]] _) -> do
          innerConstructor <- buildParameterConstructor innerType fieldVars
          return $ AppE (ConE conName) innerConstructor
        _ -> fail $ "Parameter type " ++ show typeName ++ " must be a record type or single-parameter constructor"
    _ -> fail $ "Parameter type must be a concrete type, got: " ++ show paramType

-- | Check if a type requires parsing that can fail
canParseFail :: Type -> Bool
canParseFail (ConT n) = nameBase n `elem` ["Int", "Integer", "Double", "Float", "Bool"]
canParseFail _        = False

-- | Generate an expression of type @Maybe a@ that attempts to parse a Text value.
-- Returns Nothing on parse failure. Only call for types where 'canParseFail' is True.
mkParseExpr :: Type -> Name -> Q Exp
mkParseExpr innerType rawVar = case innerType of
  ConT n | nameBase n `elem` ["Int", "Integer", "Double", "Float"] ->
    [| readMaybe (T.unpack $(varE rawVar)) |]
  ConT n | nameBase n == "Bool" ->
    [| case T.toLower $(varE rawVar) of
         "true"  -> Just True
         "false" -> Just False
         _       -> Nothing |]
  _ -> [| Just $(varE rawVar) |]

mkRecordCase :: Name -> Name -> [(Name, Bang, Type)] -> Q Exp
mkRecordCase conName handlerName fields = do
  case fields of
    [] -> [| do
        content <- $(varE handlerName) $(conE conName)
        pure $ Right content |]
    _ -> do
      let mkBase vars = do
            let constructorApp = foldl AppE (ConE conName) (map VarE vars)
            [| do
                content <- $(varE handlerName) $(return constructorApp)
                pure $ Right content |]
      buildFieldValidation mkBase fields 0

-- | Unified field validation logic, parameterized by a callback that builds the
-- final expression from the validated field variables.
buildFieldValidation :: ([Name] -> Q Exp) -> [(Name, Bang, Type)] -> Int -> Q Exp
buildFieldValidation mkBase [] depth = do
  let fieldVars = [mkName ("field" ++ show i) | i <- [0..depth-1]]
  mkBase fieldVars

buildFieldValidation mkBase ((fieldName, _, fieldType):remainingFields) depth = do
  let fieldStr = nameBase fieldName
  let innerType = unwrapMaybe fieldType
  let fieldVar = mkName ("field" ++ show depth)

  continuation <- buildFieldValidation mkBase remainingFields (depth + 1)

  if isOptionalType fieldType
    then
      if canParseFail innerType
        then do
          rawFieldVar <- newName ("raw" ++ show depth)
          [| case lookup $(litE $ stringL fieldStr) args of
              Nothing -> do
                let $(varP fieldVar) = Nothing
                $(return continuation)
              Just $(varP rawFieldVar) ->
                case $(mkParseExpr innerType rawFieldVar) of
                  Just parsed -> do
                    let $(varP fieldVar) = Just parsed
                    $(return continuation)
                  Nothing -> pure $ Left $ InvalidParams $
                    "Failed to parse field '" <> $(litE $ stringL fieldStr) <> "' from: " <> $(varE rawFieldVar) |]
        else
          [| do
              let $(varP fieldVar) = lookup $(litE $ stringL fieldStr) args
              $(return continuation) |]
    else
      if canParseFail innerType
        then do
          rawFieldVar <- newName ("raw" ++ show depth)
          [| case lookup $(litE $ stringL fieldStr) args of
              Just $(varP rawFieldVar) ->
                case $(mkParseExpr innerType rawFieldVar) of
                  Just $(varP fieldVar) -> $(return continuation)
                  Nothing -> pure $ Left $ InvalidParams $
                    "Failed to parse field '" <> $(litE $ stringL fieldStr) <> "' from: " <> $(varE rawFieldVar)
              Nothing -> pure $ Left $ MissingRequiredParams $ "field '" <> $(litE $ stringL fieldStr) <> "' is missing" |]
        else
          [| case lookup $(litE $ stringL fieldStr) args of
              Just $(varP fieldVar) -> $(return continuation)
              Nothing -> pure $ Left $ MissingRequiredParams $ "field '" <> $(litE $ stringL fieldStr) <> "' is missing" |]

-- | Derive resource handlers from a data type with custom descriptions
-- Usage: $(deriveResourceHandlerWithDescription ''MyResource 'handleResource [("Constructor", "Description")])
deriveResourceHandlerWithDescription :: Name -> Name -> [(String, String)] -> Q Exp
deriveResourceHandlerWithDescription typeName handlerName descriptions = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ constructors _) -> do
      resourceDefs <- mapM (mkResourceDefWithDescription descriptions) constructors

      listHandlerExp <- [| pure $(return $ ListE resourceDefs) |]

      matches <- mapM (mkResourceMatch handlerName) constructors
      defaultCase <- [| pure $ Left $ ResourceNotFound $ "Resource not found: " <> T.pack unknown |]
      let defaultMatch = Match (VarP (mkName "unknown")) (NormalB defaultCase) []

      readHandlerExp <- return $ LamE [VarP (mkName "uri")] $
        CaseE (AppE (VarE 'show) (VarE (mkName "uri")))
          (matches ++ [defaultMatch])

      return $ TupE [Just listHandlerExp, Just readHandlerExp]
    _ -> fail $ "deriveResourceHandlerWithDescription: " ++ show typeName ++ " is not a data type"

-- | Derive resource handlers from a data type
-- Usage: $(deriveResourceHandler ''MyResource 'handleResource)
deriveResourceHandler :: Name -> Name -> Q Exp
deriveResourceHandler typeName handlerName =
  deriveResourceHandlerWithDescription typeName handlerName []

mkResourceDefWithDescription :: [(String, String)] -> Con -> Q Exp
mkResourceDefWithDescription descriptions (NormalC name []) = do
  let resourceName = T.pack . toSnakeCase . nameBase $ name
  let resourceURI = "resource://" <> T.unpack resourceName
  let constructorName = nameBase name
  let description = case lookup constructorName descriptions of
        Just desc -> Just desc
        Nothing   -> Just constructorName
  [| ResourceDefinition
      { resourceDefinitionURI = $(litE $ stringL resourceURI)
      , resourceDefinitionName = $(litE $ stringL $ T.unpack resourceName)
      , resourceDefinitionDescription = $(case description of
          Just desc -> [| Just $(litE $ stringL desc) |]
          Nothing   -> [| Nothing |])
      , resourceDefinitionMimeType = Just "text/plain"
      , resourceDefinitionTitle = Nothing
      } |]
mkResourceDefWithDescription _ _ = fail "Unsupported constructor type for resources"

mkResourceMatch :: Name -> Con -> Q Match
mkResourceMatch handlerName (NormalC name []) = do
  let resourceName = T.pack . toSnakeCase . nameBase $ name
  let resourceURI = "resource://" <> T.unpack resourceName
  body <- [| $(varE handlerName) $(varE (mkName "uri")) $(conE name) >>= pure . Right |]
  return $ Match (LitP $ StringL resourceURI) (NormalB body) []
mkResourceMatch _ _ = fail "Unsupported constructor type for resources"

-- | Derive tool handlers from a data type with custom descriptions
-- Usage: $(deriveToolHandlerWithDescription ''MyTool 'handleTool [("Constructor", "Description")])
deriveToolHandlerWithDescription :: Name -> Name -> [(String, String)] -> Q Exp
deriveToolHandlerWithDescription typeName handlerName descriptions = do
  info <- reify typeName
  case info of
    TyConI (DataD _ _ _ _ constructors _) -> do
      toolDefs <- mapM (mkToolDefWithDescription descriptions) constructors

      listHandlerExp <- [| pure $(return $ ListE toolDefs) |]

      matches <- mapM (mkHandlerCase handlerName) constructors
      defaultCase <- [| pure $ Left $ UnknownTool $ "Unknown tool: " <> name |]
      let defaultMatch = Match WildP (NormalB defaultCase) []

      callHandlerExp <- return $ LamE [VarP (mkName "name"), VarP (mkName "args")] $
        CaseE (AppE (VarE 'T.unpack) (VarE (mkName "name")))
          (matches ++ [defaultMatch])

      return $ TupE [Just listHandlerExp, Just callHandlerExp]
    _ -> fail $ "deriveToolHandlerWithDescription: " ++ show typeName ++ " is not a data type"

-- | Derive tool handlers from a data type
-- Usage: $(deriveToolHandler ''MyTool 'handleTool)
deriveToolHandler :: Name -> Name -> Q Exp
deriveToolHandler typeName handlerName =
  deriveToolHandlerWithDescription typeName handlerName []

mkToolDefWithDescription :: [(String, String)] -> Con -> Q Exp
mkToolDefWithDescription descriptions con = do
  (snakeName, description, fields) <- getConInfo descriptions "" con
  case con of
    NormalC _ [] ->
      [| ToolDefinition
          { toolDefinitionName = $(litE $ stringL snakeName)
          , toolDefinitionDescription = $(litE $ stringL description)
          , toolDefinitionInputSchema = InputSchemaDefinitionObject
              { properties = []
              , required = []
              }
          , toolDefinitionTitle = Nothing
          } |]
    _ -> do
      props <- mapM (mkProperty descriptions) fields
      let requiredFieldNames = [nameBase fn | (fn, _, ft) <- fields, not (isOptionalType ft)]
      [| ToolDefinition
          { toolDefinitionName = $(litE $ stringL snakeName)
          , toolDefinitionDescription = $(litE $ stringL description)
          , toolDefinitionInputSchema = InputSchemaDefinitionObject
              { properties = $(return $ ListE props)
              , required = $(return $ ListE $ map (LitE . StringL) requiredFieldNames)
              }
          , toolDefinitionTitle = Nothing
          } |]

mkProperty :: [(String, String)] -> (Name, Bang, Type) -> Q Exp
mkProperty descriptions (fieldName, _, fieldType) = do
  let fieldStr = nameBase fieldName
  let description = case lookup fieldStr descriptions of
        Just desc -> desc
        Nothing   -> fieldStr
  let jsonType = jsonTypeOf (unwrapMaybe fieldType)
  [| ($(litE $ stringL fieldStr), InputSchemaDefinitionProperty
      { propertyType = $(litE $ stringL jsonType)
      , propertyDescription = $(litE $ stringL description)
      }) |]
