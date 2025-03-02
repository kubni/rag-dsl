module MyCodegen where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import MyParser (KeyValuePair, MetaHeader, MetaValue, Statement (..), Value (..))

keyValuePairToString :: KeyValuePair -> String
keyValuePairToString (k, v) = k ++ ": " ++ valueToPython v

valueToPython :: Value -> String
valueToPython value = case value of
  IntVal v -> show v
  StringVal v -> "'" ++ v ++ "'"
  BoolVal v -> show v
  ListVal l -> "[" ++ intercalate ", " (map valueToPython l) ++ "]"
  DictVal d -> "{" ++ intercalate ", " (map keyValuePairToString d) ++ "}"

-- TODO: Possibly unneeded?
statementToPython :: Statement -> String
statementToPython statement = case statement of
  Assign var value -> var ++ " = " ++ valueToPython value
  Print var -> "print(" ++ var ++ ")"

-- Hardcoded imports that are used by every program we make
-- Minio is not necessarily required in 100% of cases, but it makes for a better storage of huge payloads that come with base64 data, for which qdrant wasn't optimized.
-- It also prevents occasional timeouts that Qdrant experiences in such cases.
generateNecessaryImports :: [String]
generateNecessaryImports = ["from colpali_engine.models import ColQwen2, ColQwen2Processor", "from pdf2image import convert_from_path", "from minio import Minio", "import base64", "import torch"]

-- TODO: Handle \t and \n inside of the strings
generateNecessarySetups :: [String]
generateNecessarySetups =
  [ "def setup_colqwen(model_name: str, device: str) -> Tuple[PreTrainedModel, Colqwen2ProcessorAliasType]:",
    "\tcolqwen_model = ColQwen2.from_pretrained(pretrained_model_name_or_path=model_name,torch_dtype=torch.bfloat16,device_map=device)",
    "\tcolqwen_processor = ColQwen2Processor.from_pretrained(pretrained_model_name_or_path=model_name)",
    "\treturn colqwen_model, colqwen_processor"
  ]

processMetaValue :: MetaValue -> [String]
processMetaValue (header, keyValuePairs) = case fst header of
  "model" -> ["err: model header not yet implemented"]
  "db" -> case snd header of
    "qdrant" -> generateQdrant keyValuePairs
    _ -> ["[processMetaValue] error: unimplemented"]
  _ -> ["[processMetaValue] error: unimplemented"]

generateQdrant :: [KeyValuePair] -> [String]
generateQdrant pairs =
  let m = Map.fromList pairs
      url = case Map.lookup "url" m of
        Just (StringVal url) -> url
        Just _ -> ""
        Nothing -> ""
   in [ case Map.lookup "local" m of
          Just (StringVal "True") -> "qdrant_client = QdrantClient(url=" ++ url ++ ")"
          Just (StringVal "False") -> "FIXME: QdrantCloud code gen not yet implemented"
          Just _ -> ""
          Nothing -> "",
        case Map.lookup "vector_parameters" m of
          Just (DictVal vector_params) -> processQdrantVectorParameters vector_params
          Just _ -> ""
          Nothing -> "",
        case Map.lookup "collection" m of
          Just (DictVal collection_params) -> processQdrantCollectionParameters collection_params
          Just _ -> ""
          Nothing -> ""
      ]

-- NOTE: It makes more sense to have a list of strings here since its a multi-line generation, but it wouldn't be compatible with the upper functions
-- TODO: Make the cases in generateQdrant function return [String], which will make it have [[String]], but after the cases are done,
--       flatten the list so that one-element lists are just turned into strings, and multi-element lists are turned into a single string but with newline separators for lines
{--
      size: 128,
      distance_metric: 'cosine',
      on_disk: True,
      use_multivectors: True,
      quantization: 'binary'

--}

-- TODO: Should we just use a dict parser and then afterwards check the values?
-- TODO: Should we check the values here or afterwards? # TODO TODO: Important
-- TODO: Doesn't this only return values and not keys associated with them?
processQdrantVectorParameters :: [KeyValuePair] -> [String]
processQdrantVectorParameters vector_params =
  let vpm = Map.fromList vector_params
   in [ case Map.lookup "size" vpm of
          Just (IntVal size) -> show size
          Just _ -> "[processQdrantVectorParameters] Error: Size isn't an IntVal"
          Nothing -> "[processQdrantVectorParameters] Error: Size not specified",
        case Map.lookup "distance_metric" vpm of
          Just (StringVal distance_metric) -> show distance_metric
          Just _ -> "[processQdrantVectorParameters] Error: distance_metric isn't a StringVal"
          Nothing -> "[processQdrantVectorParameters] Error: distance_metric isn't specified",
        case Map.lookup "on_disk" vpm of
          Just (BoolVal on_disk) -> show on_disk
          Just _ -> "[processQdrantVectorParameters] Error: on_disk isn't a BoolVal "
          Nothing -> "[processQdrantVectorParameters] Error: on_disk isn't specified ",
        -- TODO: Maybe this should be a default, since without it Colqwen can't be used...
        case Map.lookup "use_multivectors" vpm of
          Just (BoolVal use_multivectors) -> show use_multivectors
          Just _ -> "[processQdrantVectorParameters] Error: use_multivectors isn't a BoolVal"
          Nothing -> "[processQdrantVectorParameters] Error: use_multivectors isn't specified ",
        case Map.lookup "quantization" vpm of
          Just (StringVal quantization) -> quantization
          Just _ -> "[processQdrantVectorParameters] Error: quantization isn't a StringVal"
          Nothing -> "[processQdrantVectorParameters] Error: quantization isn't specified "
      ]

processQdrantCollectionParameters :: [KeyValuePair] -> [String]
processQdrantCollectionParameters collection_params =
  let cpm = Map.fromList collection_params
   in [ case Map.lookup "name" cpm of
          Just (StringVal name) -> name
          Just _ -> "[processQdrantCollectionParameters] Error: Name isn't a StringVal"
          Nothing -> "[processQdrantCollectionParameters] Error: Name isn't specified",
        case Map.lookup "force_recreate" cpm of
          Just (BoolVal force_recreate) -> show force_recreate
          Just _ -> "[processQdrantCollectionParameters] Error: force_recreate isn't a BoolVal"
          Nothing -> "[processQdrantCollectionParameters] Error: force_recreate isn't specified"
      ]

-- FIXME: processQdrantVectorParameters and procesQdrantCollectionParameters maybe can be merged into one function, that accepts the allowed keys
--        NOTE: How would we get specialized error messages for each case then?
