module MyCodegen where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import MyParser (KeyValuePair, MetaHeader, MetaValue, Value (..))

keyValuePairToString :: KeyValuePair -> String
keyValuePairToString (k, v) = k ++ ": " ++ valueToPython v

valueToPython :: Value -> String
valueToPython value = case value of
  IntVal v -> show v
  StringVal v -> "'" ++ v ++ "'"
  BoolVal v -> show v
  ListVal l -> "[" ++ intercalate ", " (map valueToPython l) ++ "]"
  DictVal d -> "{" ++ intercalate ", " (map keyValuePairToString d) ++ "}"

-- Hardcoded imports that are used by every program we make
-- Minio is not necessarily required in 100% of cases, but it makes for a better storage of huge payloads that come with base64 data, for which qdrant wasn't optimized.
-- It also prevents occasional timeouts that Qdrant experiences in such cases.
generateNecessaryImports :: [String]
generateNecessaryImports = ["from colpali_engine.models import ColQwen2, ColQwen2Processor", "from pdf2image import convert_from_path", "from minio import Minio", "import base64", "import torch"]

-- FIXME: Handle \t and \n inside of the strings
generateNecessarySetups :: [String]
generateNecessarySetups =
  [ "def setup_colqwen(model_name: str, device: str) -> Tuple[PreTrainedModel, Colqwen2ProcessorAliasType]:",
    "    colqwen_model = ColQwen2.from_pretrained(pretrained_model_name_or_path=model_name,torch_dtype=torch.bfloat16,device_map=device)",
    "    colqwen_processor = ColQwen2Processor.from_pretrained(pretrained_model_name_or_path=model_name)",
    "    return colqwen_model, colqwen_processor"
  ]

processMetaValue :: MetaValue -> [String]
processMetaValue (header, keyValuePairs) = case fst header of
  "model" -> ["err: model header not yet implemented"]
  "db" -> case snd header of
    -- "qdrant" -> generateQdrant keyValuePairs
    "qdrant" -> [valueToPython (DictVal keyValuePairs)] -- FIXME: We don't check the validity of fields here at all
    _ -> ["[processMetaValue] error: unimplemented"]
  _ -> ["[processMetaValue] error: unimplemented"]
