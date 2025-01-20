module MyCodegen where

import Data.List (intercalate)
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

generateQdrant :: [KeyValuePair]

{-
TODO:
   1) Create Python types here (it will all come down to String or [String], but for the sake of easier indentation care maybe?)
   1) Something like MetaHeader -> [String], based on the header ((db, qdrant) for example) it could know how to parse the MetaValue that comes after
-}
