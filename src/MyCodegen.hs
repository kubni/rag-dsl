module MyCodegen where

import Debug.Trace (trace)

import Data.List (intercalate, intersperse, find)
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





-- def setup_qdrant():
--     qdrant_client = QdrantClient(url="http://localhost:6333")

--     # Mean pooling
--     vector_params_mean = models.VectorParams(
--         size=128,
--         distance=models.Distance.COSINE,
--         multivector_config=models.MultiVectorConfig(
--             comparator=models.MultiVectorComparator.MAX_SIM
--         ),
--     )

--     vector_params_original = models.VectorParams(
--         size=128,
--         distance=models.Distance.COSINE,
--         # on_disk=True,
--         multivector_config=models.MultiVectorConfig(
--             comparator=models.MultiVectorComparator.MAX_SIM
--         ),
--         hnsw_config=models.HnswConfigDiff(m=0),
--     )

--     return qdrant_client, vector_params_original, vector_params_mean









generateNecessarySetups :: [MetaValue] -> [[String]]
generateNecessarySetups mvs =
  [
    [
      "def setup_colqwen(model_name: str, device: str):",
      "    colqwen_model = ColQwen2.from_pretrained(pretrained_model_name_or_path=model_name,torch_dtype=torch.bfloat16,device_map=device)",
      "    colqwen_processor = ColQwen2Processor.from_pretrained(pretrained_model_name_or_path=model_name)",
      "    return colqwen_model, colqwen_processor"
    ]
  ]
  ++ [
       case find (\(header, _) -> snd header == "qdrant") mvs of
        Just qdrantMetaValue -> generateQdrantSetup qdrantMetaValue
        Nothing              -> error "Error: No Qdrant meta value found" -- TODO: Handle this properly
     ]

generateQdrantSetup :: MetaValue -> [String]
generateQdrantSetup (_, keyValuePairs) =
  let dict = Map.fromList keyValuePairs
  in
      -- vp = dict Map.! "vector_parameters"
    [
      "def setup_qdrant():",
      "    qdrant_client = QdrantClient(url=" ++ show (dict Map.! "url") ++ ")",
      case lookup "vector_parameters" keyValuePairs of
        Just (DictVal vpPairs) ->
          let vpMap = Map.fromList vpPairs
          in
            -- TODO: Handle \n
              "    vector_params = models.VectorParams(\n"
              ++ "        size=" ++ show (vpMap Map.! "size") ++ "),\n"
              ++ "        distance=" ++ show (vpMap Map.! "distance_metric") ++ "),\n"
              ++        if (vpMap Map.! "use_multivectors" == (BoolVal True)) then "        multivector_config=models.MultiVectorConfig(comparator=models.MultiVectorComparator.MAX_SIM)" else ""
        _ -> error "Missing 'vector_parameters'"
    ]

processMetaValue :: MetaValue -> [String]
processMetaValue (header, keyValuePairs) = case fst header of
  "model" -> ["err: model header not yet implemented"]
  "db" -> case snd header of
    -- "qdrant" -> generateQdrant keyValuePairs
    "qdrant" -> [valueToPython (DictVal keyValuePairs)] -- FIXME: We don't check the validity of fields here at all
    _ -> ["[processMetaValue] error: unimplemented"]
  _ -> ["[processMetaValue] error: unimplemented"]


codegen :: [MetaValue] -> [String]
codegen parsedDslCode = (intersperse "\n" generateNecessaryImports) ++ (concat $ intersperse ["\n\n"] (generateNecessarySetups parsedDslCode)) ++ (concat $ processMetaValue <$> parsedDslCode)
