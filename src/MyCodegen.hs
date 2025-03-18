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
generateNecessaryImports = [
  "from colpali_engine.models import ColQwen2, ColQwen2Processor"
  , "from pdf2image import convert_from_path"
  , "from qdrant_client import QdrantClient"
  , "from qdrant_client.http import models"
  , "from minio import Minio"
  , "import base64"
  , "import torch"
 ]


generateNecessarySetups :: [(MetaHeader, Map String Value)] -> [[String]]
generateNecessarySetups metaValues =
  [
    intersperse "\n"
    [
      "def setup_colqwen(model_name: str, device: str):",
      "    colqwen_model = ColQwen2.from_pretrained(pretrained_model_name_or_path=model_name,torch_dtype=torch.bfloat16,device_map=device)",
      "    colqwen_processor = ColQwen2Processor.from_pretrained(pretrained_model_name_or_path=model_name)",
      "    return colqwen_model, colqwen_processor"
    ]
  ]
  ++ [
       case find (\(header, _) -> snd header == "qdrant") metaValues of
        Just (_, keyValuePairsMap) -> generateQdrantSetup keyValuePairsMap
        Nothing              -> error "Error: No Qdrant meta value found" -- TODO: Handle this properly
     ]


-- TODO: Extract actual values out of StringVal, IntVal, BoolVal, ...
generateQdrantSetup :: Map String Value -> [String]
generateQdrantSetup pairsMap =
    intersperse "\n"
    [
      "def setup_qdrant():",
      "    qdrant_client = QdrantClient(url=" ++ show (pairsMap Map.! "url") ++ ")",
      generateQdrantVectorParameters pairsMap,
      "    return qdrant_client, vector_params"
    ]


generateQdrantVectorParameters :: Map String Value -> String
generateQdrantVectorParameters pairsDict =
  case pairsDict Map.! "vector_parameters" of
    DictVal vp ->
      let vpMap = Map.fromList vp
          distance_metric = vpMap Map.! "distance_metric"
      in
        intercalate "\n"  [
          "    vector_params = models.VectorParams(",
          "        size=" ++ show (vpMap Map.! "size") ++ ",",
          "        distance=models.Distance." ++ (if distance_metric == StringVal "cosine" then "COSINE" else "DOT") ++ ",", -- TODO: Handle other distance metrics
          "        on_disk=" ++ show (vpMap Map.! "on_disk") ++ ",",
                  if (vpMap Map.! "use_multivectors" == (BoolVal True)) then "        multivector_config=models.MultiVectorConfig(comparator=models.MultiVectorComparator.MAX_SIM)" else "",
          "    )"
        ]
    _ -> error "Error: Vector parameters isn't a map!"




-- TODO: Unneeded, remove?
-- processMetaValue :: MetaValue -> [String]
-- processMetaValue (header, keyValuePairs) = case fst header of
--   "model" -> ["err: model header not yet implemented"]
--   "db" -> case snd header of
--     -- "qdrant" -> generateQdrant keyValuePairs
--     "qdrant" -> [valueToPython (DictVal keyValuePairs)]
--     _ -> ["[processMetaValue] error: unimplemented"]
--   _ -> ["[processMetaValue] error: unimplemented"]


-- generateSpace :: Int -> String
-- generateSpace amount = replicate amount ' '

generateTab :: Int -> String
generateTab amount = replicate (amount*4) ' '




generateQdrantCollection :: Map String Value -> [String]
generateQdrantCollection pairsDict =
  case pairsDict Map.! "collection_parameters" of
    DictVal cp ->
      let cpMap = Map.fromList cp
          doForceRecreate = cpMap Map.! "force_recreate"
          collectionName = cpMap Map.! "name"
      in
       [
          "def create_qdrant_collection(flag_force_recreate: bool, collection_name: str, qdrant_client: QdrantClient):",
          (generateTab 1) ++ "if flag_force_recreate:",
          -- TODO: Handle the force_recreate=False case
          if doForceRecreate == (BoolVal True) then (generateTab 2) ++ "qdrant_client.delete_collection(collection_name=" ++ show collectionName ++ ")" else "",
          (generateTab 2) ++ "qdrant_client.create_collection(",
          (generateTab 3) ++ "collection_name=collection_name,",  -- TODO: Use show collection_name here or use it in main and pass it as collection_name argument to this
          (generateTab 3) ++ "shard_number=1,",
          (generateTab 3) ++ "on_disk_payload=False,",
          (generateTab 3) ++ "optimizers_config=models.OptimizersConfigDiff(indexing_threshold=0)",
          (generateTab 3) ++ "vectors_config={",
          (generateTab 4) ++ "\"initial\": vector_params",
          (generateTab 3) ++ "},",
          (generateTab 2) ++ ")"

        ]

    _ -> error "Error: Vector parameters isn't a map!"


codegen :: [MetaValue] -> [String]
codegen parsedDslCode =
  let parsedCodeWithMaps = zip (fst <$> parsedDslCode) (Map.fromList . snd <$> parsedDslCode)
      qdrantMetaValue = case lookup ("db", "qdrant") parsedCodeWithMaps of
        Just qmv -> qmv
        Nothing -> error "Qdrant meta value missing"
  in
    (intersperse "\n" generateNecessaryImports)
    ++ ["\n\n"]
    ++ (concat $ intersperse ["\n\n"] (generateNecessarySetups parsedCodeWithMaps))
    ++ ["\n\n"]
    ++ intersperse "\n" (generateQdrantCollection qdrantMetaValue)



-- TODO: Maybe we can make MetaValue' that is like MetaValue but with [KeyValuePair] actually being a Map?

-- TODO: Indent with some function that manipulates strings, not by hand
-- FIXME: We don't check the validity of fields at all

-- TODO: Solve indentation and if blocks with some functions...
-- Tab could be a function that accepts the number of spaces


-- TODO: Improve the generateTab logic, and then use it instead of hardcoded tabs in all the places, not just generateQdrantCollection
