module Duet.Rpc.JsonRpc.Types
  ( RequestId (..),
    JsonRpcRequest (..),
    JsonRpcResponse (..),
    JsonRpcErrorDetail (..),
    JsonRpcError (..),
    jsonRpcVersion,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), (.:), (.:?), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Scientific (Scientific)
import Data.Text (Text)

jsonRpcVersion :: Text
jsonRpcVersion = "2.0"

data RequestId
  = IdString Text
  | IdNumber Scientific
  | IdNull
  deriving (Eq, Show)

data JsonRpcRequest = JsonRpcRequest
  { jsonrpc :: Text,
    method :: Text,
    params :: Maybe Value,
    id :: Maybe RequestId
  }
  deriving (Eq, Show)

data JsonRpcResponse = JsonRpcResponse
  { jsonrpc :: Text,
    result :: Value,
    id :: RequestId
  }
  deriving (Eq, Show)

data JsonRpcErrorDetail = JsonRpcErrorDetail
  { code :: Int,
    message :: Text,
    errorData :: Maybe Value
  }
  deriving (Eq, Show)

data JsonRpcError = JsonRpcError
  { jsonrpc :: Text,
    errorDetail :: JsonRpcErrorDetail,
    id :: RequestId
  }
  deriving (Eq, Show)

instance FromJSON RequestId where
  parseJSON (String t) = return (IdString t)
  parseJSON (Number n) = return (IdNumber n)
  parseJSON Null = return IdNull
  parseJSON v@(Bool _) = A.typeMismatch "String, Number, or Null" v
  parseJSON v@(Array _) = A.typeMismatch "String, Number, or Null" v
  parseJSON v@(Object _) = A.typeMismatch "String, Number, or Null" v

instance ToJSON RequestId where
  toJSON (IdString t) = String t
  toJSON (IdNumber n) = Number n
  toJSON IdNull = Null

instance FromJSON JsonRpcRequest where
  parseJSON = A.withObject "JsonRpcRequest" $ \obj -> do
    v <- obj .: "jsonrpc"
    if v /= jsonRpcVersion
      then A.parseFail "jsonrpc field must be \"2.0\""
      else do
        m <- obj .: "method"
        p <- obj .:? "params"
        i <- obj .:? "id"
        return $ JsonRpcRequest v m p i

instance ToJSON JsonRpcResponse where
  toJSON (JsonRpcResponse v r i) =
    A.object
      [ "jsonrpc" .= v,
        "result" .= r,
        "id" .= i
      ]

instance ToJSON JsonRpcErrorDetail where
  toJSON (JsonRpcErrorDetail c m d) =
    A.object $
      [ "code" .= c,
        "message" .= m
      ]
        ++ maybe [] (\val -> ["data" .= val]) d

instance ToJSON JsonRpcError where
  toJSON (JsonRpcError v e i) =
    A.object
      [ "jsonrpc" .= v,
        "error" .= e,
        "id" .= i
      ]
