module Control.Monad.Map.DynamoMap where

import Prelude

import Control.Monad.Map.Class      (class MonadMap)
import Control.Monad.Reader.Trans   (ReaderT, ask, lift)
import Data.List                    as L
import Data.Maybe                   (Maybe)
import Data.Symbol                  (class IsSymbol, SProxy(..), reflectSymbol)
import Effect.Aff                   (Aff)
import Foreign.Generic              (class Decode)
import Network.AWS.DynamoDB.DocumentClient as P


newtype DynamoMap (t :: Symbol) k v a = DynamoMap (ReaderT P.DocumentClient Aff a)

instance monadMapDynamoMap :: (IsSymbol t, Decode v) => MonadMap k v (DynamoMap t k v) where
        delete k = DynamoMap $ do
            docClient <- ask
            lift $ P.delete docClient
                { "TableName": reflectSymbol (SProxy :: SProxy t)
                , "Key": k
                }
        insert k v = DynamoMap $ do
            docClient <- ask
            lift $ P.put docClient
                { "TableName": reflectSymbol (SProxy :: SProxy t)
                , "Item": v
                }
        lookup k = DynamoMap $ do
            docClient <- ask
            let params = 
                    { "TableName": reflectSymbol (SProxy :: SProxy t)
                    , "Key": k
                    }
            { "Item": v } <- lift $ (P.get docClient params :: Aff { "Item" :: Maybe v })
            pure v
        values = DynamoMap $ do
            docClient <- ask
            let params = { "TableName": reflectSymbol (SProxy :: SProxy t) }
            { "Items": vs } <- lift $ (P.scan docClient params :: Aff { "Items" :: Array v })
            pure $ L.fromFoldable vs
