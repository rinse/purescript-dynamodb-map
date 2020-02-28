module Control.Monad.Map.DynamoMap where

import Prelude

import Control.Alt                (class Alt)
import Control.Monad.Error.Class  (class MonadThrow, class MonadError)
import Control.Monad.Map.Class    (class MonadMap)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Reader.Trans (ReaderT, ask, lift, runReaderT)
import Control.Monad.Rec.Class    (class MonadRec)
import Control.Plus               (class Plus)
import Data.List                  as L
import Data.Maybe                 (Maybe)
import Data.Newtype               (class Newtype, unwrap)
import Data.Symbol                (class IsSymbol, SProxy(..), reflectSymbol)
import Effect.Aff                 (Aff, Error)
import Effect.Class               (class MonadEffect)
import Foreign.Generic            (class Decode)
import Network.AWS.DynamoDB.DocumentClient as P


newtype DynamoMap (t :: Symbol) k v a = DynamoMap (ReaderT P.DocumentClient Aff a)

runDynamoMap :: forall t k v a. DynamoMap t k v a -> P.DocumentClient -> Aff a
runDynamoMap = runReaderT <<< unwrap

derive         instance newtypeDynamoMap     :: Newtype (DynamoMap t k v a) _
derive newtype instance functorDynamoMap     :: Functor (DynamoMap t k v)
derive newtype instance applyDynamoMap       :: Apply (DynamoMap t k v)
derive newtype instance applicativeDynamoMap :: Applicative (DynamoMap t k v)
derive newtype instance altDynamoMap         :: Alt (DynamoMap t k v)
derive newtype instance plusDynamoMap        :: Plus (DynamoMap t k v)
derive newtype instance bindDynamoMap        :: Bind (DynamoMap t k v)
derive newtype instance monadDynamoMap       :: Monad (DynamoMap t k v)
derive newtype instance semigroupDynamoMap   :: Semigroup a => Semigroup (DynamoMap t k v a)
derive newtype instance monoidDynamoMap      :: Monoid a => Monoid (DynamoMap t k v a)
derive newtype instance monadEffectDynamoMap :: MonadEffect (DynamoMap t k v)
derive newtype instance monadThrowDynamoMap  :: MonadThrow Error (DynamoMap t k v)
derive newtype instance monadErrorDynamoMap  :: MonadError Error (DynamoMap t k v)
derive newtype instance monadAskDynamoMap    :: MonadAsk P.DocumentClient (DynamoMap t k v)
derive newtype instance monadReaderDynamoMap :: MonadReader P.DocumentClient (DynamoMap t k v)
derive newtype instance monadRecDynamoMap    :: MonadRec (DynamoMap t k v)

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
