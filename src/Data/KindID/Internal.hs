{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.KindID.Internal where

import           Control.Monad
import           Data.Aeson.Types hiding (String)
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Type.Bool
import           Data.Type.Equality
import           Data.Type.Ord
import           Data.TypeID.Class
import           Data.TypeID.Internal (TypeID, TypeIDError)
import qualified Data.TypeID.Internal as TID
import           Data.UUID.V7 (UUID)
import qualified Data.UUID.V7 as V7
import           Data.Word
import           GHC.TypeLits hiding (Text)

-- | A TypeID with the prefix encoded at type level.
--
-- It is dubbed 'KindID' because we the prefix here is a data kind rather than
-- a type.
--
-- Note that the 'Show' instance is for debugging purposes only. To pretty-print
-- a 'KindID', use 'toString', 'toText' or 'toByteString'.
newtype KindID prefix = KindID { _getUUID :: UUID }
  deriving (Eq, Ord, Show)

-- | A constraint for valid prefix 'Symbol's.
type ValidPrefix prefix = ( KnownSymbol prefix
                          , LengthSymbol prefix < 64
                          , IsLowerSymbol prefix ~ 'True )

instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => ToJSON (KindID prefix) where
    toJSON :: KindID prefix -> Value
    toJSON = toJSON . toText
    {-# INLINE toJSON #-}

instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => FromJSON (KindID prefix) where
    parseJSON :: Value -> Parser (KindID prefix)
    parseJSON str = do
      s <- parseJSON str
      case parseText s of
        Left err  -> fail $ show err
        Right kid -> pure kid
    {-# INLINE parseJSON #-}

-- | Get the prefix, 'UUID', and timestamp of a 'KindID'.
--
-- While the instance is available by importing "Data.KindID", the class itself
-- in the future will not be re-exported from "Data.KindID". To use the class
-- explicitly, please import "Data.TypeID.Class".
instance (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
  => IDType (KindID prefix) where
    getPrefix :: KindID prefix -> Text
    getPrefix _ = T.pack (symbolVal (Proxy @(PrefixSymbol prefix)))
    {-# INLINE getPrefix #-}

    getUUID :: KindID prefix -> UUID
    getUUID = _getUUID
    {-# INLINE getUUID #-}

    getTime :: KindID prefix -> Word64
    getTime = V7.getTime . getUUID
    {-# INLINE getTime #-}

-- | Generate a new 'KindID' from a prefix.
--
-- It throws a 'TypeIDError' if the prefix does not match the specification,
-- namely if it's longer than 63 characters or if it contains characters other
-- than lowercase latin letters.
genKindID :: forall prefix. (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
          => IO (KindID prefix)
genKindID = KindID <$> V7.genUUID
{-# INLINE genKindID #-}

-- | Generate n 'KindID's from a prefix.
--
-- It tries its best to generate 'KindID's at the same timestamp, but it may not
-- be possible if we are asking too many 'UUID's at the same time.
--
-- It is guaranteed that the first 32768 'KindID's are generated at the same
-- timestamp.
genKindIDs :: forall prefix
            . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
           => Word16 -> IO [KindID prefix]
genKindIDs n = fmap KindID <$> V7.genUUIDs n
{-# INLINE genKindIDs #-}

-- | The nil 'KindID'.
nil :: KindID ""
nil = KindID V7.nil
{-# INLINE nil #-}

-- | Obtain a 'KindID' from a prefix and a 'UUID'.
decorate :: forall prefix. (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
         => UUID -> KindID prefix
decorate = KindID
{-# INLINE decorate #-}

-- | Convert a 'KindID' to a 'TypeID'.
toTypeID :: forall prefix. (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
         => KindID prefix -> TypeID
toTypeID kid = TID.TypeID (getPrefix kid) (getUUID kid)
{-# INLINE toTypeID #-}

-- | Convert a 'TypeID' to a 'KindID'. If the actual prefix does not match
-- with the expected one as defined by the type, it returns @Nothing@.
fromTypeID :: forall prefix
            . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
           => TypeID -> Maybe (KindID prefix)
fromTypeID tid = do
  guard (T.pack (symbolVal (Proxy @(PrefixSymbol prefix))) == getPrefix tid)
  pure $ KindID (getUUID tid)
{-# INLINE fromTypeID #-}

-- | Pretty-print a 'KindID'.
toString :: forall prefix. (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
         => KindID prefix -> String
toString = TID.toString . toTypeID
{-# INLINE toString #-}

-- | Pretty-print a 'KindID' to strict 'Text'.
toText :: forall prefix. (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
       => KindID prefix -> Text
toText = TID.toText . toTypeID
{-# INLINE toText #-}

-- | Pretty-print a 'KindID' to lazy 'ByteString'.
toByteString :: forall prefix
              . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
             => KindID prefix -> ByteString
toByteString = TID.toByteString . toTypeID
{-# INLINE toByteString #-}

-- | Parse a 'KindID' from its 'String' representation.
parseString :: forall prefix
             . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
            => String -> Either TID.TypeIDError (KindID prefix)
parseString str = do
  tid <- TID.parseString str
  case fromTypeID tid of
    Nothing  -> Left $ TID.TypeIDErrorPrefixMismatch
                       (T.pack (symbolVal (Proxy @(PrefixSymbol prefix))))
                       (getPrefix tid)
    Just kid -> pure kid
{-# INLINE parseString #-}

-- | Parse a 'KindID' from its string representation as a strict 'Text'.
parseText :: forall prefix. (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
          => Text -> Either TypeIDError (KindID prefix)
parseText str = do
  tid <- TID.parseText str
  case fromTypeID tid of
    Nothing  -> Left $ TID.TypeIDErrorPrefixMismatch
                       (T.pack (symbolVal (Proxy @(PrefixSymbol prefix))))
                       (getPrefix tid)
    Just kid -> pure kid
{-# INLINE parseText #-}

-- | Parse a 'KindID' from its string representation as a lazy 'ByteString'.
parseByteString :: forall prefix
                 . (ToPrefix prefix, ValidPrefix (PrefixSymbol prefix))
                => ByteString -> Either TID.TypeIDError (KindID prefix)
parseByteString str = do
  tid <- TID.parseByteString str
  case fromTypeID tid of
    Nothing  -> Left $ TID.TypeIDErrorPrefixMismatch
                       (T.pack (symbolVal (Proxy @(PrefixSymbol prefix))))
                       (getPrefix tid)
    Just kid -> pure kid
{-# INLINE parseByteString #-}

type family LengthSymbol (prefix :: Symbol) :: Nat where
  LengthSymbol prefix = LSUH (UnconsSymbol prefix)

-- | Length Symbol Uncons Helper.
type family LSUH (uncons :: Maybe (Char, Symbol)) :: Nat where
  LSUH 'Nothing        = 0
  LSUH ('Just '(c, s)) = 1 + LengthSymbol s

type family IsLowerChar (ch :: Char) :: Bool where
  IsLowerChar ch = Compare '`' ch == LT && Compare ch '{' == LT

type family IsLowerSymbol (prefix :: Symbol) :: Bool where
  IsLowerSymbol prefix = ILSUH (UnconsSymbol prefix)

-- | Is Lower Symbol Uncons Helper.
type family ILSUH (uncons :: Maybe (Char, Symbol)) :: Bool where
  ILSUH 'Nothing        = 'True
  ILSUH ('Just '(c, s)) = IsLowerChar c && IsLowerSymbol s

-- | A class that translates any kind to a 'Symbol'. It is used to translate
-- custom data kinds to a 'Symbol' so that they can be used as 'KindID'
-- prefixes.
--
-- For example, suppose we have the following data structure that represents the
-- prefixes we are going to use:
--
-- > data Prefix = User | Post | Comment
--
-- Then we can make it an instance of 'ToPrefix' like this:
--
-- > instance ToPrefix 'User where
-- >   type PrefixSymbol 'User = "user"
-- >
-- > instance ToPrefix 'Post where
-- >   type PrefixSymbol 'Post = "post"
-- >
-- > instance ToPrefix 'Comment where
-- >   type PrefixSymbol 'Comment = "comment"
--
-- Now we can use Prefix as a prefix for 'KindID's, e.g.
--
-- > do
-- >   userID <- genKindID @'User -- Same as genKindID @"user"
-- >   postID <- genKindID @'Post -- Same as genKindID @"post"
-- >   commentID <- genKindID @'Comment -- Same as genKindID @"comment"
class ToPrefix a where
  type PrefixSymbol a :: Symbol

instance ToPrefix (a :: Symbol) where
  type PrefixSymbol a = a
