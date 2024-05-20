{-# LANGUAGE LambdaCase #-}
module Bot.Commands.Decoder where
import           Control.Applicative
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Text
import qualified Data.Text             as T
import           Discord.Interactions
import           Discord.Internal.Rest

type Failure f r = String -> f r
type Success a f r = a -> f r

newtype Decoder f r a = Decoder
  { runDecoder :: Failure f r -> Success a f r -> f r
  }

instance Functor (Decoder f r) where
  fmap x (Decoder y) = Decoder $ \f s -> y f (s . x)

instance Monad (Decoder f r) where
  (Decoder fa) >>= f = Decoder $ \failure success ->
    fa failure $ \a ->
      let Decoder g = f a
      in g failure success

instance Applicative (Decoder f r) where
  pure a = Decoder $ \_f s -> s a
  (Decoder fab) <*> (Decoder fa)= Decoder $ \failure success ->
    fab failure $ \ab -> fa failure (success . ab)

instance Alternative (Decoder f r) where
  empty = Decoder $ \f _s -> f "Empty"
  (Decoder p) <|> (Decoder q) = Decoder $ \f s ->
    p (\_ -> q f s) s

instance MonadFail (Decoder f r) where
  fail msg = Decoder $ \f _s -> f msg

failText :: Text -> Decoder f r a
failText = fail . T.unpack

withOptionsDataSubcommands :: ([OptionDataSubcommandOrGroup] -> Decoder f r a) -> OptionsData -> Decoder f r a
withOptionsDataSubcommands f = \case
  OptionsDataSubcommands l -> f l
  _                        -> fail "Did not get OptionsDataSubcommands"

withOptionsDataValues :: ([OptionDataValue] -> Decoder f r a) -> OptionsData -> Decoder f r a
withOptionsDataValues f = \case
  OptionsDataValues l -> f l
  _                   -> fail "Did not get OptionsDataDataValues"

withOptionDataSubcommandGroup :: (Text -> [OptionDataSubcommand] -> Bool -> Decoder f r a) -> OptionDataSubcommandOrGroup -> Decoder f r a
withOptionDataSubcommandGroup g = \case
  OptionDataSubcommandGroup n o f -> g n o f
  _ -> fail "Did not get OptionDataSubcommandGroup"

withOptionDataSubcommandOrGroupSubcommand :: (OptionDataSubcommand -> Decoder f r a) -> OptionDataSubcommandOrGroup -> Decoder f r a
withOptionDataSubcommandOrGroupSubcommand f = \case
  OptionDataSubcommandOrGroupSubcommand d -> f d
  _ -> fail "Did not get OptionDataSubcommandOrGroupSubcommand"

withOptionDataValueString :: (Text -> Either Text Text -> Decoder f r a) -> OptionDataValue -> Decoder f r a
withOptionDataValueString f = \case
  OptionDataValueString n v -> f n v
  _                         -> fail "Did not get OptionDataValueString"
withOptionDataValueInteger :: (Text -> Either Text Integer -> Decoder f r a) -> OptionDataValue -> Decoder f r a
withOptionDataValueInteger f = \case
  OptionDataValueInteger n v -> f n v
  _                          -> fail "Did not get OptionDataValueInteger"
withOptionDataValueBoolean :: (Text -> Bool -> Decoder f r a) -> OptionDataValue -> Decoder f r a
withOptionDataValueBoolean f = \case
  OptionDataValueBoolean n v -> f n v
  _                          -> fail "Did not get OptionDataValueBoolean"
withOptionDataValueUser :: (Text -> UserId -> Decoder f r a) -> OptionDataValue -> Decoder f r a
withOptionDataValueUser f = \case
  OptionDataValueUser n v -> f n v
  _                       -> fail "Did not get OptionDataValueUser"
withOptionDataValueChannel :: (Text -> ChannelId -> Decoder f r a) -> OptionDataValue -> Decoder f r a
withOptionDataValueChannel f = \case
  OptionDataValueChannel n v -> f n v
  _                          -> fail "Did not get OptionDataValueChannel"
withOptionDataValueRole :: (Text -> RoleId -> Decoder f r a) -> OptionDataValue -> Decoder f r a
withOptionDataValueRole f = \case
  OptionDataValueRole n v -> f n v
  _                       -> fail "Did not get OptionDataValueRole"
withOptionDataValueMentionable :: (Text -> Snowflake -> Decoder f r a) -> OptionDataValue -> Decoder f r a
withOptionDataValueMentionable f = \case
  OptionDataValueMentionable n v -> f n v
  _ -> fail "Did not get OptionDataValueMentionable"
withOptionDataValueNumber :: (Text -> Either Text Number -> Decoder f r a) -> OptionDataValue -> Decoder f r a
withOptionDataValueNumber f = \case
  OptionDataValueNumber n v -> f n v
  _                         -> fail "Did not get OptionDataValueNumber"

class HasName o where
  name :: o -> Text

instance HasName OptionDataSubcommandOrGroup where
  name (OptionDataSubcommandGroup n _ _) = n
  name (OptionDataSubcommandOrGroupSubcommand (OptionDataSubcommand n _ _)) = n

instance HasName OptionDataSubcommand where
  name (OptionDataSubcommand n _ _) = n

instance HasName OptionDataValue where
  name = optionDataValueName

mapNames :: HasName o => [o] -> Map Text o
mapNames l = M.fromList $ (\o -> (name o, o)) <$> l

named :: Text -> (o -> Decoder f r a) -> Map Text o -> Decoder f r a
named n f = maybe (fail $ "Missing named value: " <> T.unpack n) f . M.lookup n

namedOptional :: Text -> (o -> Decoder f r a) -> Map Text o -> Decoder f r (Maybe a)
namedOptional n f = maybe (pure Nothing) (fmap pure . f) . M.lookup n