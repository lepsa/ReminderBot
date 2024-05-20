{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}

module Bot.Commands.Decoder where
import           Control.Applicative
import           Data.Kind
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Text
import qualified Data.Text             as T
import           Discord.Interactions
import           Discord.Internal.Rest
import Control.Monad

-- This is the same as the `aeson` "parser" but with a less
-- confusing name and no path information.

type Failure f r = String -> f r
type Success a f r = a -> f r

newtype Decoder a = Decoder
  { runDecoder :: forall (f :: (Type -> Type)) (r :: Type). Failure f r -> Success a f r -> f r
  }

decode :: MonadFail m => Decoder a -> m a
decode d = runDecoder d fail pure

decodeEither :: Decoder a -> Either String a
decodeEither d = runDecoder d Left pure

instance Functor Decoder where
  fmap x (Decoder y) = Decoder $ \f s -> y f (s . x)

instance Monad Decoder where
  (Decoder fa) >>= f = Decoder $ \failure success ->
    fa failure $ \a ->
      let (Decoder g) = f a
      in g failure success

instance Applicative Decoder where
  pure a = Decoder $ \_f s -> s a
  (Decoder fab) <*> (Decoder fa) = Decoder $ \failure success ->
    fab failure $ \ab -> fa failure (success . ab)

instance Alternative Decoder where
  empty = Decoder $ \f _s -> f "Empty"
  (Decoder p) <|> (Decoder q) = Decoder $ \f s ->
    p (\_ -> q f s) s

instance MonadFail Decoder where
  fail msg = Decoder $ \f _s -> f msg

instance MonadPlus Decoder

failText :: Text -> Decoder a
failText = fail . T.unpack

withOptionsDataSubcommands :: ([OptionDataSubcommandOrGroup] -> Decoder a) -> OptionsData -> Decoder a
withOptionsDataSubcommands f = \case
  OptionsDataSubcommands l -> f l
  _                        -> fail "Did not get OptionsDataSubcommands"

withOptionsDataValues :: ([OptionDataValue] -> Decoder a) -> OptionsData -> Decoder a
withOptionsDataValues f = \case
  OptionsDataValues l -> f l
  _                   -> fail "Did not get OptionsDataDataValues"

withOptionDataSubcommandGroup :: (Text -> [OptionDataSubcommand] -> Bool -> Decoder a) -> OptionDataSubcommandOrGroup -> Decoder a
withOptionDataSubcommandGroup g = \case
  OptionDataSubcommandGroup n o f -> g n o f
  _                               -> fail "Did not get OptionDataSubcommandGroup"

withOptionDataSubcommandOrGroupSubcommand :: (OptionDataSubcommand -> Decoder a) -> OptionDataSubcommandOrGroup -> Decoder a
withOptionDataSubcommandOrGroupSubcommand f = \case
  OptionDataSubcommandOrGroupSubcommand d -> f d
  _                                       -> fail "Did not get OptionDataSubcommandOrGroupSubcommand"

withOptionDataValueString :: (Text -> Either Text Text -> Decoder a) -> OptionDataValue -> Decoder a
withOptionDataValueString f = \case
  OptionDataValueString n v -> f n v
  _                         -> fail "Did not get OptionDataValueString"
withOptionDataValueInteger :: (Text -> Either Text Integer -> Decoder a) -> OptionDataValue -> Decoder a
withOptionDataValueInteger f = \case
  OptionDataValueInteger n v -> f n v
  _                          -> fail "Did not get OptionDataValueInteger"
withOptionDataValueBoolean :: (Text -> Bool -> Decoder a) -> OptionDataValue -> Decoder a
withOptionDataValueBoolean f = \case
  OptionDataValueBoolean n v -> f n v
  _                          -> fail "Did not get OptionDataValueBoolean"
withOptionDataValueUser :: (Text -> UserId -> Decoder a) -> OptionDataValue -> Decoder a
withOptionDataValueUser f = \case
  OptionDataValueUser n v -> f n v
  _                       -> fail "Did not get OptionDataValueUser"
withOptionDataValueChannel :: (Text -> ChannelId -> Decoder a) -> OptionDataValue -> Decoder a
withOptionDataValueChannel f = \case
  OptionDataValueChannel n v -> f n v
  _                          -> fail "Did not get OptionDataValueChannel"
withOptionDataValueRole :: (Text -> RoleId -> Decoder a) -> OptionDataValue -> Decoder a
withOptionDataValueRole f = \case
  OptionDataValueRole n v -> f n v
  _                       -> fail "Did not get OptionDataValueRole"
withOptionDataValueMentionable :: (Text -> Snowflake -> Decoder a) -> OptionDataValue -> Decoder a
withOptionDataValueMentionable f = \case
  OptionDataValueMentionable n v -> f n v
  _                              -> fail "Did not get OptionDataValueMentionable"
withOptionDataValueNumber :: (Text -> Either Text Number -> Decoder a) -> OptionDataValue -> Decoder a
withOptionDataValueNumber f = \case
  OptionDataValueNumber n v -> f n v
  _                         -> fail "Did not get OptionDataValueNumber"

(.!) :: Decoder a -> (a -> Decoder b) -> Decoder b
(.!) = (>>=)

(.!?) :: Decoder (Maybe a) -> (a -> Decoder b) -> Decoder (Maybe b)
d .!? f = d >>= maybe (pure Nothing) (fmap pure . f)

(.:) :: HasNames o i => o -> Text -> Decoder i
o .: n = named n pure o

(.:?) :: HasNames o i => o -> Text -> Decoder (Maybe i)
o .:? n = namedOptional n pure o

named :: HasNames o i => Text -> (i -> Decoder a) -> o -> Decoder a
named n f = maybe (fail $ "Missing named value: " <> T.unpack n) f . M.lookup n . mapNames

namedOptional :: HasNames o i => Text -> (i -> Decoder a) -> o -> Decoder (Maybe a)
namedOptional n f = maybe (pure Nothing) (fmap pure . f) . M.lookup n . mapNames

class HasName o where
  name :: o -> Text

class HasNames outer inner | outer -> inner where
  mapNames :: outer -> Map Text inner

instance HasName OptionDataSubcommandOrGroup where
  name (OptionDataSubcommandGroup n _ _)                                    = n
  name (OptionDataSubcommandOrGroupSubcommand (OptionDataSubcommand n _ _)) = n

instance HasName OptionDataSubcommand where
  name (OptionDataSubcommand n _ _) = n

instance HasName OptionDataValue where
  name = optionDataValueName

instance HasName o => HasNames [o] o where
  mapNames l = M.fromList $ (\o -> (name o, o)) <$> l

instance HasNames (Map Text a) a where
  mapNames = id

instance HasNames OptionDataSubcommand OptionDataValue where
  mapNames = mapNames . optionDataSubcommandOptions

instance HasNames OptionDataSubcommandOrGroup OptionDataSubcommand where
  mapNames = mapNames . optionDataSubcommandGroupOptions