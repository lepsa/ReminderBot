{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}

module Bot.Commands.Decoder where
import           Control.Applicative
import           Control.Monad
import           Data.Foldable         (find)
import           Data.Kind
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Discord.Interactions
import           Discord.Internal.Rest

-- This is the same as the `aeson` "parser" but with a less
-- confusing name and no path information.

type EpsilonFailure f r = String -> f r
type Failure        f r = String -> f r
type Success      a f r = a      -> f r

newtype Decoder a = Decoder
  { runDecoder
      :: forall (f :: (Type -> Type)) (r :: Type).
         -- This parse branch can implicitly backtrack and try
         -- another parser. Basically it hasn't "consumed"
         -- any of the input
         EpsilonFailure f r
         -- Committed failure. This cannot implicitly backtrack.
         -- This is useful because it will limit the returned error
         -- to the current parse branch rather than wandering off
         -- into another branch.
      -> Failure f r
         -- Committed success. Causes the parser to switch from
         -- epsilon failures into commited failures. Basically
         -- recognises that we have found a partially successful
         -- parse path.
      -> Success a f r
      -> f r
  }

decode :: MonadFail m => Decoder a -> m a
decode d = runDecoder d fail fail pure

decodeEither :: Decoder a -> Either String a
decodeEither d = runDecoder d Left Left pure

instance Functor Decoder where
  fmap x (Decoder y) = Decoder $ \ef f s -> y ef f (s . x)

instance Monad Decoder where
  (Decoder fa) >>= f = Decoder $ \efailure failure success ->
    fa efailure failure $ \a ->
      let (Decoder g) = f a
      in g failure failure success

instance Applicative Decoder where
  pure a = Decoder $ \_ef _f s -> s a
  (Decoder fab) <*> (Decoder fa) = Decoder $ \efailure failure success ->
    fab efailure failure $ \ab -> fa failure failure (success . ab)

instance Alternative Decoder where
  empty = Decoder $ \ef _f _s -> ef "Empty"
  (Decoder p) <|> (Decoder q) = Decoder $ \ef f s ->
    p (\_ -> q ef f s) f s

try :: Decoder a -> Decoder a
try (Decoder d) = Decoder $ \ef _f -> d ef ef

instance MonadFail Decoder where
  fail msg = Decoder $ \ef _f _s -> ef msg

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

(.:) :: HasNames o f i => o -> Text -> Decoder i
o .: n = named n pure o

(.:?) :: HasNames o f i => o -> Text -> Decoder (Maybe i)
o .:? n = namedOptional n pure o

named :: HasNames o f i => Text -> (i -> Decoder a) -> o -> Decoder a
named n f = maybe (fail $ "Missing named value: " <> T.unpack n) f . find (\v -> name v == n) . mapNames

namedOptional :: HasNames o f i => Text -> (i -> Decoder a) -> o -> Decoder (Maybe a)
namedOptional n f = maybe (pure Nothing) (fmap pure . f) . find (\v -> name v == n) . mapNames

class HasName o where
  name :: o -> Text

class (HasName inner, Foldable f) => HasNames outer f inner | outer -> f inner where
  mapNames :: outer -> f inner

instance HasName OptionDataSubcommandOrGroup where
  name (OptionDataSubcommandGroup n _ _)                                    = n
  name (OptionDataSubcommandOrGroupSubcommand (OptionDataSubcommand n _ _)) = n

instance HasName OptionDataSubcommand where
  name (OptionDataSubcommand n _ _) = n

instance HasName OptionDataValue where
  name = optionDataValueName

instance HasName o => HasNames [o] [] o where
  mapNames = id

instance HasNames OptionDataSubcommand [] OptionDataValue where
  mapNames = optionDataSubcommandOptions

instance HasNames OptionDataSubcommandOrGroup [] OptionDataSubcommand where
  mapNames = optionDataSubcommandGroupOptions
