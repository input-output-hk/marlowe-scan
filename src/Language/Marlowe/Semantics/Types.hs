{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Marlowe.Semantics.Types
  ( Contract(..)
  , Money
  , Party(..)
  , State(..)
  , Token(..)
  , emptyState
  , getAction
  , getInputContent
  , inBounds
  , ivFrom, ivTo
  ) where

import           Data.Aeson.Types (FromJSON(parseJSON), KeyValue((.=)), Parser, ToJSON(toJSON, toJSONList), object, withArray, withObject, (.:))
import           GHC.Generics (Generic)
import           Language.Marlowe.Pretty (Pretty, prettyFragment)
import           Data.ByteString (ByteString)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Text.PrettyPrint.Leijen (text)
import qualified Data.Aeson as JSON
import qualified Data.Foldable as F
import           Control.Applicative ((<|>))
import           Data.Scientific (Scientific, floatingOrInteger)
import qualified Data.Text.Encoding as T
import qualified Data.Aeson.Types as JSON
import           Data.Text (Text)

newtype POSIXTime = POSIXTime { getPOSIXTime :: Integer }
  deriving stock (Eq,Ord,Generic)
  deriving newtype (Pretty)

instance Show POSIXTime where
  showsPrec p (POSIXTime n) = showsPrec p n
instance Read POSIXTime where
  readsPrec p x = [(POSIXTime v, s) | (v, s) <- readsPrec p x]

instance Num POSIXTime where
    POSIXTime l + POSIXTime r = POSIXTime (l + r)
    POSIXTime l * POSIXTime r = POSIXTime (l * r)
    abs (POSIXTime l) = POSIXTime (abs l)
    signum (POSIXTime l) = POSIXTime (signum l)
    fromInteger = POSIXTime
    negate (POSIXTime l) = POSIXTime (negate l)

data Party = Address Text
           | Role Text
  deriving (Eq,Ord,Show,Read,Generic,Pretty)

instance FromJSON Party where
  parseJSON = withObject "Party" (\v -> (Address <$> (v .: "address"))
    <|> (Role <$> (v .: "role_token")))
instance ToJSON Party where
    toJSON (Address address) = object
        [ "address" .= JSON.String address ]
    toJSON (Role name) = object
        [ "role_token" .= JSON.String name ]

type ChoiceName = Text
type Timeout = POSIXTime
type Money = Integer
type ChosenNum = Integer

data ChoiceId = ChoiceId ChoiceName Party
  deriving (Eq,Ord,Show,Read,Generic,Pretty)

instance FromJSON ChoiceId where
  parseJSON = withObject "ChoiceId" (\v ->
       ChoiceId <$> (v .: "choice_name")
                <*> (v .: "choice_owner"))

instance ToJSON ChoiceId where
  toJSON (ChoiceId name party) = object [ "choice_name" .= JSON.String name
                                        , "choice_owner" .= party
                                        ]

newtype ValueId = ValueId Text
  deriving stock (Eq,Ord,Generic)

instance FromJSON ValueId where
    parseJSON = JSON.withText "ValueId" $ \t ->
        pure (ValueId t)
instance ToJSON ValueId where
    toJSON (ValueId x) = JSON.String x

data Token = Token Text Text
  deriving (Eq,Ord,Show,Read,Generic,Pretty)

instance FromJSON Token where
  parseJSON = withObject "Token" (\v ->
       Token <$> (v .: "currency_symbol")
             <*> (v .: "token_name"))

instance ToJSON Token where
  toJSON (Token currSym tokName) = object
      [ "currency_symbol" .= JSON.String currSym
      , "token_name" .= JSON.String tokName
      ]

instance Pretty ValueId where
  prettyFragment = text . show

instance Show ValueId where
  showsPrec p (ValueId txt) = showsPrec p txt
instance Read ValueId where
  readsPrec p x = [(ValueId v, s) | (v, s) <- readsPrec p x]

data Value = AvailableMoney Party Token
           | Constant Integer
           | NegValue Value
           | AddValue Value Value
           | SubValue Value Value
           | MulValue Value Value
           | DivValue Value Value
           | ChoiceValue ChoiceId
           | TimeIntervalStart
           | TimeIntervalEnd
           | UseValue ValueId
           | Cond Observation Value Value
  deriving (Eq,Ord,Show,Read,Generic,Pretty)

data Observation = AndObs Observation Observation
                  | OrObs Observation Observation
                  | NotObs Observation
                  | ChoseSomething ChoiceId
                  | ValueGE Value Value
                  | ValueGT Value Value
                  | ValueLT Value Value
                  | ValueLE Value Value
                  | ValueEQ Value Value
                  | TrueObs
                  | FalseObs
  deriving (Eq,Ord,Show,Read,Generic,Pretty)

data TimeInterval = TimeInterval POSIXTime POSIXTime
  deriving (Eq,Ord,Show,Read,Generic,Pretty)

ivFrom, ivTo :: TimeInterval -> POSIXTime
ivFrom (TimeInterval from _) = from
ivTo   (TimeInterval _ to)   = to

data Bound = Bound Integer Integer
  deriving (Eq,Ord,Show,Read,Generic,Pretty)

instance FromJSON Bound where
  parseJSON = withObject "Bound" (\v ->
       Bound <$> (getInteger "lower bound"=<< (v .: "from"))
             <*> (getInteger "higher bound" =<< (v .: "to"))
                                 )
instance ToJSON Bound where
  toJSON (Bound from to) = object
      [ "from" .= from
      , "to" .= to
      ]

inBounds :: ChosenNum -> [Bound] -> Bool
inBounds num = any (\(Bound l u) -> num >= l && num <= u)

data Action = Deposit Party Party Token Value
            | Choice ChoiceId [Bound]
            | Notify Observation
  deriving (Eq,Ord,Show,Read,Generic,Pretty)

data Payee = Account Party
           | Party Party
  deriving (Eq,Ord,Show,Read,Generic,Pretty)

data Case = Case Action Contract
          | MerkleizedCase Action ByteString
  deriving (Eq,Ord,Show,Read,Generic,Pretty)

getAction :: Case -> Action
getAction (Case action _) = action
getAction (MerkleizedCase action _) = action

data Contract = Close
              | Pay Party Payee Token Value Contract
              | If Observation Contract Contract
              | When [Case] Timeout Contract
              | Let ValueId Value Contract
              | Assert Observation Contract
  deriving (Eq,Ord,Show,Read,Generic,Pretty)

data State = State { accounts    :: Map (Party, Token) Money
                   , choices     :: Map ChoiceId ChosenNum
                   , boundValues :: Map ValueId Integer
                   , minTime     :: POSIXTime }
  deriving (Eq,Ord,Show,Read)

emptyState :: POSIXTime -> State
emptyState sn = State { accounts = Map.empty
                      , choices = Map.empty
                      , boundValues = Map.empty
                      , minTime = sn }


toJSONAssocMap :: ToJSON k => ToJSON v => Map k v -> JSON.Value
toJSONAssocMap = toJSON . Map.toList

fromJSONAssocMap :: Ord k => FromJSON k => FromJSON v => JSON.Value -> JSON.Parser (Map k v)
fromJSONAssocMap v = Map.fromList <$> parseJSON v

instance FromJSON State where
  parseJSON = withObject "State" (\v ->
         State <$> (v .: "accounts" >>= fromJSONAssocMap)
               <*> (v .: "choices" >>= fromJSONAssocMap)
               <*> (v .: "boundValues" >>= fromJSONAssocMap)
               <*> (POSIXTime <$> (withInteger "minTime" =<< (v .: "minTime")))
                                 )

instance ToJSON State where
  toJSON State { accounts = a
               , choices = c
               , boundValues = bv
               , minTime = POSIXTime ms } = object
        [ "accounts" .= toJSONAssocMap a
        , "choices" .= toJSONAssocMap c
        , "boundValues" .= toJSONAssocMap bv
        , "minTime" .= ms ]

newtype Environment = Environment { timeInterval :: TimeInterval }
  deriving (Eq,Ord,Show,Read)

data InputContent = IDeposit Party Party Token Money
                  | IChoice ChoiceId ChosenNum
                  | INotify
  deriving (Eq,Ord,Show,Read)

data Input = NormalInput InputContent
           | MerkleizedInput InputContent ByteString
  deriving (Eq,Ord,Show,Read)

getInputContent :: Input -> InputContent
getInputContent (NormalInput inputContent) = inputContent
getInputContent (MerkleizedInput inputContent _) = inputContent

-- Processing of time interval
data IntervalError = InvalidInterval TimeInterval
                    | IntervalInPastError POSIXTime TimeInterval
  deriving (Eq,Ord,Show,Read)

data IntervalResult = IntervalTrimmed Environment State
                    | IntervalError IntervalError
  deriving (Eq,Ord,Show,Read)

instance FromJSON Value where
  parseJSON (JSON.Object v) =
        (AvailableMoney <$> (v .: "in_account")
                        <*> (v .: "amount_of_token"))
    <|> (NegValue <$> (v .: "negate"))
    <|> (AddValue <$> (v .: "add")
                  <*> (v .: "and"))
    <|> (SubValue <$> (v .: "value")
                  <*> (v .: "minus"))
    <|> (MulValue <$> (v .: "multiply")
                  <*> (v .: "times"))
    <|> (DivValue <$> (v .: "divide") <*> (v .: "by"))
    <|> (ChoiceValue <$> (v .: "value_of_choice"))
    <|> (UseValue <$> (v .: "use_value"))
    <|> (Cond <$> (v .: "if")
              <*> (v .: "then")
              <*> (v .: "else"))
  parseJSON (JSON.String "time_interval_start") = return TimeIntervalStart
  parseJSON (JSON.String "time_interval_end") = return TimeIntervalEnd
  parseJSON (JSON.Number n) = Constant <$> getInteger "constant value" n
  parseJSON _ = fail "Value must be either an object or an integer"
instance ToJSON Value where
  toJSON (AvailableMoney accountId token) = object
      [ "amount_of_token" .= token
      , "in_account" .= accountId
      ]
  toJSON (Constant x) = toJSON x
  toJSON (NegValue x) = object
      [ "negate" .= x ]
  toJSON (AddValue lhs rhs) = object
      [ "add" .= lhs
      , "and" .= rhs
      ]
  toJSON (SubValue lhs rhs) = object
      [ "value" .= lhs
      , "minus" .= rhs
      ]
  toJSON (MulValue lhs rhs) = object
      [ "multiply" .= lhs
      , "times" .= rhs
      ]
  toJSON (DivValue lhs rhs) = object
      [ "divide" .= lhs
      , "by" .= rhs
      ]
  toJSON (ChoiceValue choiceId) = object
      [ "value_of_choice" .= choiceId ]
  toJSON TimeIntervalStart = JSON.String "time_interval_start"
  toJSON TimeIntervalEnd = JSON.String "time_interval_end"
  toJSON (UseValue valueId) = object
      [ "use_value" .= valueId ]
  toJSON (Cond obs tv ev) = object
      [ "if" .= obs
      , "then" .= tv
      , "else" .= ev
      ]


instance FromJSON Observation where
  parseJSON (JSON.Bool True) = return TrueObs
  parseJSON (JSON.Bool False) = return FalseObs
  parseJSON (JSON.Object v) =
        (AndObs <$> (v .: "both")
                <*> (v .: "and"))
    <|> (OrObs <$> (v .: "either")
               <*> (v .: "or"))
    <|> (NotObs <$> (v .: "not"))
    <|> (ChoseSomething <$> (v .: "chose_something_for"))
    <|> (ValueGE <$> (v .: "value")
                 <*> (v .: "ge_than"))
    <|> (ValueGT <$> (v .: "value")
                 <*> (v .: "gt"))
    <|> (ValueLT <$> (v .: "value")
                 <*> (v .: "lt"))
    <|> (ValueLE <$> (v .: "value")
                 <*> (v .: "le_than"))
    <|> (ValueEQ <$> (v .: "value")
                 <*> (v .: "equal_to"))
  parseJSON _ = fail "Observation must be either an object or a boolean"

instance ToJSON Observation where
  toJSON (AndObs lhs rhs) = object
      [ "both" .= lhs
      , "and" .= rhs
      ]
  toJSON (OrObs lhs rhs) = object
      [ "either" .= lhs
      , "or" .= rhs
      ]
  toJSON (NotObs v) = object
      [ "not" .= v ]
  toJSON (ChoseSomething choiceId) = object
      [ "chose_something_for" .= choiceId ]
  toJSON (ValueGE lhs rhs) = object
      [ "value" .= lhs
      , "ge_than" .= rhs
      ]
  toJSON (ValueGT lhs rhs) = object
      [ "value" .= lhs
      , "gt" .= rhs
      ]
  toJSON (ValueLT lhs rhs) = object
      [ "value" .= lhs
      , "lt" .= rhs
      ]
  toJSON (ValueLE lhs rhs) = object
      [ "value" .= lhs
      , "le_than" .= rhs
      ]
  toJSON (ValueEQ lhs rhs) = object
      [ "value" .= lhs
      , "equal_to" .= rhs
      ]
  toJSON TrueObs = toJSON True
  toJSON FalseObs = toJSON False

instance FromJSON Action where
  parseJSON = withObject "Action" (\v ->
       (Deposit <$> (v .: "into_account")
                <*> (v .: "party")
                <*> (v .: "of_token")
                <*> (v .: "deposits"))
   <|> (Choice <$> (v .: "for_choice")
               <*> ((v .: "choose_between") >>=
                    withArray "Bound list" (mapM parseJSON . F.toList)))
   <|> (Notify <$> (v .: "notify_if"))
                                  )
instance ToJSON Action where
  toJSON (Deposit accountId party token val) = object
      [ "into_account" .= accountId
      , "party" .= party
      , "of_token" .= token
      , "deposits" .= val
      ]
  toJSON (Choice choiceId bounds) = object
      [ "for_choice" .= choiceId
      , "choose_between" .= toJSONList (map toJSON bounds)
      ]
  toJSON (Notify obs) = object
      [ "notify_if" .= obs ]

instance FromJSON Case where
  parseJSON = withObject "Case" (\v ->
      (Case <$> (v .: "case") <*> (v .: "then"))
      <|> (MerkleizedCase
        <$> v .: "case"
        <*> (T.encodeUtf8 <$> v .: "merkleized_then")))

instance ToJSON Case where
  toJSON (Case act cont) = object
      [ "case" .= act
      , "then" .= cont
      ]
  toJSON (MerkleizedCase act bs) = object
      [ "case" .= act
      , "merkleized_then" .= T.decodeUtf8 bs
      ]


instance FromJSON Payee where
  parseJSON = withObject "Payee" (\v ->
                (Account <$> (v .: "account"))
            <|> (Party <$> (v .: "party")))

instance ToJSON Payee where
  toJSON (Account acc) = object ["account" .= acc]
  toJSON (Party party) = object ["party" .= party]

instance FromJSON Contract where
  parseJSON (JSON.String "close") = return Close
  parseJSON (JSON.Object v) =
        (Pay <$> (v .: "from_account")
             <*> (v .: "to")
             <*> (v .: "token")
             <*> (v .: "pay")
             <*> (v .: "then"))
    <|> (If <$> (v .: "if")
            <*> (v .: "then")
            <*> (v .: "else"))
    <|> (When <$> ((v .: "when") >>=
                   withArray "Case list" (mapM parseJSON . F.toList))
              <*> (POSIXTime <$> (withInteger "when timeout" =<< (v .: "timeout")))
              <*> (v .: "timeout_continuation"))
    <|> (Let <$> (v .: "let")
             <*> (v .: "be")
             <*> (v .: "then"))
    <|> (Assert <$> (v .: "assert")
                <*> (v .: "then"))
  parseJSON _ = fail "Contract must be either an object or a the string \"close\""

instance ToJSON Contract where
  toJSON Close = JSON.String "close"
  toJSON (Pay accountId payee token value cont) = object
      [ "from_account" .= accountId
      , "to" .= payee
      , "token" .= token
      , "pay" .= value
      , "then" .= cont
      ]
  toJSON (If obs cont1 cont2) = object
      [ "if" .= obs
      , "then" .= cont1
      , "else" .= cont2
      ]
  toJSON (When caseList timeout cont) = object
      [ "when" .= toJSONList (map toJSON caseList)
      , "timeout" .= getPOSIXTime timeout
      , "timeout_continuation" .= cont
      ]
  toJSON (Let valId value cont) = object
      [ "let" .= valId
      , "be" .= value
      , "then" .= cont
      ]
  toJSON (Assert obs cont) = object
      [ "assert" .= obs
      , "then" .= cont
      ]
getInteger :: String -> Scientific -> Parser Integer
getInteger ctx x = case (floatingOrInteger x :: Either Double Integer) of
                 Right a -> return a
                 Left _  -> fail $ "parsing " ++ ctx ++ " failed, expected integer, but encountered floating point"

withInteger :: String -> JSON.Value -> Parser Integer
withInteger ctx = JSON.withScientific ctx $ getInteger ctx
