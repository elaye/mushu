module Config
( load
, Config(..)
, Account(..)
) where

import Prelude hiding (readFile, lookup)
import Data.Text.IO (readFile)
import Data.Text
import Text.Toml
import Text.Toml.Types (Table, Node(..))

import Data.Monoid ((<>))

import Data.HashMap.Strict
import qualified Data.HashMap.Strict as H

-- TODO: convert ~ to $HOME for filepaths (see getHomeDirectory in System.Directory)

data Config = Config
	{ accounts :: HashMap Text Account
	} deriving Show

data Account = Account
	{ name :: Text
	, user :: Text
	, password :: Text
	, imap :: Text
  , maildir :: Text
	} deriving Show

loadToml :: FilePath -> IO (Either Text Table)
loadToml path = do
	file <- readFile path 
	case parseTomlDoc path file of
		Left pe -> return $ Left "Parsing error : the configuration file must be a valid TOML file"
		Right tomlConfig -> return $ Right tomlConfig

load :: FilePath -> IO (Either Text Config)
load path = do
	toml <- loadToml path
	return $ toml >>= tomlToConfig

tomlToConfig :: Table -> Either Text Config
tomlToConfig conf = Config <$> accounts
	where
		accounts = case (lookup "accounts" conf) of
								Nothing -> Left "missing accounts"
								Just accs -> case accs of
															VTable table -> checkAccounts $ H.map tomlToAccount table
															_ -> Left "wrong type for accounts"

-- Check that all accounts are valid. If not, report error
checkAccounts :: HashMap Text (Either Text Account) -> Either Text (HashMap Text Account)
checkAccounts accs = foldrWithKey f (Right H.empty) accs
	where
		f key account hash = case hash of
											Left err -> Left err
											Right h -> case account of
																	Left accErr -> Left $ "Error with account " <> key <> ": " <> accErr
																	Right acc -> Right $ insert key acc h 

tomlToAccount :: Node -> Either Text Account
tomlToAccount node = case node of
	VTable table -> Account <$> name <*> user <*> password <*> imap <*> maildir
										where
											name = getStrField "name" table
											user = getStrField "user" table
											password = getStrField "password" table
											imap = getStrField "imap" table
											maildir = getStrField "maildir" table
	_ -> Left "account must be a hashMap"

getStrField :: Text -> Table -> Either Text Text
getStrField name table =
	case (lookup name table) of
		Nothing -> Left $ "No field " <> name
		Just node -> case node of
									VString value -> Right value
									_ -> Left $ "Field " <> name <> " must be a string"


