module Data.Proof.TransactionsRefreshBody
  ( HasTransactionsRefreshBody
  , proveTransactionsRefreshBody
  ) where

import           Data.Api.Types
import           Data.Proof.Named
import           Data.Text (Text)

data HasTransactionsRefreshBody plaidEnv accessToken = Proof

proveTransactionsRefreshBody
  :: Named plaidEnv PlaidEnv
  -> Named accessToken AccessToken
  -> Either Text (HasTransactionsRefreshBody plaidEnv accessToken)
proveTransactionsRefreshBody _ atoken =
  case unWrapNamed atoken of
    (AccessToken "") -> Left "AccessToken cannot be empty"
    (AccessToken _ ) -> Right Proof
