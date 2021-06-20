module Data.Api.Transactions
  ( plaidGetTransactions
  , plaidRefreshTransactions
  , plaidCategoriesGet
  ) where

import           Data.Common

plaidGetTransactions
  :: ( MonadReader PlaidEnv m
     , MonadThrow m
     , PlaidHttp m
     )
  => PlaidBody PlaidTransactionsGet
  -> m ByteString
plaidGetTransactions body = do
  env <- ask
  let url = envUrl (env ^. plaidEnvEnvironment)
  executePost (url <> "/transactions/get") body

plaidRefreshTransactions
  :: ( MonadReader PlaidEnv m
     , MonadThrow m
     , PlaidHttp m
     )
  => PlaidBody PlaidTransactionsRefresh
  -> m ByteString
plaidRefreshTransactions body = do
  env <- ask
  let url = envUrl (env ^. plaidEnvEnvironment)
  executePost (url <> "/transactions/refresh") body

plaidCategoriesGet
  :: ( MonadReader PlaidEnv m
     , MonadThrow m
     , PlaidHttp m
     )
  => m ByteString
plaidCategoriesGet = do
  env <- ask
  let url = envUrl (env ^. plaidEnvEnvironment)
  executePost (url <> "/categories/get") ("" :: String)
