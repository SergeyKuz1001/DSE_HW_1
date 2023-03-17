{- |
Модуль с внутренней нечистой командой @pwd@.
-}
module Phases.Executor.Impure.Pwd (
    pwd,
  ) where

import Data.FSObjects (asFilePath)
import Monads.PwdReader (MonadPwdReader(..))

import Data.Text.Lazy (Text, pack)

-- | Команда @pwd@ принимает текст, игнорирует его и возвращает путь до текущей
-- директории в виде текста.
pwd :: MonadPwdReader m => Text -> m Text
pwd _ = pack . (++ "\n") . asFilePath <$> getVarPwd
