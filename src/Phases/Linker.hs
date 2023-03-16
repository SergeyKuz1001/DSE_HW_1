module Phases.Linker (
    linker,
  ) where

import Phases.Linker.Commands

import Data.Handles
import qualified Data.AnalyzedPrimitive as AP
import qualified Data.LinkedPrimitive as LP
import Environment.MonadPwdReader (MonadPwdReader(..))
import Environment.FSPrimitive

import Data.List.NonEmpty (toList)

linker :: MonadPwdReader m => AP.Primitive -> m LP.Primitive
linker (AP.Special sp) = return $ LP.Special sp
linker (AP.Assignment name value) = return $ LP.Assignment name value
linker (AP.Empty) = return $ LP.Commands []
linker (AP.Commons cmns) =
  addInputHandles (toList cmns) >>= commonsToCommands >>= concatInternals >>= addOutputHandles >>= toPrimitive

addInputHandles :: MonadPwdReader m => [AP.Common] -> m [(Maybe AP.Common, InputHandle)]
addInputHandles = traverse go
  where
    go cmn@(AP.External _)                = return (Just cmn, FromParentHandle)
    go (AP.Internal (AP.Cat Nothing))     = return (Nothing, FromParentHandle)
    go (AP.Internal (AP.Cat (Just path))) = return (Nothing, FromFile path)
    go (AP.Internal (AP.Echo args))       = return (Nothing, FromString $ echo args)
    go cmn@(AP.Internal (AP.Wc Nothing))  = return (Just cmn, FromParentHandle)
    go (AP.Internal (AP.Wc (Just path)))  = return (Just . AP.Internal $ AP.Wc Nothing, FromFile path)
    go (AP.Internal AP.Pwd) = do
      pwd <- asFilePath <$> getVarPwd
      return (Nothing, FromString $ pwd ++ "\n")

commonsToCommands :: Monad m => [(Maybe AP.Common, InputHandle)] -> m [(LP.Command, InputHandle)]
commonsToCommands = return . go id
  where
    go acc [] =
      acc []
    go acc [(Nothing, inp)] =
      if null (acc []) || inp /= FromParentHandle
        then acc [(LP.Internal (LP.Func "cat" cat), inp)]
        else acc []
    go acc ((Nothing, inp) : (cmn, inp') : cmns) =
      go acc ((cmn, if inp' == FromParentHandle then inp else inp') : cmns)
    go acc ((Just (AP.External (AP.Arguments path args)), inp) : cmns) =
      go (acc . ((LP.External (LP.Arguments path args), inp) : )) cmns
    go acc ((Just (AP.Internal (AP.Wc Nothing)), inp) : cmns) =
      go (acc . ((LP.Internal (LP.Func "wc" wc), inp) : )) cmns

concatInternals :: Monad m => [(LP.Command, InputHandle)] -> m [(LP.Command, InputHandle)]
concatInternals = return . go id
  where
    go acc [] =
      acc []
    go acc ((LP.Internal (LP.Func name1 func1), inp1) : (LP.Internal (LP.Func name2 func2), FromParentHandle) : cmds) =
      go acc ((LP.Internal (LP.Func (name2 ++ "." ++ name1) (func2 . func1)), inp1) : cmds)
    go acc ((LP.Internal _, _) : (LP.Internal int, inp) : cmds) | inp /= FromParentHandle =
      go acc ((LP.Internal int, inp) : cmds)
    go acc ((LP.Internal _, _) : (LP.External ext, inp) : cmds) | inp /= FromParentHandle =
      go acc ((LP.External ext, inp) : cmds)
    go acc (cmd : cmds) =
      go (acc . (cmd : )) cmds

addOutputHandles :: Monad m => [(LP.Command, InputHandle)] -> m [LP.CommandWithHandles]
addOutputHandles [] = return []
addOutputHandles cmds = return $
  zipWith (\(cmd, hIn) (_, hIn') ->
      (cmd, hIn, if hIn' == FromParentHandle then ToNewPipe else ToNowhere))
    cmds (tail cmds) ++
  map (\(cmd, hIn) -> (cmd, hIn, ToStdout)) [last cmds]

toPrimitive :: Monad m => [LP.CommandWithHandles] -> m LP.Primitive
toPrimitive = return . LP.Commands
