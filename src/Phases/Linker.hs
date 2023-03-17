{- |
Модуль для связывания команд, написанных через pipes.
-}
module Phases.Linker (
    linker,
  ) where

import Phases.Linker.Commands

import Data.FSObjects
import Data.Handles
import qualified Data.AnalyzedPrimitive as AP
import qualified Data.LinkedPrimitive as LP
import Monads.PwdReader (MonadPwdReader(..))
import Monads.SelfReferenced (MonadSelfReferenced(..))

import Data.List.NonEmpty (toList)

-- Предупреждение: этот модуль полагается на многие инварианты, при изменении
-- любого из них крайне рекомендуется полностью переписать данный модуль.

-- | Функция для связывания команд, написанных через pipes. Работает только с
-- последовательностями команд, остальные типы пользовательских запросов
-- возвращаются без изменений.
--
-- Под "связыванием" команд здесь понимается определение типов потоков ввода и
-- вывода для каждой команды, а также некоторая оптимизация плана исполнения
-- пользовательского запроса.
linker :: (MonadPwdReader m, MonadSelfReferenced m) => AP.Primitive -> m LP.Primitive
linker (AP.Special sp) = return $ LP.Special sp
linker (AP.Assignment name value) = return $ LP.Assignment name value
linker AP.Empty = return $ LP.Commons []
linker (AP.Commons cmns) =
  addInputHandles (toList cmns) >>=
  commonsTransformation >>=
  concatInternals >>=
  addOutputHandles >>=
  addExternalAtBegin >>=
  toPrimitive

-- | Функция определения типа потока ввода для каждой команды, а также маленькая
-- оптимизация, звязанная со следующими соображениями:
--
--   * @cat@ равносилен простому переносу текста из одного потока в другой,
--     поэтому он заменяется на @Nothing@ с соответствующим типом потока ввода;
--   * @echo@ равносилен @cat@ (мы сначала выполняем @echo@ как чистую функцию
--     от своих аргументов, а потом заменяем на @cat@, читающий из строки);
--   * @wc@ с аргументом равносилен @wc@ без аргументов, но с другим типом
--     потока ввода;
--   * @pwd@ равносилен @echo@ от абсолютного пути до текущей директории.
--
-- Таким образом из внутренних команд остаётся только @wc@ без аргументов.
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
      return (Nothing, FromString $ echo [pwd])

-- | Функция преобразования команд из одного типа в другой. Здесь также
-- проводится оптимизация, связанная с удалением @Nothing@-команд (которые
-- просто переносят информацию из одного потока в другой).
--
-- Заметим, что на входе в качестве внутренней команды полагается возможным
-- только команда @wc@ без аргументов. На выходе же внутренней командой также
-- может быть и @cat@ без аргументов. Это связано с преобразованием
-- @Nothing@-команды обратно в @cat@ в случае невозможного её удаления из плана
-- выполнения запроса без изменения поведения.
commonsTransformation :: Monad m => [(Maybe AP.Common, InputHandle)] -> m [(LP.Common, InputHandle)]
commonsTransformation = return . go id
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
    go _ _ = undefined -- по неявному инварианту иного быть не может

-- | Функция склеивания подряд идущих внутренних команд в одну, так как каждая
-- внутренняя команда представима здесь как чистая функция, преобразующая текст.
concatInternals :: Monad m => [(LP.Common, InputHandle)] -> m [(LP.Common, InputHandle)]
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

-- | Функция добавления типа потока вывода для каждой (из оставшихся) команды.
addOutputHandles :: Monad m => [(LP.Common, InputHandle)] -> m [LP.CommonWithHandles]
addOutputHandles [] = return []
addOutputHandles cmds = return $
  zipWith (\(cmd, hIn) (_, hIn') ->
      (cmd, hIn, if hIn' == FromParentHandle then ToNewPipe else ToNowhere))
    cmds (tail cmds) ++
  [(\(cmd, hIn) -> (cmd, hIn, ToStdout)) $ last cmds]

-- | Функция добавления внешней команды в начало плана выполнения запроса в
-- случае необходимости.
--
-- Эта функция была добавлена для исправления ситуации, когда при заканчивании
-- потока ввода для @cat@ также заканчивался поток ввода команд пользователя.
addExternalAtBegin :: MonadSelfReferenced m => [LP.CommonWithHandles] -> m [LP.CommonWithHandles]
addExternalAtBegin cwhs@((LP.Internal (LP.Func name _), FromParentHandle, outp):_) = do
  selfPath <- getSelfPath
  let self = LP.External (LP.Arguments selfPath ["--work-as-cat"])
    -- полагаться на то, что реальная программа, построенная на этой
    -- абстрактной, будет поддерживать такой специфичный параметр без явного
    -- указания этого где-либо — плохая идея, лучше как-то исправить в будущем
  return $
    if name == "cat" && outp == ToStdout
      then (self, FromParentHandle, ToStdout)  : tail cwhs
      else (self, FromParentHandle, ToNewPipe) : cwhs
addExternalAtBegin cwhs = return cwhs

-- | Функция преобразования списка команд в примитив.
--
-- Заметим, что тип примитива для пользовательского запроса после линковки не
-- гарантирует того инварианта, что в плане нет двух последовательных внутренних
-- команд, однако этот инвариант в этой реализации выполняется. Так сделано
-- из-за того, что в процессе исполнения плана этот инвариант не имеет значения,
-- при этом исполнение плана, выраженного в существующем типе примитива, проще.
toPrimitive :: Monad m => [LP.CommonWithHandles] -> m LP.Primitive
toPrimitive = return . LP.Commons
