module Agda.Interaction.Highlighting.LaTeX.Prettify where

-- import Agda.Interaction.Highlighting.LaTeX.Base
--   ( LaTeXOptions(..)
--   , LogLaTeXT
--   , runLogLaTeXTWith
--   , logMsgToText
--   , generateLaTeXIO
--   , prepareCommonAssets
--   )

import Debug.Trace

import Control.Monad.Trans (MonadIO)

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Text (Text)
import qualified Data.Text as T

import System.FilePath ( (</>) )

import Agda.Compiler.Backend (Backend(..), Backend'(..), Definition, Recompile(..))
import Agda.Compiler.Common (curIF, IsMain(IsMain, NotMain))

import Agda.Interaction.Options
  ( ArgDescr(NoArg, ReqArg)
  , CommandLineOptions ( optGHCiInteraction, optPragmaOptions )
  , PragmaOptions ( optCountClusters )
  , Flag
  , OptDescr(..)
  )
import Agda.Interaction.Highlighting.Precise

import Agda.Syntax.Abstract.Name (ModuleName, toTopLevelModuleName)
import Agda.Syntax.Concrete.Name (TopLevelModuleName, projectRoot)

import Agda.TypeChecking.Monad
  ( HasOptions(commandLineOptions)
  , MonadDebug
  , stModuleToSource
  , useTC
  , ReadTCState
  , reportS
  )

import Agda.Utils.FileName (filePath, mkAbsolute)

import Control.DeepSeq

class TokenLike a where
    getTokenText :: a -> Text
    setTokenText :: a -> Text -> a
    makePlainToken :: Maybe Aspect -> Text -> a

data Prettifier = Prettifier
  {
    prettify :: forall a. (Show a, TokenLike a) => [a] -> [a]
  , processChars :: Char -> String
  }

instance Eq Prettifier where
  a == b = True

instance NFData Prettifier where
  rnf _ = ()


