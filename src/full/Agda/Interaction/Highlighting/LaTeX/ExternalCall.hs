
{-# LANGUAGE NondecreasingIndentation #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Agda.Interaction.Highlighting.LaTeX.ExternalCall where

-- import Agda.Interaction.Highlighting.LaTeX.Base
--   ( LaTeXOptions(..)
--   , LogLaTeXT
--   , runLogLaTeXTWith
--   , logMsgToText
--   , generateLaTeXIO
--   , prepareCommonAssets
--   )

---------------------------------------------------------------
-- InteractionTop imports


import Prelude hiding (null)

import System.IO

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import qualified Control.Exception as E
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State hiding (state)
import Control.Monad.STM

import qualified Data.Char as Char
import Data.Function
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe

import System.Directory
import System.FilePath

import Agda.TypeChecking.Monad as TCM
  hiding (initState, setCommandLineOptions)
import qualified Agda.TypeChecking.Monad as TCM
import qualified Agda.TypeChecking.Pretty as TCP
import Agda.TypeChecking.Rules.Term (checkExpr, isType_)
import Agda.TypeChecking.Errors
import Agda.TypeChecking.Warnings (runPM)

import Agda.Syntax.Fixity
import Agda.Syntax.Position
import Agda.Syntax.Parser
import Agda.Syntax.Common
import Agda.Syntax.Concrete as C
import Agda.Syntax.Concrete.Glyph
import Agda.Syntax.Abstract as A
import Agda.Syntax.Abstract.Pretty
import Agda.Syntax.Info (mkDefInfo)
import Agda.Syntax.Translation.ConcreteToAbstract
import Agda.Syntax.Translation.AbstractToConcrete hiding (withScope)
import Agda.Syntax.Scope.Base

import Agda.Interaction.Base
import Agda.Interaction.FindFile
import Agda.Interaction.Options
import Agda.Interaction.Options.Lenses as Lenses
import Agda.Interaction.MakeCase
import Agda.Interaction.SearchAbout
import Agda.Interaction.Response hiding (Function, ExtendedLambda)
import qualified Agda.Interaction.Response as R
import qualified Agda.Interaction.BasicOps as B
import Agda.Interaction.Highlighting.Precise hiding (Error, Postulate, singleton)
import Agda.Interaction.Imports  ( Mode(..) )
import qualified Agda.Interaction.Imports as Imp
import Agda.Interaction.Highlighting.Generate

import Agda.Compiler.Backend

import Agda.Auto.Auto as Auto

import Agda.Utils.Either
import Agda.Utils.FileName
import Agda.Utils.Function
import Agda.Utils.Hash
import Agda.Utils.Lens
import qualified Agda.Utils.Maybe.Strict as Strict
import Agda.Utils.Monad
import Agda.Utils.Null
import Agda.Utils.Pretty
import Agda.Utils.Singleton
import Agda.Utils.String
import Agda.Utils.Time
import Agda.Utils.Tuple

import Agda.Utils.Impossible


---------------------------------------------------------------



import Debug.Trace

import Control.Monad.Trans (MonadIO)

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Text (Text)
import qualified Data.Text as T

import System.FilePath ( (</>) )

import Agda.Compiler.Backend (Backend(..), Backend'(..), Definition, Recompile(..), compilerMain)
import Agda.Compiler.Common (curIF, IsMain(IsMain, NotMain))

import Agda.Interaction.Options
  ( ArgDescr(NoArg, ReqArg)
  , CommandLineOptions ( optGHCiInteraction, optPragmaOptions )
  , PragmaOptions ( optCountClusters )
  , Flag
  , OptDescr(..)
  )

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


import Agda.Interaction.ExitCode (AgdaError(..), exitSuccess, exitAgdaWith)

import qualified Agda.Interaction.Imports as Imp
import qualified Agda.Interaction.Base as AIB
import qualified Agda.Interaction.BasicOps as B

import Agda.Interaction.Highlighting.LaTeX.Backend
import Agda.Interaction.Highlighting.LaTeX.Base
import Agda.Interaction.Highlighting.LaTeX.Prettify
import Agda.Interaction.InteractionTop
import Agda.Compiler.Backend

import Agda.Main

sayHello :: IO ()
sayHello = putStrLn "I am in Agda!"


externalInteractionOutputCallback :: InteractionOutputCallback
externalInteractionOutputCallback = \case
  Resp_HighlightingInfo {}  -> liftIO $ do
                                 putStr "Response: ClearHighlighting"
                                 hFlush stdout
  Resp_Status s            -> liftIO $ do
                                 putStr $ "ILLEGAL Response: status " -- ++ show s
                                 hFlush stdout
  Resp_JumpToError {}       -> __IMPOSSIBLE__
  Resp_InteractionPoints {} -> __IMPOSSIBLE__
  Resp_GiveAction {}        -> __IMPOSSIBLE__
  Resp_MakeCase {}          -> __IMPOSSIBLE__
  Resp_SolveAll {}          -> __IMPOSSIBLE__
  Resp_DisplayInfo {}       -> liftIO $ do
                                 putStr "Response: DisplayInfo"
                                 hFlush stdout
  Resp_RunningInfo _ s      -> liftIO $ do
                                 putStr s
                                 hFlush stdout
  Resp_ClearRunningInfo {}  -> liftIO $ do
                                 putStr "Response: CleanRunningInfo"
                                 hFlush stdout
  Resp_ClearHighlighting {} -> liftIO $ do
                                 putStr "Response: ClearHighlighting"
                                 hFlush stdout
  Resp_DoneAborting {}      -> liftIO $ do
                                 putStr "Response: DoneAborting"
                                 hFlush stdout
  Resp_DoneExiting {}       -> liftIO $ do
                                 putStr "Response: DoneExiting"
                                 hFlush stdout



generatePrettyLatexIO :: Prettifier -> FilePath -> FilePath -> IO ()
generatePrettyLatexIO p file_Abs dir_Abs = do
    cmdQueue <- initialiseCommandQueue (return Done)
    let res = runStateT (generatePrettyLatex p file_Abs dir_Abs) (initCommandState $ cmdQueue)
    externalRunTCMPrettyErrors ((const ()) <$> res)

generatePrettyLatex :: Prettifier -> FilePath -> FilePath -> CommandM ()
generatePrettyLatex p file_Abs dir_Abs =
    let res = cmd_load' file_Abs [] True (Imp.TypeCheck)
              $ \checkResult -> do 
                              mw <- lift $ applyFlagsToTCWarnings $ crWarnings checkResult
                              case mw of
                                [] -> do
                                  lift $ compilerMain (prettyLatexBackend' p dir_Abs) IsMain checkResult
                                  --  case backend of
                                  --   LaTeX                    -> callBackend "LaTeX" IsMain checkResult
                                  --   QuickLaTeX               -> callBackend "LaTeX" IsMain checkResult
                                  --   OtherBackend "GHCNoMain" -> callBackend "GHC" NotMain checkResult   -- for backwards compatibility
                                  --   OtherBackend b           -> callBackend b IsMain checkResult
                                  display_info . Info_CompilationOk =<< lift B.getWarningsAndNonFatalErrors
                                w@(_:_) -> display_info $ Info_Error $ Info_CompilationError w
    in do 
        lift $ setInteractionOutputCallback externalInteractionOutputCallback
        res
        --  where
        --  allowUnsolved = backend `elem` [LaTeX, QuickLaTeX]
        --  mode | QuickLaTeX <- backend = Imp.ScopeCheck
        --       | otherwise             = Imp.TypeCheck RegularInteraction  -- reset InteractionMode



-- | Run a TCM action in IO; catch and pretty print errors.
externalRunTCMPrettyErrors :: TCM () -> IO ()
externalRunTCMPrettyErrors tcm = do
    r <- runTCMTop $ tcm `catchError` \err -> do
      s2s <- prettyTCWarnings' =<< getAllWarningsOfTCErr err
      s1  <- prettyError err
      let ss = filter (not . null) $ s2s ++ [s1]
      unless (null s1) (liftIO $ putStr $ unlines ss)
      throwError err
    case r of
      Right _ -> return ()
      Left _  -> exitAgdaWith TCMError
  `catchImpossible` \e -> do
    putStr $ show e
    exitAgdaWith ImpossibleError

