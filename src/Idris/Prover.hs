module Idris.Prover where

import Idris.Core.Elaborate hiding (Tactic(..))
import Idris.Core.TT
import Idris.Core.Evaluate
import Idris.Core.CaseTree
import Idris.Core.Typecheck

import Idris.AbsSyntax
import Idris.Delaborate
import Idris.ElabDecls
import Idris.ElabTerm
import Idris.Parser
import Idris.Error
import Idris.DataOpts
import Idris.Completion
import Idris.IdeSlave

import Text.Trifecta.Result(Result(..))

import System.Console.Haskeline
import System.Console.Haskeline.History
import Control.Monad.State.Strict

import Data.List
import Data.Maybe
import qualified Data.Map as M

import Util.Pretty
import Debug.Trace

prover :: Bool -> Name -> Idris ()
prover lit x =
           do (es, ty) <- startMVProof x
              prove lit x ty es 

startMVProof :: Name -> Idris (ElabState [PDecl], Type)
startMVProof x = do
  ctxt <- getContext
  i <- getIState
  case lookupTy x ctxt of
    [t] -> if elem x (map fst (idris_metavars i))
             then mkPS ctxt x t
             else ifail $ show x ++ " is not a metavariable"
    _ -> fail "No such metavariable"
  where
   mkPS :: Context -> Name -> Type -> Idris (ElabState [PDecl], Type)
   mkPS ctxt n ty = return (ES (initElaborator n ctxt ty, []) "" Nothing, ty)

maybeIntros :: ElabState [PDecl] -> Idris (ElabState [PDecl])
maybeIntros es = do
  i <- getIState
  idrisCatch
    (do (_, st) <- elabStep es (runTac True i Intros); return st)
    (\err -> return es)

maybeRefine :: ElabState [PDecl] -> Name -> Idris (Maybe (ElabState [PDecl]))
maybeRefine es x = do
  i <- getIState
  idrisCatch
    (do (_, st) <- elabStep es (runTac True i $ Refine x []); return $ Just st)
    (\err -> return Nothing)

filterIdentifiers :: ElabState [PDecl] -> [Name] -> Idris [(Name, ElabState [PDecl])]
filterIdentifiers es xs = do
  fmap catMaybes $ forM xs
    (\x -> do
       mes <- maybeRefine es x
       return $ fmap (\es -> (x, es)) mes)

overFilterIdentifiers :: ElabState [PDecl] -> [Name] -> Idris [(Name, ElabState [PDecl])]
overFilterIdentifiers es xs = fmap (filter (\(_,es) -> let p = proof es
                                                       in not $ willIncomplete $ pterm p))
                              $ filterIdentifiers es xs

localIdentifiers :: ElabState [PDecl] -> [Name]
localIdentifiers es = let OK env = envAtFocus $ proof es
                      in map fst env

globalIdentifiers :: Idris [Name]
globalIdentifiers = fmap (map fst . M.toList . definitions) getContext

countNewMetavariables :: ElabState [PDecl] -> Int
countNewMetavariables es = let ps = proof es
                               explicit = dontunify ps
                               open = holes ps
                           in length $ intersect open explicit

readableNames :: [Name] -> [Name]
readableNames = filter (\n -> case n of
                                UN _ -> True
                                _ -> False)

delaboratedProofTerm :: ElabState [PDecl] -> Idris PTerm
delaboratedProofTerm es = do
  i <- getIState
  return $ delabMV i (pterm $ proof es)

showProof :: Bool -> Name -> [String] -> String
showProof lit n ps
    = bird ++ show n ++ " = proof" ++ break ++
             showSep break (map (\x -> "  " ++ x) ps) ++
                     break ++ "\n"
  where bird = if lit then "> " else ""
        break = "\n" ++ bird

proverSettings :: ElabState [PDecl] -> Settings Idris
proverSettings e = setComplete (proverCompletion (assumptionNames e)) defaultSettings

assumptionNames :: ElabState [PDecl] -> [String]
assumptionNames e
  = case envAtFocus (proof e) of
         OK env -> names env
  where names [] = []
        names ((MN _ _, _) : bs) = names bs
        names ((n, _) : bs) = show n : names bs

prove :: Bool -> Name -> Type -> ElabState [PDecl] -> Idris ()
prove lit n ty es
    = do ideslavePutSExp "start-proof-mode" n
         ctxt <- getContext
         (tm, prf) <- ploop True ("-" ++ show n) [] es Nothing
         iLOG $ "Adding " ++ show tm
         iputStrLn $ showProof lit n prf
         i <- getIState
         ideslavePutSExp "end-proof-mode" n
         let proofs = proof_list i
         putIState (i { proof_list = (n, prf) : proofs })
         let tree = simpleCase False True False CompileTime (fileFC "proof") [] [([], P Ref n ty, tm)]
         logLvl 3 (show tree)
         (ptm, pty) <- recheckC (fileFC "proof") [] tm
         logLvl 5 ("Proof type: " ++ show pty ++ "\n" ++
                   "Expected type:" ++ show ty)
         case converts ctxt [] ty pty of
              OK _ -> return ()
              Error e -> ierror (CantUnify False ty pty e [] 0)
         ptm' <- applyOpts ptm
         updateContext (addCasedef n (CaseInfo True False) False False True False
                                 []
                                 [Right (P Ref n ty, ptm)]
                                 [([], P Ref n ty, ptm)]
                                 [([], P Ref n ty, ptm)]
                                 [([], P Ref n ty, ptm)]
                                 [([], P Ref n ty, ptm')] ty)
         solveDeferred n
elabStep :: ElabState [PDecl] -> ElabD a -> Idris (a, ElabState [PDecl])
elabStep st e = do case runStateT e st of
                     OK (a, st') -> return (a, st')
                     Error a -> ierror a

dumpState :: IState -> ProofState -> Idris ()
dumpState ist (PS nm [] _ _ tm _ _ _ _ _ _ _ _ _ _ _ _ _ _) =
  do rendered <- iRender $ prettyName False [] nm <> colon <+> text "No more goals."
     iputGoal rendered
dumpState ist ps@(PS nm (h:hs) _ _ tm _ _ _ _ _ _ problems i _ _ ctxy _ _ _) = do
  let OK ty  = goalAtFocus ps
  let OK env = envAtFocus ps
  let state = prettyOtherGoals hs <> line <>
              prettyAssumptions env <> line <>
              prettyGoal (zip (assumptionNames env) (repeat False)) ty
  rendered <- iRender state
  iputGoal rendered

  where
    showImplicits = opt_showimp (idris_options ist)

    tPretty bnd t = pprintPTerm showImplicits bnd $ delab ist t

    assumptionNames :: Env -> [Name]
    assumptionNames = map fst

    prettyPs :: [(Name, Bool)] -> Env -> Doc OutputAnnotation
    prettyPs bnd [] = empty
    prettyPs bnd ((n@(MN _ r), _) : bs)
        | r == txt "rewrite_rule" = prettyPs ((n, False):bnd) bs
    prettyPs bnd ((n, Let t v) : bs) =
      nest nestingSize (bindingOf n False <+> text "=" <+> tPretty bnd v <> colon <+>
        tPretty ((n, False):bnd) t <> line <> prettyPs ((n, False):bnd) bs)
    prettyPs bnd ((n, b) : bs) =
      line <> bindingOf n False <+> colon <+> align (tPretty bnd (binderTy b)) <> prettyPs ((n, False):bnd) bs

    prettyG bnd (Guess t v) = tPretty bnd t <+> text "=?=" <+> tPretty bnd v
    prettyG bnd b = tPretty bnd $ binderTy b

    prettyGoal bnd ty =
      text "----------                 Goal:                  ----------" <$$>
      bindingOf h False <+> colon <+> align (prettyG bnd ty)

    prettyAssumptions env =
      if length env == 0 then
        empty
      else
        text "----------              Assumptions:              ----------" <>
        nest nestingSize (prettyPs [] $ reverse env)

    prettyOtherGoals hs =
      if length hs == 0 then
        empty
      else
        text "----------              Other goals:              ----------" <$$>
        nest nestingSize (align . cat . punctuate (text ",") . map (flip bindingOf False) $ hs)

lifte :: ElabState [PDecl] -> ElabD a -> Idris a
lifte st e = do (v, _) <- elabStep st e
                return v

receiveInput :: ElabState [PDecl] -> Idris (Maybe String)
receiveInput e =
  do i <- getIState
     l <- runIO $ getLine
     (sexp, id) <- case parseMessage l of
                     Left err -> ierror err
                     Right (sexp, id) -> return (sexp, id)
     putIState $ i { idris_outputmode = (IdeSlave id) }
     case sexpToCommand sexp of
       Just (REPLCompletions prefix) ->
         do (unused, compls) <- proverCompletion (assumptionNames e) (reverse prefix, "")
            let good = SexpList [SymbolAtom "ok", toSExp (map replacement compls, reverse unused)]
            ideslavePutSExp "return" good
            receiveInput e
       Just (Interpret cmd) -> return (Just cmd)
       Nothing -> return Nothing

ploop :: Bool -> String -> [String] -> ElabState [PDecl] -> Maybe History -> Idris (Term, [String])
ploop d prompt prf e h
    = do i <- getIState
         when d $ dumpState i (proof e)
         (x, h') <-
           case idris_outputmode i of
             RawOutput -> runInputT (proverSettings e) $
                          -- Manually track the history so that we can use the proof state
                          do _ <- case h of
                               Just history -> putHistory history
                               Nothing -> return ()
                             l <- getInputLine (prompt ++ "> ")
                             h' <- getHistory
                             return (l, Just h')
             IdeSlave n ->
               do isetPrompt prompt
                  i <- receiveInput e
                  return (i, h)
         (cmd, step) <- case x of
            Nothing -> do iPrintError ""; ifail "Abandoned"
            Just input -> do return (parseTactic i input, input)
         case cmd of
            Success Abandon -> do iPrintError ""; ifail "Abandoned"
            _ -> return ()
         (d, st, done, prf') <- idrisCatch
           (case cmd of
              Failure err -> do iPrintError (show err)
                                return (False, e, False, prf)
              Success Undo -> do (_, st) <- elabStep e loadState
                                 iPrintResult ""
                                 return (True, st, False, init prf)
              Success ProofState -> do iPrintResult ""
                                       return (True, e, False, prf)
              Success ProofTerm -> do tm <- lifte e get_term
                                      iPrintResult $ "TT: " ++ show tm ++ "\n"
                                      return (False, e, False, prf)
              Success Qed -> do hs <- lifte e get_holes
                                when (not (null hs)) $ ifail "Incomplete proof"
                                iPrintResult "Proof completed!"
                                return (False, e, True, prf)
              Success tac -> do (_, e) <- elabStep e saveState
                                (_, st) <- elabStep e (runTac True i tac)
--                               trace (show (problems (proof st))) $
                                iPrintResult ""
                                return (True, st, False, prf ++ [step]))
           (\err -> do iPrintError (pshow i err)
                       return (False, e, False, prf))
         ideslavePutSExp "write-proof-state" (prf', length prf')
         if done then do (tm, _) <- elabStep st get_term
                         return (tm, prf')
                 else ploop d prompt prf' st h'

