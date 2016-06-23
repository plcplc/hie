-- in the browser, evaluate: "Haskell.log.enable(true)" to see debugging info..
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.HIE.ThreePenny where

-- import Graphics.HIE.AST
import qualified Graphics.UI.Threepenny as TP
import Graphics.UI.Threepenny.Core as TP hiding (UI)
import qualified Data.Map.Strict as M
import qualified Reactive.Threepenny as Reactive

type TPUI = TP.UI

type Document = M.Map String Exp
type Exp = String

emptyDoc :: Document
emptyDoc = M.empty

data DeclarationsW = DeclarationsW {
  mlElement :: Element,
  mlModel   :: Behavior Document,
  mlMutate  :: Event (Document -> Document)
}

bindingsWMutate :: DeclarationsW -> Event (Document -> Document)
bindingsWMutate = mlMutate

instance Widget DeclarationsW where
  getElement = mlElement

data DeclarationW = DeclarationW {
  beElement :: Element,
  beMutate  :: Event (Exp -> Exp)
}

instance Widget DeclarationW where
  getElement = beElement

tpMain :: IO ()
tpMain = do
  startGUI defaultConfig clientMain

clientMain :: Window -> TPUI ()
clientMain _ = mdo
  (askWindow >>= getBody) #+ [TP.h1 # set text "HIE Document"]
  ms <- documentContainerW bDocument
  let eMutate = bindingsWMutate ms
  bDocument <- accumB emptyDoc $ concatenate <$> unions
    [ eMutate ]
  (askWindow >>= getBody) #+ [element ms]
  return ()

documentContainerW :: Behavior Document -> TPUI DeclarationsW
documentContainerW documentB = mdo

  containerElm <- TP.div

  
  {-

  * The 'decomposeMap' approach needs more work before it's practical.
    I observed, that whenever the model changed the children (our bindings)
    would be entirely removed and recreated, removing focus from the active
    entry widget. In order to be usable we'll have to make it take incremental
    steps.

  * Instead, for now we accept the flaw that the order of bound
    expressions in the UI has no relation to their order in the map.

  (eChildrenMut, bindingsWs) <- decomposeMap
    bDocument (\name bBinding -> do
                  boundW <- declarationW name bBinding
                  return (beMutate boundW, boundW))
  return documentContainer # sink children (map getElement <$> bindingsWs)
  -}

  -- The UI for adding/removing bindings.
  addButton <- TP.button # set text "Add Binding"
  removeButton <- TP.button # set text "Remove Binding"
  bindingNameEntry <- TP.entry (maybe "" id <$> bindingNameModel)
  bindingNameModel <- stepper Nothing $ head <$> unions
    [ Just <$> rumors (TP.userText bindingNameEntry)
    , Nothing <$ TP.click addButton
    , Nothing <$ TP.click removeButton
    ]

  let createBindingEv = filterJust (bindingNameModel <@ TP.click addButton)
  {-
  let eCreateBinding =
        (flip M.insert "undefined") <$> filterJust (bindingNameModel <@ TP.click addButton)
  let eDeleteBinding =
        M.delete <$> filterJust (bindingNameModel <@ TP.click remButton)
  -}

  -- Wire (impurely) the eCreateBinding to actually creating UI elements.
  w <- askWindow
  (childrenMutEv, fireChildrenMut) <- liftIO newEvent
  liftIO $ Reactive.register createBindingEv
    (\name -> runUI w $ do
        -- ARGH. remember to actually insert (name, expr) through
        -- childrenMutEv! The failure mode is not nice when using
        -- partial functions (here: M.!)in behaviors or events. The
        -- threepenny server thread just silently dies with no message
        -- on the console. Keep this in mind.
        liftIO $ fireChildrenMut (M.insert name "undefined")
        let bindingB = (M.! name) <$> documentB
        exprWidget <- declarationW name bindingB
        return containerElm #+ [ return $ beElement exprWidget ]
        liftIO $ Reactive.register (flip M.adjust name <$> beMutate exprWidget) fireChildrenMut
        return ())

  root <- TP.div # set children
    [ containerElm
    , getElement bindingNameEntry
    -- , pDebug
    , addButton
    , removeButton ]

  return $ DeclarationsW {
    mlElement = root,
    mlModel = documentB,
    mlMutate = concatenate <$> unions
      [ -- createBindingEv
      --, eDeleteBinding
      childrenMutEv ]
  }

-- | This widget represents an evaluatable expression bound to a name.
declarationW :: String -> Behavior (Exp) -> TPUI DeclarationW
declarationW name bindingB = mdo

  nameElm <- TP.span  # set text name
  bindingElm <- TP.entry bindingB

  w <- askWindow
  liftIO $ Reactive.onChange bindingB (\v -> runUI w $ debug (show v))

  -- 0. Next up: Clean up the mess of this module/project:
  --             'git init'@github? -> remove stuff -> tidy up -> commit -> push

  -- 1. Next up: Wire in 'Hint' here.
  evaluationResultElm <- TP.div # set text "<Evaluation here...>"
  typeCheckResultElm  <- TP.div # set text "<type here...>"

  -- 2. Next up: Make a means for attaching an evaluation behaviour to the declaration
  --             (Manual/Reactive). It's part of the 'Document'. Consult notes.
  --             Do we need to do anything special to achieve 'Reactive'?

  -- 2. Next up: Make a means for attaching UI annotations (different ways to visualise a computed value).
  --             Consult notes. It's part of the 'Document'.
  

  e <- TP.div # set style
    [ ("background", "#E0E0E0")
    , ("padding", "5px")
    , ("margin", "5px")
    ]
    #+
    [ element nameElm
    , element bindingElm
    , element evaluationResultElm
    , element typeCheckResultElm
    -- , element debugSpan
    ]

  return $ DeclarationW e (const <$> rumors (TP.userText bindingElm)) -- (pure (const "foo") <@ TP.click fooBtn)

{-
-- Eller dette?
switchE :: Event (Event a) -> TPUI (Event a)
switchE = undefined

switchB :: Behavior a -> Event (Behavior a) -> TPUI (Behavior a)
switchB = undefined

-- Kan vi løse vores declarationW-problem med switchE/switchB?
-- Eksperiment med dekomponering af Behaviors.
step1 ::
  Behavior [String] ->
  TPUI (Behavior [(Event String, Element)])
step1 = undefined

step2 ::
  Behavior [Event String] ->
  TPUI (Moment [Event String])
step2 = undefined

step3 ::
  Moment [Event String)] ->
  Moment (Event ([String] -> [String]))
step3 = undefined

-- Skal vi i virkeligheden have Moment === Behavior?

step3 ::
  Moment a -> (a -> UI ()) -> UI ()

-- Det her kan jeg ikke se at vi kan gøre på en fornuftig måde i TP...
-- ... Specielt når vi ikke kan lave event-unregistration...
higherOrderEvent :: Behavior (Event a) -> UI (Event a)
higherOrderEvent = undefined
-}


{-
frpList :: (TVar Document) -> TPUI ()
frpList docTV = mdo
  moduleNameEntry <- TP.entry (pure "")
  addModuleButton <- TP.button # set TP.text "Add Module"

  moduleName <- stepper Nothing
    $ (pure Just) <@> rumors (userText moduleNameEntry)

  -- A behavior of the modules currently present in the impure map.
  modules <- stmMapBehavior docTV

  -- Register a handler that updates the impure map.
  let addModule =
    (,M.fromList [])
    <$> filterJust (maybeExpTxt <@ click evalButton)
  _ <- stmMapInsert docTV addModule

  modulesUI <- moduleWidget <$> addModule
  moduleContainer <- TP.div # sink children modulesUI

  {-
  let bExps =  <$> bMap
  -}

  _ <- (getBody =<< askWindow) #+ [
    element moduleNameEntry,
    element addModuleButton,
    element moduleContainer
    ]
  return ()

moduleWidget :: (String, Module) -> TPUI Element
moduleWidget m = TP.div # set TP.text (show m)
-}


{-

modifyTVarOnEvent :: (b -> a -> a) -> TVar a -> Event b -> TPUI ()
modifyTVarOnEvent f tv ev =
  onEvent ev $ \b ->
    liftIO $ atomically $ modifyTVar tv $ f b

stmMapInsert :: (Ord k) => TVar (M.Map k a) -> Event (k, a) -> TPUI ()
stmMapInsert = modifyTVarOnEvent (uncurry M.insert)

stmMapDelete :: (Ord k) => TVar (M.Map k a) -> Event k -> TPUI ()
stmMapDelete = modifyTVarOnEvent M.delete

-- | Make a behavior from a TVar Map. (Generalises to any Eq a it appears.)
stmMapBehavior ::
  (Ord k, Eq a) =>
  TVar (M.Map k a) ->
  TPUI (Behavior (M.Map k a))
stmMapBehavior mapTV = do

  initialMap <- liftIO $ atomically $ readTVar mapTV

  (mapChangedEv, changeMap) <- liftIO newEvent

  w <- askWindow
  -- liftIOLater?
  mapRelayThread <- liftIO $ forkIO $ mapRelay initialMap changeMap
  on TP.disconnect w $ const $ liftIO $ do
    killThread mapRelayThread

  stepper initialMap mapChangedEv

  where

    -- mapRelay :: M.Map k a -> Handler (M.Map k a) -> IO ()
    mapRelay prevMap fire = do
      newMap <- atomically $ do
        newMap <- readTVar mapTV
        when (prevMap == newMap) retry
        return newMap
      _ <- fire newMap
      mapRelay newMap fire
-}

{-
reactiveMapUI ::
  Ord k =>
  M.Map k a ->
  Event (TPUI (k, a)) ->
  Event k ->
  TPUI (Behavior (M.Map k a))
reactiveMapUI initial addEv removeEv = do

  addEv' <- uiEvent addEv

  accumB initial $ concatenate <$> unions [
    uncurry M.insert <$> addEv',
    M.delete <$> removeEv
    ]

uiEvent :: Event (TPUI a) -> TPUI (Event a)
uiEvent ev = do
  (ev', ev'act) <- liftIO $ newEvent
  onEvent ev (\act -> do
    v <- act
    liftIO $ ev'act v)

  return ev'
    -}


{-
import Control.Concurrent
import Data.List
import Data.IORef
import Control.Concurrent.STM
import Control.Monad
decomposeMap ::
  (Ord k, Eq k, Widget w, Show k, Show v) =>
  Behavior (M.Map k v) ->
  (k -> Behavior v -> TPUI (Event (v -> v), w)) ->
  TPUI (Event (M.Map k v -> M.Map k v), Behavior [w])
decomposeMap bModel constructor = do
  (eChildMutations, fireChildMutations) <- liftIO $ newEvent
  (eModelChanged, fireModelChanged)     <- liftIO $ newEvent

  bindingElements <- liftIO $ newIORef M.empty
  window <- askWindow

  liftIOLater $ Reactive.onChange bModel
    (go window bindingElements fireChildMutations fireModelChanged)

  bElements <- stepper [] eModelChanged
  return (eChildMutations, bElements)

  where

  go window bindingElements fireChildMutations fireModelChanged newModel = runUI window $ do

    currentElements <- liftIO $ readIORef bindingElements
    let both = (M.keys currentElements) `intersect` (M.keys newModel)
    let deletedKeys = filter (not . (`elem` both)) $ M.keys currentElements
    let addedKeys = filter (not . (`elem` both)) $ M.keys newModel

    {-
    debug $ "both : " ++ show both
    debug $ "deletedKeys : " ++ show deletedKeys
    debug $ "addedKeys : " ++ show addedKeys
    debug $ "newModel : " ++ show newModel
    -}

    let currentElements' = foldr M.delete currentElements deletedKeys
    currentElements'' <- foldM
      (\els newEl -> do
        (ev, w) <- constructor newEl ((M.! newEl) <$> bModel)
        _ <- liftIO $ register ((\mut -> M.adjust mut newEl) <$> ev)
                        fireChildMutations
        return $ M.insert newEl w els)
      currentElements'
      addedKeys

    liftIO $ fireModelChanged (M.elems currentElements'')
    liftIO $ writeIORef bindingElements currentElements''
    return ()
-}
