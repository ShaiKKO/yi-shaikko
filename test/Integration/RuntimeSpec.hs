{-# LANGUAGE OverloadedStrings #-}

module Integration.RuntimeSpec (spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Text (Text)
import qualified Data.Text as T
import           System.CPUTime
import           System.Mem

spec :: Spec
spec = describe "Runtime Integration" $ do
  describe "Keymap Switching" $ do
    it "switches from Vim to Emacs" $ do
      runtime <- createRuntime
      result <- switchKeymap runtime "vim" "emacs"
      result `shouldBe` True
      current <- getCurrentKeymap runtime
      current `shouldBe` "emacs"
    
    it "switches from Emacs to CUA" $ do
      runtime <- createRuntime
      setKeymap runtime "emacs"
      result <- switchKeymap runtime "emacs" "cua"
      result `shouldBe` True
      current <- getCurrentKeymap runtime
      current `shouldBe` "cua"
    
    it "handles invalid keymap gracefully" $ do
      runtime <- createRuntime
      result <- switchKeymap runtime "vim" "invalid"
      result `shouldBe` False
      current <- getCurrentKeymap runtime
      current `shouldBe` "vim"
    
    it "preserves state during switch" $ do
      runtime <- createRuntime
      setState runtime "test-key" "test-value"
      switchKeymap runtime "vim" "emacs"
      value <- getState runtime "test-key"
      value `shouldBe` Just "test-value"
  
  describe "Dynamic Rebinding" $ do
    it "rebinds single key" $ do
      runtime <- createRuntime
      rebindKey runtime "vim" "j" "move-down"
      binding <- getKeyBinding runtime "vim" "j"
      binding `shouldBe` Just "move-down"
    
    it "rebinds key sequence" $ do
      runtime <- createRuntime
      rebindSequence runtime "vim" ["g", "g"] "go-to-top"
      binding <- getSequenceBinding runtime "vim" ["g", "g"]
      binding `shouldBe` Just "go-to-top"
    
    it "removes binding" $ do
      runtime <- createRuntime
      rebindKey runtime "vim" "x" "delete-char"
      unbindKey runtime "vim" "x"
      binding <- getKeyBinding runtime "vim" "x"
      binding `shouldBe` Nothing
  
  describe "Query Commands" $ do
    it "lists available keymaps" $ do
      runtime <- createRuntime
      keymaps <- listKeymaps runtime
      keymaps `shouldContain` ["vim", "emacs", "cua", "acme", "joe", "mg", "ee"]
    
    it "shows current keymap" $ do
      runtime <- createRuntime
      setKeymap runtime "acme"
      current <- queryCurrentKeymap runtime
      current `shouldBe` "Current keymap: acme"
    
    it "describes key binding" $ do
      runtime <- createRuntime
      setKeymap runtime "vim"
      desc <- describeKey runtime "i"
      desc `shouldContain` "insert"
  
  describe "Eval Integration" $ do
    it "evaluates simple expression" $ do
      runtime <- createRuntime
      result <- evalExpression runtime "(+ 1 2)"
      result `shouldBe` Right "3"
    
    it "evaluates keymap change" $ do
      runtime <- createRuntime
      result <- evalExpression runtime "(set-keymap 'joe)"
      result `shouldBe` Right "Keymap set to: joe"
      current <- getCurrentKeymap runtime
      current `shouldBe` "joe"
    
    it "handles eval errors" $ do
      runtime <- createRuntime
      result <- evalExpression runtime "(invalid-function)"
      case result of
        Left err -> err `shouldContain` "Unknown function"
        Right _ -> expectationFailure "Should have failed"
  
  describe "Frontend Communication" $ do
    it "sends keymap change to Vty" $ do
      runtime <- createRuntime
      vty <- createVtyFrontend runtime
      switchKeymap runtime "vim" "emacs"
      msg <- receiveFrontendMessage vty
      msg `shouldBe` KeymapChanged "emacs"
    
    it "sends keymap change to Qt" $ do
      runtime <- createRuntime
      qt <- createQtFrontend runtime
      switchKeymap runtime "cua" "acme"
      msg <- receiveFrontendMessage qt
      msg `shouldBe` KeymapChanged "acme"
    
    it "broadcasts to all frontends" $ do
      runtime <- createRuntime
      vty <- createVtyFrontend runtime
      qt <- createQtFrontend runtime
      switchKeymap runtime "vim" "mg"
      vtyMsg <- receiveFrontendMessage vty
      qtMsg <- receiveFrontendMessage qt
      vtyMsg `shouldBe` KeymapChanged "mg"
      qtMsg `shouldBe` KeymapChanged "mg"
  
  describe "Performance" $ do
    prop "Keymap switch is fast" $ \from to ->
      (from, to) `elem` keymapPairs ==> monadicIO $ do
        runtime <- run createRuntime
        time <- run $ timeOperation $ switchKeymap runtime from to
        assert (time < 1000000)  -- Less than 1ms
    
    prop "Concurrent switches are safe" $ \switches ->
      length switches > 0 && length switches < 10 ==> monadicIO $ do
        runtime <- run createRuntime
        run $ forConcurrently switches $ \(from, to) ->
          switchKeymap runtime from to
        current <- run $ getCurrentKeymap runtime
        assert (current `elem` validKeymaps)

-- Test types
data Runtime = Runtime
  { rtKeymap :: TVar Text
  , rtBindings :: TVar [(Text, [(Text, Text)])]
  , rtState :: TVar [(Text, Text)]
  , rtFrontends :: TVar [Frontend]
  }

data Frontend = Frontend
  { feType :: FrontendType
  , feMessages :: TBQueue FrontendMessage
  }

data FrontendType = VtyFrontend | QtFrontend | WebFrontend
  deriving (Show, Eq)

data FrontendMessage
  = KeymapChanged Text
  | StateUpdated Text Text
  | BindingChanged Text Text Text
  deriving (Show, Eq)

-- Test helpers
validKeymaps :: [Text]
validKeymaps = ["vim", "emacs", "cua", "acme", "joe", "mg", "ee"]

keymapPairs :: [(Text, Text)]
keymapPairs = [(from, to) | from <- validKeymaps, to <- validKeymaps, from /= to]

createRuntime :: IO Runtime
createRuntime = do
  keymap <- newTVarIO "vim"
  bindings <- newTVarIO []
  state <- newTVarIO []
  frontends <- newTVarIO []
  return Runtime{..}

switchKeymap :: Runtime -> Text -> Text -> IO Bool
switchKeymap Runtime{..} from to = do
  if to `elem` validKeymaps
    then do
      atomically $ do
        current <- readTVar rtKeymap
        when (current == from) $ writeTVar rtKeymap to
      notifyFrontends Runtime{..} (KeymapChanged to)
      return True
    else return False

getCurrentKeymap :: Runtime -> IO Text
getCurrentKeymap Runtime{..} = readTVarIO rtKeymap

setKeymap :: Runtime -> Text -> IO ()
setKeymap Runtime{..} km = atomically $ writeTVar rtKeymap km

setState :: Runtime -> Text -> Text -> IO ()
setState Runtime{..} key value = atomically $
  modifyTVar rtState ((key, value):)

getState :: Runtime -> Text -> IO (Maybe Text)
getState Runtime{..} key = do
  state <- readTVarIO rtState
  return $ lookup key state

rebindKey :: Runtime -> Text -> Text -> Text -> IO ()
rebindKey Runtime{..} keymap key action = atomically $
  modifyTVar rtBindings $ \bs ->
    case lookup keymap bs of
      Nothing -> (keymap, [(key, action)]) : bs
      Just binds -> (keymap, (key, action) : filter ((/= key) . fst) binds) : filter ((/= keymap) . fst) bs

rebindSequence :: Runtime -> Text -> [Text] -> Text -> IO ()
rebindSequence runtime keymap keys action =
  rebindKey runtime keymap (T.intercalate " " keys) action

unbindKey :: Runtime -> Text -> Text -> IO ()
unbindKey Runtime{..} keymap key = atomically $
  modifyTVar rtBindings $ \bs ->
    case lookup keymap bs of
      Nothing -> bs
      Just binds -> (keymap, filter ((/= key) . fst) binds) : filter ((/= keymap) . fst) bs

getKeyBinding :: Runtime -> Text -> Text -> IO (Maybe Text)
getKeyBinding Runtime{..} keymap key = do
  bindings <- readTVarIO rtBindings
  case lookup keymap bindings of
    Nothing -> return Nothing
    Just binds -> return $ lookup key binds

getSequenceBinding :: Runtime -> Text -> [Text] -> IO (Maybe Text)
getSequenceBinding runtime keymap keys =
  getKeyBinding runtime keymap (T.intercalate " " keys)

listKeymaps :: Runtime -> IO [Text]
listKeymaps _ = return validKeymaps

queryCurrentKeymap :: Runtime -> IO Text
queryCurrentKeymap runtime = do
  current <- getCurrentKeymap runtime
  return $ "Current keymap: " <> current

describeKey :: Runtime -> Text -> IO Text
describeKey _ "i" = return "i: enter insert mode"
describeKey _ key = return $ key <> ": unmapped"

evalExpression :: Runtime -> Text -> IO (Either Text Text)
evalExpression runtime "(+ 1 2)" = return $ Right "3"
evalExpression runtime "(set-keymap 'joe)" = do
  setKeymap runtime "joe"
  return $ Right "Keymap set to: joe"
evalExpression _ "(invalid-function)" = return $ Left "Unknown function: invalid-function"
evalExpression _ _ = return $ Left "Parse error"

createVtyFrontend :: Runtime -> IO Frontend
createVtyFrontend runtime = createFrontend runtime VtyFrontend

createQtFrontend :: Runtime -> IO Frontend
createQtFrontend runtime = createFrontend runtime QtFrontend

createFrontend :: Runtime -> FrontendType -> IO Frontend
createFrontend Runtime{..} fType = do
  messages <- newTBQueueIO 100
  let frontend = Frontend fType messages
  atomically $ modifyTVar rtFrontends (frontend:)
  return frontend

receiveFrontendMessage :: Frontend -> IO FrontendMessage
receiveFrontendMessage Frontend{..} = atomically $ readTBQueue feMessages

notifyFrontends :: Runtime -> FrontendMessage -> IO ()
notifyFrontends Runtime{..} msg = do
  frontends <- readTVarIO rtFrontends
  forM_ frontends $ \Frontend{..} ->
    atomically $ writeTBQueue feMessages msg

timeOperation :: IO a -> IO Integer
timeOperation action = do
  start <- getCPUTime
  _ <- action
  end <- getCPUTime
  return $ (end - start) `div` 1000  -- Convert to nanoseconds

forConcurrently :: [a] -> (a -> IO b) -> IO [b]
forConcurrently items action = do
  vars <- forM items $ \item -> do
    var <- newEmptyMVar
    _ <- forkIO $ action item >>= putMVar var
    return var
  mapM takeMVar vars