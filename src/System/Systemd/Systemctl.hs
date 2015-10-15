{-# LANGUAGE OverloadedStrings #-}

module System.Systemd.Systemctl where

import           Control.Monad
import           Control.Monad.Trans
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Shelly


type Name = Text

-- | A `systemd` service
newtype Service = MkService Name
                  deriving (Eq, Show)

-- | A `systemd` target
newtype Target = MkTarget Name
                 deriving (Eq, Show)


-- | A `systemd` unit
data Unit = Service Service
          | Target Target
          deriving (Eq, Show)

-- | Gets the `Name` of a `Unit`
unitName :: Unit -> Name
unitName (Service (MkService s)) = s
unitName (Target (MkTarget s)) = s

-- | Create a `Service` instance
service :: Name -> Unit
service = Service . MkService . (<> ".service")

-- | Create a `Target` instance
target :: Name -> Unit
target = Target . MkTarget . (<> ".target")

-- | A flag to a command.
type Flag = Text

-- | Actions passed to @systemctl@
--
-- Eg: @enable@, @start@, @reload@, etc
type Action =  Text


-- | Generate the parameters to be passed to @systemctl@
systemctlParams :: Unit -> Action -> [Flag] -> [Text]
systemctlParams unit action flags = [action, unitName unit] ++ flags


-- | Execute a systemctl command on a `Unit`.
systemctl :: MonadIO m => Action -> [Flag] -> Unit -> m Text
systemctl a f u = shelly $ print_stdout False $ systemctlSh a f u

-- | Run `systemctl` in the `Sh` monad.
systemctlSh :: Action -> [Flag] -> Unit -> Sh Text
systemctlSh a f u = do
  let prms = systemctlParams u a f
  run "systemctl" prms

-- | Restart a `Unit`
restart :: MonadIO m => Unit -> m ()
restart = void . systemctl "restart" []

-- | Start a `Unit`
start :: MonadIO m => Unit -> m ()
start = void . systemctl "start" []

-- | Stop a `Unit`
stop :: MonadIO m => Unit -> m ()
stop = void . systemctl "stop" []

-- | Check if a `Unit` is active.
isActive :: MonadIO m => Unit -> m Bool
isActive u = shelly $ errExit False $ do
  void $ systemctlSh "is-active" ["--quiet"] u
  (0 ==) <$> lastExitCode

-- | Check if a `Unit` is failed.
isFailed :: MonadIO m => Unit -> m Bool
isFailed u = shelly $ errExit False $ do
  void $ systemctlSh "is-failed" ["--quiet"] u
  (0 ==) <$> lastExitCode

-- | Check if a `Unit` is enabled
--
-- Note: only units that are "enabled" or "enabled-runtime" are
-- considered as enabled.
isEnabled :: MonadIO m => Unit -> m Bool
isEnabled u = shelly $ errExit False $ print_stdout False $ do
  str <- T.strip <$> systemctlSh "is-enabled" [] u
  exitCode <- lastExitCode

  -- see (man 1 systemctl) section on "is-enabled" command
  let enabled = str `elem` ["enabled", "enabled-runtime"]
  return $ enabled && exitCode == 0

-- | Call @systemctl show <unit>@
show :: MonadIO m => Unit -> m (HashMap Text Text)
show u = do
  status_lines <- T.lines <$> systemctl "show" [] u
  let pairs = map
                ((\(k,v) -> (k, T.dropWhile (=='=') v)) . T.breakOn "=")
                status_lines
  return $ HashMap.fromList pairs

