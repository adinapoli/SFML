{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.SFML
  ( SFML
  , runSFML
  , createRenderWindow
  , waitEvent
  , liftIO
  , display
  )where

import SFML.SFDisplayable hiding (display)
import SFML.Window hiding (display, waitEvent)
import SFML.Graphics hiding (display, waitEvent, createRenderWindow)
import qualified SFML.Graphics as G
import Control.Monad.State.Strict


--------------------------------------------------------------------------------
type DestroyAction = IO ()


--------------------------------------------------------------------------------
type SFMLState = [DestroyAction]


--------------------------------------------------------------------------------
type SFML a = StateT SFMLState IO a


--------------------------------------------------------------------------------
createRenderWindow
    :: VideoMode
    -- ^ Video mode to use
    -> String
    -- ^ Window title
    -> [WindowStyle]
    -- ^ Window style
    -> Maybe ContextSettings
    -- ^ Creation settings ('Nothing' to use default values)
    -> SFML RenderWindow
createRenderWindow vm t stl cs = do
    wnd <- liftIO $ G.createRenderWindow vm t stl cs
    modify $ \s -> destroy wnd : s
    return wnd


--------------------------------------------------------------------------------
io :: IO a -> SFML a
io = liftIO


--------------------------------------------------------------------------------
display :: SFDisplayable a => a -> SFML ()
display = io . G.display


--------------------------------------------------------------------------------
waitEvent :: SFWindow a => a -> SFML (Maybe SFEvent)
waitEvent = io . G.waitEvent


--------------------------------------------------------------------------------
-- | Run the SFML monad, calling all the destructors appropriately.
runSFML :: SFML a -> IO ()
runSFML = join . fmap sequence_ . flip execStateT []
