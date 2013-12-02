{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.SFML
  ( SFML
  , runSFML
  , createRenderWindow
  , createSprite
  , clearRenderWindow
  , waitEvent
  , liftIO
  , display
  )where

import SFML.Graphics.Color
import SFML.SFDisplayable (SFDisplayable)
import SFML.Window (SFEvent, SFWindow, VideoMode, WindowStyle, ContextSettings)
import SFML.Graphics (Sprite, RenderWindow)
import qualified SFML.Graphics as G
import Control.Monad.State.Strict


--------------------------------------------------------------------------------
type DestroyAction = IO ()


--------------------------------------------------------------------------------
type SFMLState = [DestroyAction]


--------------------------------------------------------------------------------
type SFML a = StateT SFMLState IO a


--------------------------------------------------------------------------------
createRenderWindow :: VideoMode
                   -> String
                   -> [WindowStyle]
                   -> Maybe ContextSettings
                   -> SFML RenderWindow
createRenderWindow vm t stl cs = do
    wnd <- liftIO $ G.createRenderWindow vm t stl cs
    modify $ \s -> G.destroy wnd : s
    return wnd


--------------------------------------------------------------------------------
clearRenderWindow :: RenderWindow -> Color -> SFML ()
clearRenderWindow wnd = io . G.clearRenderWindow wnd


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
createSprite :: SFML Sprite
createSprite = do
 spr <- io . G.err $ G.createSprite
 modify $ \s -> G.destroy spr : s
 return spr


--------------------------------------------------------------------------------
-- | Run the SFML monad, calling all the destructors appropriately.
runSFML :: SFML a -> IO ()
runSFML = join . fmap sequence_ . flip execStateT []
