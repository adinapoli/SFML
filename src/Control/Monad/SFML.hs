{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.SFML 
  ( SFML
  , runSFML
  , createRenderWindow
  , liftIO
  )where

import SFML.Window
import SFML.Graphics hiding (createRenderWindow)
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
runSFML :: SFML a -> IO ()
runSFML = join . fmap sequence_ . flip execStateT []
