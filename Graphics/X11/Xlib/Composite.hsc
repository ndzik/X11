{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.X11.Xlib.Composite (
        xCompositeRedirectWindow,
        xCompositeRedirectSubwindows,
        xCompositeNameWindowPixmap,
        xCompositeQueryExtension,
        xCompositeGetOverlayWindow,
        xCompositeReleaseOverlayWindow,
        CompositeRedirectMode(..)
        ) where

import Foreign.C.Types
import Graphics.X11.Types
import Graphics.X11.Xlib.Types
import Foreign.Storable
import Foreign.Marshal.Pool
import Foreign.Ptr

data CompositeRedirectMode = CompositeRedirectAutomatic
                           | CompositeRedirectManual
                           deriving (Enum)

xCompositeRedirectWindow :: Display -> Window -> CompositeRedirectMode -> IO ()
xCompositeRedirectWindow dp w m = cXCompositeRedirectWindow dp w (fromIntegral . fromEnum $ m)

xCompositeRedirectSubwindows :: Display -> Window -> CompositeRedirectMode -> IO ()
xCompositeRedirectSubwindows dp w m = cXCompositeRedirectSubwindows dp w (fromIntegral . fromEnum $ m)

xCompositeNameWindowPixmap :: Display -> Window -> IO Pixmap
xCompositeNameWindowPixmap = cXCompositeNameWindowPixmap

xCompositeQueryExtension :: Display -> IO (Maybe (CInt, CInt))
xCompositeQueryExtension dp = withPool $ \pool -> do eventPtr <- pooledMalloc pool
                                                     errorPtr <- pooledMalloc pool
                                                     ret <- cXCompositeQueryExtension dp eventPtr errorPtr
                                                     event <- peek eventPtr
                                                     err <- peek errorPtr
                                                     if ret then
                                                       return . Just $ (event, err)
                                                      else
                                                       return Nothing

xCompositeGetOverlayWindow :: Display -> Window -> IO Window
xCompositeGetOverlayWindow = cXCompositeGetOverlayWindow

xCompositeReleaseOverlayWindow :: Display -> Window -> IO ()
xCompositeReleaseOverlayWindow = cXCompositeReleaseOverlayWindow

foreign import ccall "XCompositeRedirectWindow"
  cXCompositeRedirectWindow :: Display -> Window -> CInt -> IO ()

foreign import ccall "XCompositeRedirectSubwindows"
  cXCompositeRedirectSubwindows :: Display -> Window -> CInt -> IO ()

foreign import ccall "XCompositeNameWindowPixmap"
  cXCompositeNameWindowPixmap :: Display -> Window -> IO Pixmap

foreign import ccall "XCompositeQueryExtension"
  cXCompositeQueryExtension :: Display -> Ptr CInt -> Ptr CInt -> IO Bool

foreign import ccall "XCompositeGetOverlayWindow"
  cXCompositeGetOverlayWindow :: Display -> Window -> IO Window

foreign import ccall "XCompositeReleaseOverlayWindow"
  cXCompositeReleaseOverlayWindow :: Display -> Window -> IO ()
