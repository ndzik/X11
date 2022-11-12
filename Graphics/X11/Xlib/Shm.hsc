{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics.X11.Xlib.Shm () where

import Data.Data
import Data.Word
import Foreign.C.Types
import Foreign.Storable
import Foreign.Ptr
import Graphics.X11.Types
import Graphics.X11.Xlib.Types

#include "HsXlib.h"

type ShmSeg = Word64

data ShmSegmentInfo = ShmSegmentInfo
  { shm_seg :: !ShmSeg,
    shm_id :: !CInt,
    shm_addr :: !(Ptr Word8),
    shm_readOnly :: !Bool
  }
  deriving (Eq, Show, Typeable)

instance Storable ShmSegmentInfo where
  sizeOf _ = #{size XShmSegmentInfo}
  alignment _ = alignment (undefined::CInt)
  peek p = do
    info <- #{peek XShmSegmentInfo,shmseg} p
    shmid <- #{peek XShmSegmentInfo,shmid} p
    addr <- #{peek XShmSegmentInfo,shmaddr} p
    readOnly <- #{peek XShmSegmentInfo,readOnly} p
    return (ShmSegmentInfo info shmid addr readOnly)
  poke p seginfo = do
     #{poke XShmSegmentInfo,shmseg} p $ shm_seg seginfo
     #{poke XShmSegmentInfo,shmid} p $ shm_id seginfo
     #{poke XShmSegmentInfo,shmaddr} p $ shm_addr seginfo
     #{poke XShmSegmentInfo,readOnly} p $ shm_readOnly seginfo

foreign import ccall "XShmQueryExtension"
  cXShmQueryExtension :: Display -> IO Bool

foreign import ccall "XShmCreateImage"
  cXShmCreateImage :: Display -> Visual -> CInt -> CInt -> CInt -> CInt -> Ptr Word8 -> Ptr ShmSegmentInfo -> IO Image

foreign import ccall "XShmAttach"
  cXShmAttach :: Display -> Ptr ShmSegmentInfo -> IO Bool

foreign import ccall "XShmDetach"
  cXShmDetach :: Display -> Ptr ShmSegmentInfo -> IO Bool

foreign import ccall "XShmGetImage"
  cXShmGetImage :: Display -> Drawable -> Image -> CInt -> CInt -> CULong -> IO Bool
