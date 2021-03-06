{-# LANGUAGE DeriveDataTypeable #-}
module SFML.Graphics.Font
(
    module SFML.Utils
,   FontException(..)
,   fontFromFile
,   fontFromMemory
,   fontFromStream
,   copy
,   destroy
,   getGlyph
,   getKerning
,   getLineSpacing
,   getFontTexture
)
where


import SFML.Graphics.Glyph
import SFML.Graphics.Types
import SFML.SFCopyable
import SFML.SFResource
import SFML.System.InputStream
import SFML.Utils

import Control.Exception
import Data.Typeable
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr
import Foreign.Storable


checkNull :: Font -> Maybe Font
checkNull font@(Font ptr) = if ptr == nullPtr then Nothing else Just font


data FontException = FontException String deriving (Show, Typeable)

instance Exception FontException


-- | Create a new font from a file.
fontFromFile :: FilePath -> IO (Either FontException Font)
fontFromFile path =
    let err = FontException $ "Failed loading font from file " ++ show path
    in fmap (tagErr err . checkNull) $ withCAString path sfFont_createFromFile

foreign import ccall unsafe "sfFont_createFromFile"
    sfFont_createFromFile :: CString -> IO Font

-- \return A new sfFont object, or NULL if it failed

--CSFML_GRAPHICS_API sfFont* sfFont_createFromFile(const char* filename);


-- | Create a new image font a file in memory.
fontFromMemory
    :: Ptr Char -- ^ Pointer to the file data in memory
    -> Int -- ^ Size of the data to load, in bytes
    -> IO (Either FontException Font)

fontFromMemory pixels size =
    let err = FontException $ "Failed loading font from memory address " ++ show pixels
    in fmap (tagErr err . checkNull) $ sfFont_createFromMemory pixels (fromIntegral size)

foreign import ccall unsafe "sfFont_createFromMemory"
    sfFont_createFromMemory :: Ptr a -> CInt -> IO Font

-- \return A new sfFont object, or NULL if it failed

--CSFML_GRAPHICS_API sfFont* sfFont_createFromMemory(const void* data, size_t sizeInBytes);


-- | Create a new image font a custom stream.
fontFromStream :: InputStream -> IO (Either FontException Font)
fontFromStream stream =
    let err = FontException $ "Failed loading font from stream " ++ show stream
    in fmap (tagErr err . checkNull) $ with stream sfFont_createFromStream

foreign import ccall "sfFont_createFromStream"
    sfFont_createFromStream :: Ptr InputStream -> IO Font

-- \return A new sfFont object, or NULL if it failed

--CSFML_GRAPHICS_API sfFont* sfFont_createFromStream(sfInputStream* stream);


instance SFCopyable Font where
    
    {-# INLINABLE copy #-}
    copy = sfFont_copy


foreign import ccall unsafe "sfFont_copy"
    sfFont_copy :: Font -> IO Font

--CSFML_GRAPHICS_API sfFont* sfFont_copy(sfFont* font);


instance SFResource Font where
    
    {-# INLINABLE destroy #-}
    destroy = sfFont_destroy

foreign import ccall unsafe "sfFont_destroy"
    sfFont_destroy :: Font -> IO ()

--CSFML_GRAPHICS_API void sfFont_destroy(sfFont* font);


-- | Get a glyph in a font.
getGlyph
    :: Font -- ^ Source font
    -> Int -- ^ Unicode code point of the character to get
    -> Int -- ^ Character size, in pixels
    -> Bool -- ^ Retrieve the bold version or the regular one?
    -> IO Glyph

getGlyph font codePoint size bold =
    alloca $ \glyphPtr -> do
        sfFont_getGlyph_helper
            font (fromIntegral codePoint) (fromIntegral size) (fromIntegral . fromEnum $ bold) glyphPtr
        peek glyphPtr

foreign import ccall unsafe "sfFont_getGlyph_helper"
    sfFont_getGlyph_helper :: Font -> Word32 -> CUInt -> CInt -> Ptr Glyph -> IO ()

--CSFML_GRAPHICS_API sfGlyph sfFont_getGlyph(sfFont* font, sfUint32 codePoint, unsigned int characterSize, sfBool bold);


-- | Get the kerning value corresponding to a given pair of characters in a font.
getKerning
    :: Font -- ^ Source font
    -> Int  -- ^ Unicode code point of the first character
    -> Int  -- ^ Unicode code point of the second characte.r
    -> Int  -- ^ Character size, in pixels
    -> IO Int

getKerning font first second size =
    fmap fromIntegral $ sfFont_getKerning font (fromIntegral first) (fromIntegral second) (fromIntegral size)

foreign import ccall unsafe "sfFont_getKerning"
    sfFont_getKerning :: Font -> Word32 -> Word32 -> CUInt -> IO CInt

--CSFML_GRAPHICS_API int sfFont_getKerning(sfFont* font, sfUint32 first, sfUint32 second, unsigned int characterSize);


-- | Get the line spacing value.
getLineSpacing
    :: Font -- ^ Source font
    -> Int    -- ^ Character size, in pixels
    -> IO Int

getLineSpacing font size = fmap fromIntegral $ sfFont_getLineSpacing font (fromIntegral size)

foreign import ccall unsafe "sfFont_getLineSpacing"
    sfFont_getLineSpacing :: Font -> CUInt -> IO CInt

--CSFML_GRAPHICS_API int sfFont_getLineSpacing(sfFont* font, unsigned int characterSize);


-- | Get the texture containing the glyphs of a given size in a font.
getFontTexture
    :: Font -- ^ Source font
    -> Int    -- ^ Character size, in pixels
    -> IO Texture

getFontTexture font size = sfFont_getTexture font (fromIntegral size)

foreign import ccall unsafe "sfFont_getTexture"
    sfFont_getTexture :: Font -> CUInt -> IO Texture

--CSFML_GRAPHICS_API const sfTexture* sfFont_getTexture(sfFont* font, unsigned int characterSize);

