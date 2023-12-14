import Codec.Picture
import Codec.Picture.Gif
import Data.Bits

-- Function to hide information in the last significant bit of a pixel component
hideInLSB :: Pixel8 -> Bool -> Pixel8
hideInLSB component bit = if bit then component `setBit` 0 else component `clearBit` 0

-- Function to modify the pixel data to hide information
modifyPixelData :: DynamicImage -> DynamicImage
modifyPixelData (ImageRGB8 img) = ImageRGB8 $ pixelMap f img
  where
    f (PixelRGB8 r g b) = PixelRGB8 (hideInLSB r bit1) (hideInLSB g bit2) (hideInLSB b bit3)
    -- Example: hide 3 bits of information in the last significant bit of each color channel
    (bit1, rest1) = bit1 `divMod` 2
    (bit2, rest2) = bit2 `divMod` 2
    (bit3, _) = bit3 `divMod` 2
modifyPixelData img = img  -- Handle other image types if needed

-- Read the GIF image
readAndModifyGif :: FilePath -> IO (Either String DynamicImage)
readAndModifyGif filePath = readGifImage filePath >>= \case
  Left err -> return $ Left err
  Right img -> return $ Right (modifyPixelData img)

-- Save the modified image back to a GIF file
saveModifiedGif :: FilePath -> DynamicImage -> IO ()
saveModifiedGif filePath img = writeGif filePath [img]