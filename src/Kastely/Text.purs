module Kastely.Text where

import Prelude
import Data.Word
import Data.Char (fromCharCode)
import Data.Integral (fromIntegral)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

toChar :: Word8 -> Char
toChar b | b <= fromIntegral 0x1a = unsafePartial $ fromJust $ fromCharCode $ fromIntegral b + 0x60
         | b == fromIntegral 0x1b = 'á'
         | b == fromIntegral 0x1c = 'é'
         | b == fromIntegral 0x1d = 'í'
         | b == fromIntegral 0x1e = 'ó'
         | b == fromIntegral 0x1f = 'ü'
         | b == fromIntegral 0x23 = 'ő'
         | b == fromIntegral 0x24 = 'ö'
         | b == fromIntegral 0x25 = 'ú'
         | b == fromIntegral 0x26 = 'ű'
         | b == fromIntegral 0x21 = '!'
         | b == fromIntegral 0x22 = '\''
         | b == fromIntegral 0x27 = '''
         | b == fromIntegral 0x2c = ','
         | b == fromIntegral 0x2d = '-'
         | b == fromIntegral 0x2e = '.'
         | b == fromIntegral 0x2f = ' '
         | b == fromIntegral 0x3a = ':'
         | b == fromIntegral 0x3b = ';'
         | b == fromIntegral 0x20 = ' '
         | fromIntegral 0x30 <= b && b <= fromIntegral 0x39 = unsafePartial $ fromJust $ fromCharCode $ fromIntegral b
         | fromIntegral 0x40 < b && b < fromIntegral 0x80 = toChar (b - fromIntegral 0x40)
toChar b = '_'
