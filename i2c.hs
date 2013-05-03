{-# LANGUAGE MultiParamTypeClasses #-}

import System.Posix.IO
import System.Posix.IOCtl
import System.Posix.Types
import Foreign.C
import Data.Char

data Ioctl = I2c_slave

instance IOControl Ioctl CInt where
  ioctlReq I2c_slave = 0x0703


--i2c_init :: Char -> IO System.Posix.Types.Fd
i2c_init addr = 
  do 
    fd <- openFd "data.dat" ReadWrite Nothing defaultFileFlags
    _ <- ioctl fd I2c_slave addr
    return fd

i2c_write :: System.Posix.Types.Fd -> Int -> [Char] -> IO ByteCount
i2c_write fd reg dat = 
  do
    fdWrite fd [chr reg]
    fdWrite fd dat
    
i2c_read :: System.Posix.Types.Fd -> Int -> ByteCount -> IO [Char]
i2c_read fd reg count 
  = do
    fdWrite fd [chr reg]
    (buf, bcount) <- fdRead fd count
    return buf
  
main = do
  fd <- i2c_init 0x68
  _ <- i2c_write fd 0x6B [chr 0]
  _ <- i2c_write fd 0x6C [chr 0]
  bytes <- i2c_read fd 0x3B 14
  print bytes