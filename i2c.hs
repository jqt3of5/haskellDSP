import System.Posix.IO
import System.Posix.IOCtl
import System.Posix.Types
import Data.Char

i2c_init = 
  do 
    fd <- openFd "data.dat" ReadWrite Nothing defaultFileFlags
--    ioctl fd 
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
  fd <- i2c_init
  bytes <- i2c_read fd 0x41 45
  print bytes