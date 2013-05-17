{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

import Data.Conduit
import Data.Conduit.List as CL
import Data.Conduit.Binary as BN
import Data.Binary.Get
import Data.Word
import Data.Int

import Data.ByteString as BS hiding (putStrLn)
import Data.ByteString.Lazy as BSL hiding (putStrLn)
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import System.IO

class (Num a) => ConduitDSP a where
  lowPass :: a -> Conduit [a] IO [a]
  highPass :: a -> a -> a -> a -> a -> Conduit [a] IO [a]
  integrate :: Conduit [a] IO [a]
  
instance (Num a) => Num [a] where
  (+) a b = Prelude.map (\(x,y) -> x+y) $ Prelude.zip a b
  (*) a b = Prelude.map (\(x,y) -> x*y) $ Prelude.zip a b
  (-) a b = Prelude.map (\(x,y) -> x*y) $ Prelude.zip a b
  abs a = Prelude.map abs a
  --signum
  --fromInteger
  
instance ConduitDSP Float where
  lowPass a1 = do
    y <- await
    x <- await 
    case (x, y) of
      (Just x1, Just y1) -> 
        let ny = y1 + Prelude.map (\n -> n*a1) (x1 - y1)
        in do
          leftover ny
          yield ny
          lowPass a1
      _ -> return ()

  highPass a1 a2 b0 b1 b2 = do
    d1 <- await
    d2 <- await
    x <- await
    case (x,d1,d2) of
      (Just xi, Just d1i, Just d2i) -> 
        let di = nextDY 1 a1 a2 xi d1i d2i
            y = nextDY b0 b1 b2 di d1i d2i
        in do 
          leftover d1i
          leftover di
          yield y
          highPass a1 a2 b0 b1 b2
      _ -> return ()
    where nextDY a0 a1 a2 a b c = Prelude.map (\(x,y,z) -> a0*x + a1*y +a2*z) $ zip3 a b c
  
  integrate = do
    y <- await
    x <- await
    case (x,y) of
      (Just xi, Just yi) ->
        let yy = nextY xi yi
        in do
          leftover yy
          yield yy
          integrate
      _ -> return ()
    where nextY a b = Prelude.map (\(x,y) -> x+y) $ Prelude.zip a b

-- source :: Source (MaybeT IO) [Int]
-- source = sourceList [(0,0,0),(1,1,1),(1,1,1),(1,1,1),(1,1,1),(1,1,1)]

main =  do
  handle <- openFile "6axis.bin" ReadMode
  sourceHandle handle $= bsToList =$= scale 250 2 =$= lowPass 0.8  $$ output

bsToList :: Conduit BS.ByteString IO [Int16]
bsToList = do 
  bs <- BN.take 12
  let x = runGet deserialize bs 
    in do 
      yield x
      bsToList

deserialize :: Get [Int16]
deserialize = do
  ax <- getWord16le
  ay <- getWord16le
  az <- getWord16le
  gx <- getWord16le
  gy <- getWord16le
  gz <- getWord16le
  return [fromIntegral ax,fromIntegral ay,fromIntegral az,fromIntegral gx,fromIntegral gy,fromIntegral gz]

scale :: Int -> Int -> Conduit [Int16] IO [Float]
scale g a = do
  i <- await
  case i of 
    Just x -> do    
      yield $ Prelude.map (\(a,b) -> a*b) $ Prelude.zip [ca,ca,ca,cg,cg,cg] $ Prelude.map fromIntegral x
      scale g a
  where ca = fromIntegral a / fromIntegral 0x7FFF :: Float
        cg = fromIntegral g / fromIntegral 0x7FFF  :: Float
      
output ::   Sink [Float] IO ()
output = do
  a <- await
  case a of
    Just str -> do 
      liftIO $ print str
      output
    _ -> return ()

sink :: Sink String IO ()
sink = CL.mapM_ putStrLn

