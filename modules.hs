{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
import Control.Concurrent hiding (yield)
import Control.Monad.STM
import Data.Conduit
import Data.Conduit.List as CL hiding(map)

import Data.Conduit.Binary as BN
import Data.Conduit.TMChan
import Data.Binary.Get
import Data.Word
import Data.Int

import Data.ByteString as BS hiding (putStrLn, map)
import Data.ByteString.Lazy as BSL hiding (putStrLn,map)
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import System.IO

import Debug.Trace 

class (Num a) => ConduitDSP a where
  lowPassC :: a -> Conduit [a] IO [a]
  highPassC :: a -> Conduit [a] IO [a]
  integrateC :: a -> Conduit [a] IO [a]
  
instance (Num a) => Num [a] where
  (+) a b = Prelude.map (\(x,y) -> x+y) $ Prelude.zip a b
  (*) a b = Prelude.map (\(x,y) -> x*y) $ Prelude.zip a b
  (-) a b = Prelude.map (\(x,y) -> x-y) $ Prelude.zip a b
  abs a = Prelude.map abs a
  --signum
  --fromInteger
  
integrate dt x y = y + Prelude.map (dt*) x

highPass cf x0 x1 y =  Prelude.map (a0*) x0 + Prelude.map (a1*) x1 + Prelude.map (b1*) y
  where a0 = (1+c)/c
        a1 = -(1+c)/c
        b1 = c
        c = exp $ -2*pi*cf
lowPass cf x y = Prelude.map (a0*) x + Prelude.map (b1*) y
  where c = exp $ -2*pi*cf
        a0 = 1-c
        b1 = c
        
instance ConduitDSP Float where
  lowPassC cf = do
    y <- await
    x <- await 
    case (x, y) of
      (Just x1, Just y1) -> 
        let ny = lowPass cf x1 y1
        in do
          leftover ny
          yield ny
          lowPassC cf
      _ -> return ()
                   
  highPassC cf = do
    y <- await
    x0 <- await
    x1 <- await
    case (x0,x1,y) of
      (Just xi0,Just xi1, Just yi) ->
        let ny = highPass cf xi0 xi1 yi
        in do 
	  leftover xi1
          leftover ny
          yield ny
          highPassC cf
      _ -> return ()
  

    
  integrateC dt = do
    y <- await
    x <- await
    case (x,y) of
      (Just xi, Just yi) ->
        let yy = integrate dt xi yi
        in do
          leftover yy
          yield yy
          integrateC dt
      _ -> return ()

antiGravConduit cf = do
  a <- await
  b <- await
  case (a,b) of
    (Just y, Just x) -> 
      let ny = lowPass cf x y
          gx = trace ("x: " ++ show x ++ " ny: " ++ show ny ++ " x-ny: " ++ show (x-ny)) $  x - ny
      in do 
        leftover ny
        yield gx
        antiGravConduit cf
    _ -> return ()

signalGenerator :: Conduit Int IO [Float]
signalGenerator = do
  i <- await
  case i of
    Just n -> do
     yield [sin $ c*(fromIntegral n)/1000 | c <- [1..10]]
     signalGenerator
    _ -> return ()

-- source :: Source (MaybeT IO) [Int]
-- source = sourceList [(0,0,0),(1,1,1),(1,1,1),(1,1,1),(1,1,1),(1,1,1)]
-- main = do
--    sourceList [0..1000] $= signalGenerator =$= highPass 0.01 $$ output

-- main =  do
--    handle <- openFile "6axis.bin" ReadMode
--    sourceHandle handle $= bsToList =$= scale 250 2 =$= lowPass 0.1  =$= integrate =$= integrate $$ output

main = do
  hGyro <- openFile "6axis.gyro" ReadMode
  hAcc <- openFile "6axis.acc" ReadMode
  --chanGyro <- atomically $ newTBMChan 1000
  --chanGrav <- atomically $ newTBMChan 1000
  --chanAcc <- atomically $ newTBMChan 1000
--  forkIO $ sourceHandle hGyro $= bsToList =$= scale 250 =$= highPass 0.495 =$= integrate 1 =$= integrate 1 $$ sinkTBMChan chanGyro -- integrated gyro
--  sourceHandle hAcc $= bsToList =$= scale 2  =$= antiGravConduit 0.01 =$= lowPassC 0.45 $$ basicPrint 
  --sourceHandle hAcc $= bsToList =$= scale 2  $$ basicPrint 
--  sourceHandle hAcc $= bsToList =$= scale 2  =$= lowPassC 0.01 $$ basicPrint 
  sourceHandle hAcc $= bsToList =$= scale 2  =$= antiGravConduit 0.01 =$= lowPassC 0.45 =$= integrateC 1 =$= integrateC 1 $$ basicPrint -- $$ sinkTBMChan chanGrav --gravity
  --runResourceT $ sourceTBMChan chanAcc $$ outSink
  --mergedSource <- runResourceT $ sourceTBMChan chanGrav >=< sourceTBMChan chanAcc
  --runResourceT $ mergedSource $$ outSink
  

basicPrint :: (Show a) => Sink [a] IO ()
basicPrint = do
  a <- await
  case a of
    Just str -> do 
      liftIO $ Prelude.mapM_ (Prelude.putStr.(++" ").show) str
      liftIO $ putStrLn ""
      basicPrint
    _ -> return ()

bsToList :: Conduit BS.ByteString IO [Int16]
bsToList = do 
  bs <- BN.take 12
  let x = runGet deserialize bs 
    in do 
      yield x
      bsToList

deserialize :: Get [Int16]
deserialize = do
  x <- getWord16le
  y <- getWord16le
  z <- getWord16le
  return [fromIntegral x,fromIntegral y,fromIntegral z]

scale :: Int -> Conduit [Int16] IO [Float]
scale sc = do
  i <- await
  case i of 
    Just x -> do    
      yield $ Prelude.map (ca*) $ Prelude.map fromIntegral x
      scale sc
  where ca = fromIntegral sc / fromIntegral 0x7FFF :: Float
       
          
outSink:: (Show a) => Sink [a] (ResourceT IO) ()
outSink= do
  a <- await
  case a of
    Just str -> do 
      liftIO $ Prelude.mapM_ (Prelude.putStr.(++" ").show) str
      liftIO $ putStrLn ""
      outSink
    _ -> return ()
    
