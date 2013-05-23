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

-- mapOn range f list = mapOn' 0 range f list
--       where mapOn' _ [] _ ls = ls
--       	    mapOn' _ _ _ [] = []
--             mapOn' i (r:rs) f (l:ls)
-- 	    	   | i == r = (f l) : (mapOn' (i+1) rs f ls)
-- 	    	   | otherwise = l : (mapOn' (i+1) (r:rs) f ls)

class (Num a) => ConduitDSP a where
  lowPass :: a -> Conduit [a] IO [a]
  highPass :: a -> Conduit [a] IO [a]
  integrate :: a -> Conduit [a] IO [a]
  
instance (Num a) => Num [a] where
  (+) a b = Prelude.map (\(x,y) -> x+y) $ Prelude.zip a b
  (*) a b = Prelude.map (\(x,y) -> x*y) $ Prelude.zip a b
  (-) a b = Prelude.map (\(x,y) -> x*y) $ Prelude.zip a b
  abs a = Prelude.map abs a
  --signum
  --fromInteger
  
instance ConduitDSP Float where
  lowPass cf = do
    y <- await
    x <- await 
    case (x, y) of
      (Just x1, Just y1) -> 
        let ny = Prelude.map (a0*) x1 + Prelude.map (b1*) y1
        in do
          leftover ny
          yield ny
          lowPass cf
      _ -> return ()
    where x = exp $ -2*pi*cf
    	  a0 = 1-x
	  b1 = x

  highPass cf = do
    y <- await
    x0 <- await
    x1 <- await
    case (x0,x1,y) of
      (Just xi0,Just xi1, Just yi) ->
        let ny = Prelude.map (a0*) xi0 + Prelude.map (a1*) xi1 + Prelude.map (b1*) yi
        in do 
	  leftover xi1
          leftover ny
          yield ny
          highPass cf
      _ -> return ()
    where a0 = (1+x)/2
    	  a1 = -(1+x)/2
	  b1 = x
	  x = exp $ -2*pi*cf
  
  integrate dt = do
    y <- await
    x <- await
    case (x,y) of
      (Just xi, Just yi) ->
        let yy = yi + Prelude.map (dt*) xi
        in do
          leftover yy
          yield yy
          integrate dt
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
  chanGyro <- atomically $ newTBMChan 1000
  chanGrav <- atomically $ newTBMChan 1000
  chanAcc <- atomically $ newTBMChan 1000
--  forkIO $ sourceList [[1.0,1.0,1.0,1.0] | x <- [1..10]] $$ sinkTBMChan chanG
--  forkIO $ sourceList [[2.0,2.0,2.0,2.0] | x <- [1..10]] $$ sinkTBMChan chanA
--  forkIO $ sourceHandle hGyro $= bsToList =$= scale 250 =$= highPass 0.495 =$= integrate 1 =$= integrate 1 $$ sinkTBMChan chanGyro -- integrated gyro
  forkIO $ sourceHandle hAcc $= bsToList =$= scale 2 =$= antiGravFilter 0.05 $$ sinkTBMChan chanAcc -- main 
--  forkIO $ sourceHandle hAcc $= bsToList =$= scale 2  $$ sinkTBMChan chanGrav --gravity
  runResourceT $ sourceTBMChan chanAcc $$ outSink
--  mergedSource <- runResourceT $ sourceTBMChan chanGrav >=< sourceTBMChan chanAcc
--  runResourceT $ mergedSource $$ outSink
   
antiGravFilter :: Float -> Conduit [Float] IO [Float]
antiGravFilter cf = do
  a <- await
  b <- await
  case (a,b) of
    (Just y, Just x) -> 
      let ny = map (alpha*) (x-y) 
      in do 
        leftover ny
        yield ny
        antiGravFilter cf
    _ -> return ()
  where alpha = exp $ -2*pi*cf
            
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
    
