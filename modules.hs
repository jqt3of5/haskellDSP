import Data.Conduit
import Data.Conduit.List
type Reading = (Float, Float, Float)

source :: Source IO Reading
source = sourceList [(0,0,0),(1,1,1),(1,1,1),(1,1,1),(1,1,1),(1,1,1)]

sink :: Sink String IO ()
sink = Data.Conduit.List.mapM_ putStrLn

highPassFilter :: Float ->Float ->Float ->Float ->Float -> Conduit Reading IO Reading
highPassFilter a1 a2 b0 b1 b2= do
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
        highPassFilter a1 a2 b0 b1 b2
    _ -> return ()
  where nextDY a0 a1 a2 (ax,ay,az) (bx,by,bz) (cx,cy,cz) = (a0*ax + a1*bx + a2*cx, a0*ay + a1*by + a2*cy, a0*az + a1*bz + a2*cz) 
  
integrate :: Conduit Reading IO String
integrate = do
  y <- await
  x <- await
  case (x,y) of
    (Just xi, Just yi) ->
      let yy = nextY xi yi
      in do
        leftover yy
        yield $ show yy
        integrate
    _ -> return ()
  where nextY (ax,ay,az) (bx,by,bz) = (ax + bx, ay + by, az + bz)
