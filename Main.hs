{-# LANGUAGE OverloadedStrings #-}

module Main where


import qualified Data.Map as Map
import           Data.Map (Map)
import 	         Data.Text (Text)
import			 System.Random.Shuffle
import			 System.Random
import           Graphics.Blank


data Shapes = Circle | Square | Triangle | Diamond | Pentagon | Octagon | Star | Trapezoid deriving Eq
data State = Hid | Exp deriving Show 

main :: IO()
main = blankCanvas 3000 { events = ["mousedown"] } $ \ context -> do 
    let shape_list = [Just Circle,Just Circle,Just Square,Just Square,Just Triangle,Just Triangle,
                      Just Diamond,Just Diamond,Just Pentagon,Just Pentagon,Just Octagon,
                      Just Octagon,Just Star,Just Star,Just Trapezoid,Just Trapezoid]
    let state_list = [Just Hid,Just Hid,Just Hid,Just Hid,Just Hid,Just Hid,Just Hid,
                      Just Hid,Just Hid,Just Hid,Just Hid,Just Hid,Just Hid,Just Hid,
                      Just Hid,Just Hid]
    shape_list <- sshuffle shape_list
    grid context shape_list state_list []

sshuffle list = do g <- newStdGen
                   return $ shuffle' list 16 g

grid :: DeviceContext  -> [Maybe Shapes] -> [Maybe State] -> [(Double,Double)]-> IO ()
grid context shapes states clicks = do 
	sz <- send context $ do 
		clearRect(0,0, width context, height context)
		beginPath()

		let sz = min (width context) (height context)
		save() 
		translate (width context / 2, height context / 2)
		let size = sz*0.20
		let first = -sz*0.46
		let second = -sz*0.22
		let third = sz*0.03
		let fourth = sz*0.28
		
		let size = sz * 0.2
		let coord_list = [(first,first),(first,second),(first,third),(first,fourth),
		                  (second,first),(second,second),(second,third),(second,fourth),
		                  (third,first),(third,second),(third,third),(third,fourth),
		                  (fourth,first),(fourth,second),(fourth,third),(fourth,fourth)]

		selectDraw (clickedIterator states shapes 0 (reverse (filter (/=16) (boxIterator coord_list clicks 0 sz  size [])))) shapes sz size coord_list 0

		restore() 
		return sz


        event <- wait context
        print event
        print clicks
        case ePageXY event of
        	Nothing -> grid context shapes states clicks 
        	Just (x',y') -> grid context shapes states ((x', y'):clicks)


clickedIterator :: [Maybe State] -> [Maybe Shapes] -> Int -> [Int] -> [Maybe State]
clickedIterator ml sl y [] = ml 
clickedIterator ml sl y (x:xs) = if (length (x:xs)) <= 2
									then smallClicks ml y (x:xs)
									else if odd (length (x:xs)) 
										then bigClicks (smallClicks ml y [x]) sl y xs
										else bigClicks (smallClicks ml y (take 2 (x:xs))) sl y (drop 2(x:xs))

smallClicks ::[Maybe State] -> Int -> [Int] -> [Maybe State]
smallClicks ml y [] = ml 
smallClicks ml y (x:xs) = smallClicks (clickedBox ml y x) y xs 

bigClicks ::[Maybe State] -> [Maybe Shapes] -> Int -> [Int] -> [Maybe State]
bigClicks ml sl y [] = ml
bigClicks ml sl y (x:xs) = if (sl!!x) == (sl!! (head xs))
							then bigClicks (mclickedBox (mclickedBox ml y x) y (head xs)) sl y (tail xs)
							else bigClicks ml sl y (tail xs) ---bigClicks ml sl y (tail xs)



clickedBox :: [Maybe State] -> Int -> Int-> [Maybe State]
clickedBox [] z y = []
clickedBox (x:xs) z y = case x of
					Just Hid -> if z == y 
									then Just Exp : clickedBox xs (z + 1) y
									else x : clickedBox xs (z + 1) y
					Just Exp -> x :  clickedBox xs (z + 1) y
					Nothing -> x : clickedBox xs (z + 1) y

mclickedBox :: [Maybe State] -> Int -> Int-> [Maybe State]
mclickedBox [] z y = []
mclickedBox (x:xs) z y = case x of
					Just Hid -> if z == y 
									then Just Exp : clickedBox xs (z + 1) y
									else x : clickedBox xs (z + 1) y
					Just Exp -> x :  clickedBox xs (z + 1) y
					Nothing -> x : clickedBox xs (z + 1) y



boxIterator :: [(Double,Double)] -> [(Double,Double)] -> Int -> Double -> Double -> [Int] -> [Int]
boxIterator cl [] z sz size bl = bl 
boxIterator cl ((a,b):xs) z sz size bl = boxIterator cl xs z sz size ((boxFinder cl ((a - 1.2*sz),(b - 0.65*sz)) z size):bl)  

boxFinder :: [(Double,Double)] -> (Double,Double) -> Int -> Double ->  Int
boxFinder [] (a,b) z size = 16 
boxFinder (x:xs) (a,b) z size = if boxExtender x (a,b) size
									then  z 
									else boxFinder xs (a,b) (z+1) size

boxExtender :: (Double,Double) -> (Double,Double) -> Double -> Bool 
boxExtender (x,y) (a,b) size = if (x >=  a) && (x <= ( a + size))
								then if (y >= b) && (y <= ( b + size))
							   		then True
							   		else False
							   	else False 

selectDraw :: [Maybe State] -> [Maybe Shapes] -> Double -> Double -> [(Double,Double)] -> Int -> Canvas()
selectDraw state shapes sz size coords z= do
	let (x,y) = coords !! z
	case state !! z of
		Just Hid ->  drawHider size x y
		Just Exp ->  do drawOutside size x y
		                drawElements (shapes !! z) size (x+(sz*0.02)) (y+(sz*0.02))
		Nothing -> return () 

	if z < 15
		then selectDraw state shapes sz size coords (z + 1) 
		else return () 
	--drawOutside size x y 


drawElements :: Maybe Shapes -> Double -> Double -> Double -> Canvas()
drawElements shape size x y = do 
	case shape of 
		Just Circle -> drawCircle (size*0.4)  x y
		Just Square -> drawSquare (size*0.4)  x y
		Just Triangle -> drawTriangle (size*0.4) x y
		Just Pentagon -> drawPentagon (size*0.4) x y
		Just Star -> drawStar (size*0.4) x y
		Just Octagon -> drawOctagon (size*0.4) x y
		Just Trapezoid -> drawTrapezoid (size*0.4) x y
		Just Diamond -> drawDiamond (size*0.4) x y
		Nothing -> return () 	 

--shape 1 - hiding square - black 
drawHider :: Double -> Double -> Double -> Canvas ()
drawHider size x y = do 
	globalAlpha 1
	beginPath()
	moveTo(x,y)
	lineTo(x + size, y)
	lineTo(x + size, y + size)
	lineTo(x, y + size)
	fillStyle "#0B615E"
	fill()

--outside square
drawOutside :: Double -> Double -> Double -> Canvas()
drawOutside size x y  = do
	globalAlpha 1
	beginPath()
	moveTo(x,y)
	lineTo(x + size, y)
	lineTo(x + size, y + size)
	lineTo(x, y + size)
	lineTo(x, y)
	fillStyle "white"
	fill()
	lineWidth 4
	strokeStyle "black"
	stroke()

drawMatched :: Double -> Double -> Double -> Canvas()
drawMatched size x y  = do
	globalAlpha 0.3
	beginPath()
	moveTo(x,y)
	lineTo(x + size, y)
	lineTo(x + size, y + size)
	lineTo(x, y + size)
	lineTo(x, y)
	fillStyle "grey"
	fill()
	lineWidth 4
	strokeStyle "black"
	stroke()


--shape 1 - orange
drawSquare :: Double -> Double -> Double -> Canvas()
drawSquare size x y = do
	beginPath()	
	moveTo(x,y)
	lineTo(x + size*2, y)
	lineTo(x + size*2, y + size*2)
	lineTo(x, y + size*2)	
	fillStyle "orange"
	fill()

-- shape 2 - pink 
drawCircle :: Double -> Double -> Double -> Canvas()
drawCircle rad x y = do
	beginPath()
	globalAlpha 0.5
	fillStyle "#FF0040"
	arc(x + rad, y + rad, rad, 0, pi*2, False)
	closePath()
	fill()

--shape 3 - light green
drawTriangle :: Double -> Double -> Double -> Canvas()
drawTriangle size x y= do
	beginPath()
	moveTo(x, y + size*2)
	lineTo(x + size,y)
	lineTo(x + size*2,y + size*2)
	closePath()
	fillStyle "green"
	fill() 

-- shape 4 - dark blue
drawDiamond :: Double -> Double -> Double -> Canvas()
drawDiamond size x y = do
	beginPath()
	moveTo(x + size,y)
	lineTo(x + (size * 2),y + size)
	lineTo(x + size, y + (size * 2))
	lineTo(x,y + size)
	closePath()
	fillStyle "#0B0B61"
	fill()

-- shape 5 - teal 
-- pentagon
drawPentagon :: Double -> Double -> Double -> Canvas()
drawPentagon size x y= do
	beginPath()
	moveTo(x + size,y)
	lineTo(x+(size*2),y + size)
	lineTo(x+(size*2),y+(size*2))
	lineTo(x,y+(size*2))
	lineTo(x,y + size)
	closePath()
	fillStyle "red"
	fill()

-- shape 6 - purple 
drawOctagon:: Double -> Double -> Double -> Canvas()
drawOctagon size x y= do
	beginPath()
	moveTo(x +(size*0.5),y)
	lineTo(x +(size*1.5),y)
	lineTo(x +(size*2),y +(size*0.5))
	lineTo(x +(size*2),y +(size*1.5))
	lineTo(x +(size*1.5),y +(size*2))
	lineTo(x +(size*0.5),y +(size*2))
	lineTo(x,y + (size*1.5))
	lineTo(x,y +(size*0.5))
	closePath()
	fillStyle "purple"
	fill()

-- shape 7 - yellow 
-- star
drawStar:: Double -> Double -> Double -> Canvas()
drawStar size x y = do
	beginPath()
	moveTo(x+size,y) --tip
	lineTo(x+(size*1.25),y+(size*0.5)) --to first corner
	lineTo(x+(size*2),y+(size*0.5)) -- to flat arm
	lineTo(x+(size*1.5),y+size) -- to second corner
	lineTo(x+(size*1.75),y+(size*1.75)) --to leg
	lineTo(x+size,y+size*1.35)
	lineTo(x+(size*0.2),y+(size*1.75)) --to second leg
	lineTo(x+(size*0.5),y+size) --to third corner
	lineTo(x,y+(size*0.5)) -- to 
	lineTo(x+(size*0.75),y+(size*0.5))
	closePath()
	fillStyle "yellow"
	fill()

--shape 8 - light blue
drawTrapezoid :: Double -> Double -> Double -> Canvas()
drawTrapezoid size x y = do
	beginPath()
	moveTo(x+size*0.5,y)
	lineTo(x+size*1.5,y)
	lineTo(x+2*size,y+size*2)
	lineTo(x,y+size*2)
	closePath()
	fillStyle "#01DFD7"
	fill()
