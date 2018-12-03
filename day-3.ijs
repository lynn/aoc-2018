NB. https://adventofcode.com/2018/day/3 in J
NB. Try it here: https://tio.run/#j  (paste the problem input in the "Input" field)

NB. Parse the input:
input =. stdin ''
junk =. I. input e. '#@,:x'
data =. , ".each cutopen ' ' junk} input

NB. Now data is a boxed list, and each entry is a 5-vector of numbers.
NB. Let's turn one of those into a 1000x1000 boolean matrix:
rect =. monad : 0
  'I X Y W H' =. y
  (-Y,X) |. 1000 1000 {. (H,W) $ 1
)

NB. Part A: How many squares covered by overlap?
NB. (Sum all the rects and count >1s.)
total =. +/ > rect each data
echo +/, total>1

NB. Part B: Which ID doesn't overlap anything?
NB. (Retrieve all the rects back from the total and check which is all 1s.)
check =. dyad : 0
  'I X Y W H' =. y
  I* */,1= (H,W) {. (Y,X) |. x
)
echo >./ >(total&check) each data
