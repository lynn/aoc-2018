# https://adventofcode.com/2018/day/11 in Emily (http://emilylang.org/).
\version 0.2

# Some preliminary definitions:
inf = 1/0
abs ^n = n < 0 ? ~n : n
fixup ^n = abs n < 0.00000000001 ? 0 : n
floor ^n = fixup (n - n % 1)                   # floor 3.14 = 3
hundreds ^n = floor (n % 1000 / 100)   # hundreds 98765.432 = 7

for ^lo ^hi ^f = { i = lo; while ^(i <= hi) ^(f i; i = i + 1) }

# Input: grid serial number.
gsn = 1309

# Power of a fuel cell.
power ^x ^y = {
    rackID = x + 10
    hundreds ((rackID * y + gsn) * rackID) - 5
}

# Prototype for a memoized 2D grid of numbers.
# (Emily doesn't have tuples, so we store (x:3, y:4) as key 3004.)
grid = [
    store ^x ^y ^v = { this.let (x*1000+y) v; v }
    fetch ^x ^y = this.has (x*1000+y) ? this (x*1000+y) : this.store x y (this.compute x y)
]

# A grid of fuel cell power values.
powerGrid = [
    parent = grid
    compute ^x ^y = power x y
]

# rectGrid(x)(y) equals Σi=1..x Σj=1..y powerGrid(i)(j).
#
# This helps us sum rectangles on the grid efficiently. Suppose
#
#                                             i   j
#                                             |   |
#                                     ⎡ A A A A B B ···
#                                     ⎢ A A A A B B
#                   powerGrid  =  k ——⎢ A A A A B B
#                                     ⎢ C C C C D D
#                                 l ——⎢ C C C C D D
#                                     ⎢ .           .
#                                     ⎢ :            ·
#
# And we wish to sum the "D" values. Then
#
#                             rectGrid(i,k) = A
#                             rectGrid(j,k) = A+B
#                             rectGrid(i,l) = A+C
#                             rectGrid(j,l) = A+B+C+D
#
# So that rectGrid(j,l) − rectGrid(i,l) − rectGrid(j,k) + rectGrid(i,k) = D.
# This is the formula used by `square s x y` to sum a square of fuel powers.
#
# We use the same principle (with D 1×1) to compute rectGrid's values:
#
rectGrid = [
    parent = grid
    compute ^x ^y = {
        (x == 0 || y == 0) ? 0 : {
            a = this.fetch
            a(x)(y-1) + a(x-1)(y) - a(x-1)(y-1) + powerGrid.fetch(x)(y)
        }
    }
]

# Sum an s×s square on the grid, with top-left corner at (x,y).
square ^s ^x ^y = {
    a = rectGrid.fetch
    a(x+s-1)(y+s-1) - a(x-1)(y+s-1) - a(x+s-1)(y-1) + a(x-1)(y-1)
}

# Find the maximum square in a grid, subject to constraints on (size, x, y).
maximize ^slo ^shi ^xlo ^xhi ^ylo ^yhi = {
    best = [ value = ~inf ]
    for slo shi ^s (
        for xlo (xhi-s+1) ^x (
            for ylo (yhi-s+1) ^y (
                value = square s x y
                if (value > best.value) ^(
                    best = [ x = x; y = y; size = s; value = value ]
                )
            )
        )
    )
    best
}

answer = (maximize 3 3    1 300  1 300)
print (answer.x) "," (answer.y) ln

answer = (maximize 1 300  1 300  1 300)
print (answer.x) "," (answer.y) "," (answer.size) ln
