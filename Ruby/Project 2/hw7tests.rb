require "./hw7.rb"
# This test file syntax is similar with the provided test.
# Print out nothing is all tests are passed.

# Intersect cases
a = Point.new(1.0, 2.0)
b = Line.new(1.0, 2.0)
c = VerticalLine.new(1.0)
d = LineSegment.new(1.0, 2.0, 3.0, 1.0)
d1 = d.preprocess_prog

if not (d1.x1 == 3.0 and d1.y1 == 1.0 and d1.x2 == 1.0 and d1.y2 == 2.0)
  puts "LineSegment preprocess_prog error"
end
# Intersect tests
a1 = a.intersect a
if not (a1.x == 1.0 and a1.y == 2.0)
  puts "Point - Point intersect error"
end
a2 = a.intersect b
if not (a2.is_a? NoPoints)
  puts "Point - Line intersect error"
end
a3 = a.intersect c
if not ((a3.is_a? Point) and (a3.x == 1.0 and a3.y == 2.0))
  puts "Point - VerticalLine intersect error"
end
a4 = a.intersect d1
if not ((a4.is_a? Point) and (a4.x == 1.0 and a4.y == 2.0))
  puts "Point - LineSegment intersect error"
end
b1 = b.intersect b
b2 = b.intersect Line.new(1.0, 3.0)
b3 = b.intersect Line.new(2.0, 2.0)
if not ((b1.is_a? Line) and (b2.is_a? NoPoints) and (b3.is_a? Point) and
        (b1.m == 1.0 and b1.b == 2.0) and (b3.x == 0.0 and b3.y == 2.0))
  puts "Line - Line intersect error"
end
b4 = b.intersect c
b5 = b.intersect VerticalLine.new(2.0)
if not ((b4.is_a? Point) and (b5.is_a? Point) and (b4.x == 1.0 and b4.y == 3.0 and b5.x == 2.0 and b5.y == 4.0))
  puts "Line - VerticalLine intersect error"
end
b6 = b.intersect d1
b7 = b.intersect LineSegment.new(3.0, 2.0, 0.0, 2.0)
if not ((b6.is_a? NoPoints) and (b7.is_a? Point) and (b7.x == 0.0 and b7.y == 2.0))
  puts "Line - LineSegment intersect error"
end
c1 = c.intersect c
c2 = c.intersect VerticalLine.new(2.0)
if not ((c1.is_a? VerticalLine) and (c2.is_a? NoPoints) and c1.x == 1.0)
  puts "VerticalLine - VerticalLine intersect error"
end
c3 = c.intersect d1
c4 = c.intersect LineSegment.new(2.0, 3.0, -2.0, -9.0)
if not ((c3.is_a? Point) and (c4.is_a? Point) and c3.x == 1.0 and c3.y == 2.0 and c4.x == 1.0 and c4.y == 0.0)
  puts "VerticalLine - LineSegment intersect error"
end
d2 = d1.intersect d1
d3 = d1.intersect LineSegment.new(3.0, 1.0, 2.0, 2.0)
d4 = d1.intersect LineSegment.new(5.0, 5.0, 1.0, 2.0)
d5 = d1.intersect LineSegment.new(5.0, 5.0, 3.0, 1.0)
if not ((d2.is_a? LineSegment) and (d2.x1 == 3.0 and d2.y1 == 1.0 and d2.x2 == 1.0 and d2.y2 == 2.0) and
        (d3.is_a? LineSegment) and (d3.x1 == 3.0 and d3.y1 == 1.0 and d3.x2 == 2.0 and d3.y2 == 2.0) and
        (d4.is_a? LineSegment) and (d4.x1 == 3.0 and d4.y1 == 1.0 and d4.x2 == 1.0 and d4.y2 == 2.0) and
        (d5.is_a? Point) and (d5.x == 3.0 and d5.y == 1.0))
  puts "LineSegment - LineSegment intersect error"
end
# Shift tests
as = a.shift(1.0, 1.0)
bs = b.shift(1.0, 1.0)
cs = c.shift(1.0, 1.0)
ds = d1.shift(1.0, 1.0)
if not (as.x == 2.0 and as.y == 3.0)
  puts "Point - Shift error"
end
if not (bs.m == 1.0 and bs.b == 2.0)
  puts "Line - Shift error"
end
if not (cs.x == 2.0)
  puts "VerticalLine - Shift error"
end
if not (ds.x1 == 4.0 and ds.y1 == 2.0 and ds.x2 == 2.0 and ds.y2 == 3.0)
  puts "LineSegment - Shift error"
end
# Let tests
m = Let.new("a", LineSegment.new(3.0, 2.0, -3.0, -2.0),Intersect.new(Var.new("a"), LineSegment.new(3.0, 2.0, -3.0, -2.0)))
n = m.preprocess_prog.eval_prog([])
if not (n.x1 == 3.0 and n.y1 == 2.0 and n.x2 == -3.0 and n.y2 == -2.0)
  puts "Let error"
end
# Var tests
v = Var.new("x")
v1 = v.eval_prog([["x", Point.new(1.0, 1.0)]])
if not ((v1.is_a? Point) and v1.x == 1.0 and v1.y == 1.0)
        puts "Var error"
end 
if not (v1.preprocess_prog == v1)
        puts "Var preprocess_prog should return self"
end





  


  
