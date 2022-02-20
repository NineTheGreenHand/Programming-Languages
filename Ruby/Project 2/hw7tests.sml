(* The following tests are for preprocess_prog fun in the sml file. *)

use "hw7.sml";

val test1 = preprocess_prog(NoPoints)
val test2 = preprocess_prog(Point (1.0, 2.0))
val test3 = preprocess_prog(Line (3.0, 2.0))
val test4 = preprocess_prog(VerticalLine 1.0) 
val test5 = preprocess_prog(Var "x") 
val test6 = preprocess_prog(Let ("x",Point(1.0,2.0), Point(1.0,2.0))) 
val test7 = preprocess_prog(LineSegment (3.2, 4.1, 3.2, 4.1))
val test8 = preprocess_prog(LineSegment (3.1, 4.3, 3.2, 4.1))
val test9 = preprocess_prog(LineSegment (3.3, 4.0, 3.3, 4.1))
val test10 = preprocess_prog(Intersect (LineSegment (3.1, 4.1, 3.1, 4.3), Point (3.1, 4.1)))

(* For the 10 tests, each test should corresponding to the following result. *)
(* test1 = NoPoints
   test2 = Point (1.0,2.0)
   test3 = Line (3.0,2.0)
   test4 = VerticalLine 1.0
   test5 = Var "x"
   test6 = Let ("x",Point (1.0,2.0),Point (1.0,2.0))
   test7 = Point (3.2,4.1)
   test8 = LineSegment (3.2,4.1,3.1,4.3)
   test9 = LineSegment (3.3, 4.1, 3.3, 4.0)
   test10 = Intersect (LineSegment (3.1,4.3,3.1,4.1),Point (3.1,4.1)) *)
