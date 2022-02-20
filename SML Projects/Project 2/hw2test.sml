(* This is the test file for hw2.sml. *)
(* All tests should return true. *)

use "hw2.sml";

val epsilon = 0.0001
fun check_real (r1,r2) = Real.abs (r1 - r2) < epsilon

val test1_1 =
    case make_silly_json 1 of
	Array [Object [("n",Num x),("b",True)]] => check_real(x, 1.0)
      | _ => false
val test1_2 =
    case make_silly_json 2 of
        Array [Object [("n",Num x),("b",True)], Object [("n",Num y), ("b",True)]]
        => check_real (x,2.0) andalso check_real(y,1.0)
      | _ => false

val test2_1 = assoc("hi", [("hello", "i"), ("lol", "haha"), ("hi", "world")])
		   = SOME ("world")
val test2_2 = assoc("hi", [("hello", "i"), ("lol", "haha"), ("ha", "world")])
		   = NONE
val test2_3 = assoc("", [("i", 2), ("", 9)]) = SOME 9

val test3_1 = case dot(Object[("foo", Null), ("hi", String "hello")], "foo") of
		  SOME (Null) => true
		| _ => false
val test3_2 = case dot(Num 1.0, "ok") of
		  NONE => true
		| _ => false

val test4_1 = one_fields(Object[("foo", Null), ("hi", String "hello")]) = ["hi", "foo"]
val test4_2 = one_fields(Null) = []
val test4_3 = one_fields(Object[("hello", json_obj), ("hi", Num 1.0)]) = ["hi", "hello"]

val test5_1 = no_repeats(["hi", "hello", "ok"]) = true
val test5_2 = no_repeats([]) = true
val test5_3 = no_repeats(["hi", "hi"]) = false

val test6_1 = recursive_no_field_repeats(True) = true
val test6_2 = recursive_no_field_repeats(Num 1.0) = true
val test6_3 =  recursive_no_field_repeats(Array[Object[("hello", Object[("hello", Num 1.0)])], Num 1.0, Null])
	       = true
val test6_4 = recursive_no_field_repeats(Object[("hello", String "hi"), ("hello", Num 1.0)]) = false

exception MyOne;

val test7_1 = count_occurrences(["hello", "world", "hello", "world"], MyOne) = []  handle (MyOne) => true
val test7_2 = count_occurrences(["a", "a", "a"], MyOne) = [("a", 3)]
val test7_3 = count_occurrences([], MyOne) = []

val test8_1 = string_values_for_field("hello",[True, Num 1.0, Null]) = []
val test8_2 = string_values_for_field("x", [Object[]]) = []
val test8_3 = string_values_for_field("lol", [Object[("lol", Num 1.0), ("what?", Null)],
					      Object[("lol", String "Zed"), ("ok", String "Q")],
					      Object[("Meow", String "Pa"), ("lol", String "No.1")]])
	      = ["Zed", "No.1"]

val test9_1 = case filter_field_value("hello", "world", [True, Num 1.0, Null]) of
		  [] => true
		| _ => false
val test9_2 = case filter_field_value("x", "y", [Object[]]) of
		  [] => true
		| _ => false
val test9_3 = case filter_field_value("lol", "Zed",  [Object[("lol", Num 1.0), ("what?", Null)],
					      Object[("lol", String "Zed"), ("ok", String "Q")],
					      Object[("Meow", String "Pa"), ("lol", String "No.1")]]) of
		  [Object[("lol", String "Zed"), ("ok", String "Q")]] => true
		| _ => false

(* No test case for #10-15 since those are just variable bindings. *)

val test16_1 = concat_with("|", ["hello", "world"]) = "hello|world"
val test16_2 = concat_with("^^", ["Meow", "Meow"]) = "Meow^^Meow"
val test16_3 = concat_with("v", ["O", "O"]) = "OvO"
val test16_4 = concat_with(".", []) = ""


val test17_1 = quote_string("hello") = "\"hello\""

val test18_1 = real_to_string_for_json(~7.0) = "-7.0"
val test18_2 = real_to_string_for_json(9.0) = "9.0"
						  
val test19_1 = json_to_string(Array[]) = "[]"
val test19_2 = json_to_string(Object[]) = "{}"
val test19_3 = json_to_string(Array[Num ~7.0, String "hi", Object[("lol", String "Zed")], True, False, Null])
	       = "[-7.0, \"hi\", {\"lol\" : \"Zed\"}, true, false, null]"
		     











	

			   



