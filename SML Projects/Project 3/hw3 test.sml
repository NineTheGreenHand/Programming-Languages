(* All test should returns true. *)

use "hw3.sml";

val test1_1 = only_lowercase(["Hello", "hello", "ok", "OK", "I", "am"]) = ["hello", "ok", "am"]
val test1_2 = only_lowercase(["I"]) = []
val test1_3 = only_lowercase([]) = []
val test1_4 = only_lowercase(["hi"]) = ["hi"]
					   
val test2_1 = longest_string1(["i", "hello", "yayayayaya"]) = "yayayayaya"
val test2_2 = longest_string1(["hola", "HOLA", "MEOW"]) = "hola"
val test2_3 = longest_string1(["1"]) = "1"
val test2_4 = longest_string1([]) = ""
					
val test3_1 = longest_string2(["i", "hello", "yayayayaya"]) = "yayayayaya"
val test3_2 = longest_string2(["hola", "HOLA", "MEOW"]) = "MEOW"
val test3_3 = longest_string2(["1"]) = "1"
val test3_4 = longest_string2([]) = ""
					
(* For question 4, longest_string3 and longest_string4 should have same behavior of 
   function longest_string1 and longest_string2. *)

val test4_1 = longest_string3 ["i", "hello", "yayayayaya"] = "yayayayaya"
val test4_2 = longest_string3 ["hola", "HOLA", "MEOW"] = "hola"
val test4_3 = longest_string3 ["1"] = "1"
val test4_4 = longest_string3 [] = ""					
val test4_5 = longest_string4 ["i", "hello", "yayayayaya"] = "yayayayaya"
val test4_6 = longest_string4 ["hola", "HOLA", "MEOW"] = "MEOW"
val test4_7 = longest_string4 ["1"] = "1"
val test4_8 = longest_string4 [] = ""

val test5_1 = longest_lowercase(["hello", "helllllllllo", "HHHHHHHHHHHHHHHHHHHHHHHHHHH"]) = "helllllllllo"
val test5_2 = longest_lowercase(["Hello", "Helllllllllo", "HHHHHHHHHHHHHHHHHHHHHHHHHHH"]) = ""
val test5_3 = longest_lowercase(["hello", "Helllllllllo", "hELLO"]) = "hello"
val test5_4 = longest_lowercase([]) = ""
					  
val test6_1 = caps_no_X_string "aBxXXxDdx" = "ABDD"
val test6_2 = caps_no_X_string "XXXXXXXXXXxxxxXXX" = ""
val test6_3 = caps_no_X_string "HAHAhahaxxxXXXyuu" = "HAHAHAHAYUU"
val test6_4 = caps_no_X_string "12345XrrrXiX" = "12345RRRI"
val test6_5 = caps_no_X_string "" = ""

(* For question 7 and 8 there is not really anything I can test, but in terms of correctness, I 
   have all the correct type when ran in the REPL. *)

val test9_b1 = count_wildcards (ConstructorP("hello", TupleP[WildcardP, (VariableP "hello")])) = 1
val test9_b2 = count_wildcards (VariableP "hello") = 0
val test9_b3 = count_wildcards (TupleP[WildcardP, TupleP[WildcardP, WildcardP]]) = 3
val test9_b4 = count_wildcards WildcardP = 1
val test9_b5 = count_wildcards (ConstantP 17) = 0

val test9_c1 = count_wild_and_variable_lengths (ConstructorP("hello", TupleP[WildcardP, (VariableP "hello")])) = 6
val test9_c2 = count_wild_and_variable_lengths (VariableP "hello") = 5
val test9_c3 = count_wild_and_variable_lengths WildcardP = 1
val test9_c4 = count_wild_and_variable_lengths (TupleP[WildcardP, TupleP[WildcardP, WildcardP, ConstructorP("hi", TupleP[WildcardP, (VariableP "hi")])]]) = 6

val test9_d1 = count_a_var("hi", (ConstructorP("hi", TupleP[WildcardP, (VariableP "hello")]))) = 0
val test9_d2 = count_a_var("hi", WildcardP) = 0
val test9_d3 = count_a_var("hello", (VariableP "hello")) = 1
val test9_d4 = count_a_var("hi", (TupleP[WildcardP, TupleP[WildcardP, WildcardP, ConstructorP("hi", TupleP[WildcardP, (VariableP "hi")])]])) = 1

																		   

																	
																			    

						    

