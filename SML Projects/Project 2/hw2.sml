(* CSE 341, HW2 Provided Code *)

(* main datatype definition we will use throughout the assignment *)
datatype json =
         Num of real (* real is what SML calls floating point numbers *)
       | String of string
       | False
       | True
       | Null
       | Array of json list
       | Object of (string * json) list

(* some examples of values of type json *)
val json_pi    = Num 3.14159
val json_hello = String "hello"
val json_false = False
val json_array = Array [Num 1.0, String "world", Null]
val json_obj   = Object [("foo", json_pi), ("bar", json_array), ("ok", True)]

(* some provided one-liners that use the standard library and/or some features
   we have not learned yet. (Only) the challenge problem will need more
   standard-library functions. *)

(* dedup : string list -> string list -- it removes duplicates *)
fun dedup xs = ListMergeSort.uniqueSort String.compare xs

(* strcmp : string * string -> order compares strings alphabetically
   where datatype order = LESS | EQUAL | GREATER *)
val strcmp = String.compare

(* convert an int to a real *)
val int_to_real = Real.fromInt

(* absolute value of a real *)
val real_abs = Real.abs

(* convert a real to a string *)
val real_to_string = Real.toString

(* return true if a real is negative : real -> bool *)
val real_is_negative = Real.signBit

(* We now load 3 files with police data represented as values of type json.
   Each file binds one variable: small_incident_reports (10 reports),
   medium_incident_reports (100 reports), and large_incident_reports
   (1000 reports) respectively.

   However, the large file is commented out for now because it will take
   about 15 seconds to load, which is too long while you are debugging
   earlier problems.  In string format, we have ~10000 records -- if you
   do the challenge problem, you will be able to read in all 10000 quickly --
   it's the "trick" of giving you large SML values that is slow.
*)

(* Make SML print a little less while we load a bunch of data. *)
       ; (* this semicolon is important -- it ends the previous binding *)
Control.Print.printDepth := 3;
Control.Print.printLength := 3;
       
use "parsed_small_police.sml";
use "parsed_medium_police.sml";

(* uncomment when you are ready to do the problems needing the large report*)
use "parsed_large_police.sml";

val large_incident_reports_list =
    case large_incident_reports of
        Array js => js
      | _ => raise (Fail "expected large_incident_reports to be an array") 

(* Now make SML print more again so that we can see what we're working with. *)
; Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 1-8 HERE ****)

(* Pre : Takes an int value i, value is a positive number.
   Post: Returns a JSON array of JSON objects where every object
         has "b" and "n" fields and each corresponding to a 
         True and real value. The first object "n" holding i.0,
         second object "n" holding ((i - 1). 0), and so on. 
         The last object in the list should have "n" holding 1.0. *)

fun make_silly_json i =
    let
	fun helper i =
	    case i of
		0 => []
	      | i => Object[("n", Num(int_to_real i)), ("b", True)]
		     :: helper(i - 1)
    in
	Array(helper(i))
    end

(* Pre : Takes two arguments k and xs.
   Post: Returns SOME v1 if (k1, v1) is the pair in the list closest
         to the beginning of the list for which k and k1 are equal.
         Returns NONE is no such pair is contained. *)
	  
fun assoc (k, xs) =
    case xs of
	[] => NONE
      | (k1, v1) :: xs' => if k = k1
			   then SOME v1
			   else assoc(k, xs')

(* Pre : Takes a json, and a string value.
   Post: Returns a json option if the passed json is an object
         and has a field name with the given string value. Returns
         SOME v where v is the contents of that field. Returns NONE
         if j is not an object or does not contain a field with given
         string value. *)

fun dot (j, f) =
    case j of
	Object check => assoc(f, check)
      | _ => NONE

(* Pre : Takes a json.
   Post: Returns a string list if the given json is an object,
         return a list holding all of its field names. Else, return
         the empty list. *)
		 
fun one_fields j =
    let
	fun helper (list, result) =
	    case list of
		[] => result
	      | (name, value) :: list' => helper(list', name :: result)
    in
	case j of
	    Object check => helper(check, [])
	  | _ => []
    end

(* Pre : Takes a string list.
   Post: Returns true if and only if no string appears more than
         once in the input list; false otherwise. *)
	
fun no_repeats list =
    length(dedup(list)) = length(list)

(* Pre : Takes a json value.
   Post: Returns true if and only if no object anywhere INSIDE the
         json argument has repeated field names. For other type of 
         json values such as False, True, Null, etc. should returns
         true, also for empty list, it will also returns true. Otherwise,
         returns false. *)
				
fun recursive_no_field_repeats j =
    let
	fun find_obj j =
	    case j of
		Array(first :: rest) => find_obj(first) @ find_obj(Array rest)
	      | Object((x, y) :: rest) => Object((x, y) :: rest) :: find_obj(y)
					  @ find_obj(Object rest)
	      | _ => []
	fun no_duplicates obj =
	    case obj of
		[] => true
	      | first :: rest => no_repeats(one_fields(first)) andalso
				 no_duplicates(rest)
    in
	no_duplicates(find_obj(j))
    end
	
(* Pre : Takes a string list and an exception.
   Post: Returns a list where each string is paired with the number of 
         times it occurs if the given list is sorted. Raise the given
         exception if not sorted. *)
	
fun count_occurrences (list, exn) =
    let
	fun helper (list', current, count, result) =
	    case list' of
		[] => (current, count) :: result
	      | value :: rest =>
		case strcmp(current, value) of
		    GREATER => raise exn
		  | LESS => helper(rest, value, 1, (current, count) :: result)
		  | EQUAL => helper(rest, value, (count + 1), result)	    
    in
	case list of
	    [] => []
	  | first :: list' => helper(list', first, 1, [])
    end
	
(* Pre : Takes a string and a json list.
   Post: Returns a string list with strings where for any
         object in the json list has a field equal to the given
         string value and has contents of a JSON string, put the
         contents string in the output list. *)
	
fun string_values_for_field (str, jList) =
    case jList of
	[] => []
      | j :: j' => case dot(j, str) of
		       SOME (String value) => value ::
					      string_values_for_field(str, j')
		     | _ => string_values_for_field(str, j') 
	
(* histogram and historgram_for_field are provided, but they use your
   count_occurrences and string_values_for_field, so uncomment them
   after doing earlier problems *)

(* histogram_for_field takes a field name f and a list of objects js and
   returns counts for how often a string is the contents of f in js. *)
						   
exception SortIsBroken

fun histogram (xs : string list) : (string * int) list =
  let
    fun compare_strings (s1 : string, s2 : string) : bool = s1 > s2

    val sorted_xs = ListMergeSort.sort compare_strings xs
    val counts = count_occurrences (sorted_xs,SortIsBroken)

    fun compare_counts ((s1 : string, n1 : int), (s2 : string, n2 : int)) : bool =
      n1 < n2 orelse (n1 = n2 andalso s1 < s2)
  in
    ListMergeSort.sort compare_counts counts
  end

fun histogram_for_field (f,js) =
  histogram (string_values_for_field (f, js))

(**** PUT PROBLEMS 9-11 HERE ****)

(* Pre : Takes two string value and one json list.
   Post: Returns a json list which is a subset of the given json list
         containing only elements of the input list that have a field with 
         name equal to the first given string and contents are a JSON string
         equal to the second string argument. *)
	    
fun filter_field_value (name, content, jList) =
    case jList of
	[] => []
      | j :: j' => case dot(j, name) of
		       SOME (String value) =>
		       if (value = content)
		       then j :: filter_field_value(name, content, j')
		       else filter_field_value(name, content, j')
		     | _ => filter_field_value(name, content, j')

(* This is a variable binds to a histogram of the
   objects in large_incident_reports_list based on the
   "event_clearance_description" field. *)
					      
val large_event_clearance_description_histogram =
    histogram_for_field("event_clearance_description",
			large_incident_reports_list) 

(* This is a variable binds to a histogram of the 
   objects in large_incident_reports_list based on the
   "hundred_block_location" field. *)
		       
val large_hundred_block_location_histogram =
    histogram_for_field("hundred_block_location",
			large_incident_reports_list)    
		       
;Control.Print.printDepth := 3;
Control.Print.printLength := 3; 

(**** PUT PROBLEMS 12-15 HERE ****)

(* This is a variable binds to a json list containing
   the elements of large_incident_reports_list whose 
   "hundred_block_location" field contains the JSON string
   "43XX BLOCK OF UNIVERSITY WAY NE". *)

val forty_third_and_the_ave_reports =
    filter_field_value("hundred_block_location",
		       "43XX BLOCK OF UNIVERSITY WAY NE",
		       large_incident_reports_list)

(* This is a variable binds a histogram based on the 
   "event_clearance_description" field, but considering only
   the objects whose "hundred_block_location" field contains 
   "43XX BLOCK OF UNIVERSITY WAY NE" *)
		      
val forty_third_and_the_ave_event_clearance_description_histogram =
    histogram_for_field("event_clearance_description",
			forty_third_and_the_ave_reports)

(* This is a variable binds a json list containing the elements
   of large_incident_reports_list whose "hundred_block_location" 
   field contains the JSON string "45XX BLOCK OF 19TH AVE NE". *)
		       
val nineteenth_and_forty_fifth_reports =
    filter_field_value("hundred_block_location",
		       "45XX BLOCK OF 19TH AVE NE",
		       large_incident_reports_list)

(* This is a variable binds a a histogram based on the 
   "event_clearance_description" field, but considering
   only the objects whose "hundred_block_location" field contains 
   "45XX BLOCK OF 19TH AVE NE" *)
		      
val nineteenth_and_forty_fifty_event_clearance_description_histogram =
    histogram_for_field("event_clearance_description",
			nineteenth_and_forty_fifth_reports)

;Control.Print.printDepth := 20;
Control.Print.printLength := 20; 

(**** PUT PROBLEMS 16-19 HERE ****) 

(* Pre : Takes a separator string and a list of strings.
   Post: Returns the string that consists of all the
         strings in the list concatenated together, and 
         separated by the separator. Separator should only
         between elements, not at the beginning or end. *)

fun concat_with (separator, list) =
    case list of
	[] => ""
      | last :: [] => last
      | first :: rest => first ^ separator ^ concat_with(separator, rest)

(* Pre : Takes a string.
   Post: Returns a string that is the same except
         there is an additional " character at the 
         beginning and end. *)
							
fun quote_string str = "\"" ^ str ^ "\""

(* Pre : Takes a real. 
   Post: Returns a string of the real number.
         If the real number is negative, there
         should be a "-" in front of it. *)
					
fun real_to_string_for_json number =
    if real_is_negative(number)
    then "-" ^ real_to_string(real_abs(number))
    else real_to_string(number)

(* Pre : Takes a json.
   Post: Converts the json value to a string and
         returns that string value. *)
	       
fun json_to_string j =
    let
	fun helper_ary ary =
	    case ary of
		[] => []
	     |  ary :: ary' => json_to_string(ary) :: helper_ary(ary')
	fun helper_obj obj =
	    case obj of
		[] => []
	      | (str, value) :: obj' => quote_string(str) ^ " : " ^
					json_to_string(value) :: helper_obj(obj')
    in
	case j of
	    Num number => real_to_string_for_json(number)
	  | String str => quote_string(str)
	  | False => "false"
	  | True => "true"
	  | Null => "null"
	  | Array ary => "[" ^ concat_with(", ", helper_ary(ary))  ^ "]"
	  | Object obj => "{" ^  concat_with(", ", helper_obj(obj)) ^ "}"
    end
													
			

	
  						


