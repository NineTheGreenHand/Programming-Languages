(* This is the test file for the hw1.sml, when run in the REPL, all test should 
evaluates to true. *)

val test1_1 = is_older((4, 5, 2017), (21, 6, 2015)) = false
val test1_2 = is_older((4, 3, 2009), (13, 7, 2010)) = true
val test1_3 = is_older((12, 5, 2001), (12, 5, 2001)) = false

val test2_1 = number_in_month([(1, 2, 3), (2, 2, 3), (3, 3, 3)], 2) = 2
val test2_2 = number_in_month([], 3) = 0
val test2_3 = number_in_month([(4, 5, 2017), (12, 6, 2015), (12, 7, 2011), (1, 6, 2015)], 7) = 1

val test3_1 = number_in_months([(1, 2, 3), (2, 2, 3), (3, 3, 3)], [2, 3]) = 3
val test3_2 = number_in_months([], [1, 2, 3]) = 0
val test3_3 = number_in_months([(4, 5, 2017), (12, 6, 2015), (12, 7, 2011), (1, 6, 2015)], [5, 6, 8]) = 3

val test4_1 = dates_in_month([(1, 2, 3), (2, 2, 3), (3, 3, 3)], 2) = [(1, 2, 3), (2, 2, 3)]
val test4_2 = dates_in_month([], 3) = []
val test4_3 = dates_in_month([(4, 5, 2017), (12, 6, 2015), (12, 7, 2011),
			      (1, 6, 2015)], 7) = [(12, 7, 2011)]

val test5_1 = dates_in_months([(1, 2, 3), (2, 2, 3), (3, 3, 3)], [2, 3]) = [(1, 2, 3), (2, 2, 3), (3, 3, 3)]
val test5_2 = dates_in_months([], [1, 2, 3]) = []
val test5_3 = dates_in_months([(4, 5, 2017), (12, 8, 2015), (12, 7, 2011), (1, 6, 2015)], [5, 6, 8])
			     = [(4, 5, 2017), (1, 6, 2015), (12, 8, 2015)]
			     
val test6_1 = get_nth(["hello", "world"], 2) = "world"
val test6_2 = get_nth(["Do", "you", "want", "a", "piece", "of", "cake", "?"], 7) = "cake"
val test6_3 = get_nth(["1", "2", "3"], 3) = "3"

val test7_1 = date_to_string((15, 9, 2009)) = "September-15-2009"
val test7_2 = date_to_string((12, 1, 2019)) = "January-12-2019"
val test7_3 = date_to_string((9, 7, 2018)) = "July-9-2018"

val test8_1 = number_before_reaching_sum(14, [1, 2, 6, 7, 8 ,12]) = 3
val test8_2 = number_before_reaching_sum(27, [88, 999]) = 0
val test8_3 = number_before_reaching_sum(99, [12, 22, 87]) = 2
 
val test9_1 = what_month(252) = 9 
val test9_2 = what_month(15) = 1
val test9_3 = what_month(68) = 3

val test10_1 = month_range(19, 2) = []
val test10_2 = month_range(56, 60) = [2, 2, 2, 2, 3]
val test10_3 = month_range(1, 1) = [1]

val test11_1 = oldest([(1, 2, 3)]) = SOME (1, 2, 3)
val test11_2 = oldest([(4, 5, 2017), (12, 6, 2015), (12, 7, 2011), (1, 6, 2015)]) = SOME (12, 7, 2011)
val test11_3 = oldest([]) = NONE

val test12_1 = cumulative_sum([9]) = [9]
val test12_2 = cumulative_sum([1, 99, 100, 200, 400, 100]) = [1, 100, 200, 400, 800, 900]
val test12_3 = cumulative_sum([]) = []

val test13_1 = number_in_months_challenge([(1, 2, 3), (2, 2, 3), (3, 3, 3)], [2, 3, 2, 3]) = 3
val test13_2 = number_in_months_challenge([], [1, 1, 1, 1]) = 0	
val test13_3 = number_in_months_challenge([(4, 5, 2017), (12, 6, 2015),
					   (12, 7, 2011), (1, 6, 2015)], [5, 6, 8, 5, 5, 6]) = 3

val test14_1 = dates_in_months_challenge([], [1, 2, 3]) = []
val test14_2 = dates_in_months_challenge([(1, 2, 3), (2, 2, 3), (3, 3, 3)], [2, 3, 3, 2]) = [(3, 3, 3),
											     (1, 2, 3),
											     (2, 2, 3)]
val test14_3 = dates_in_months_challenge([(4, 5, 2017), (12, 8, 2015), (12, 7, 2011), (1, 6, 2015)],
					 [5, 6, 7, 7, 6, 5, 7])
					= [(1, 6, 2015), (4, 5, 2017), (12, 7, 2011)]

val test15_1 = reasonable_date((0, 0, 0)) = false
val test15_2 = reasonable_date((29, 2, 2019)) = false
val test15_3 = reasonable_date((29, 2, 2020)) = true
val test15_4 = reasonable_date((35, 1, 2019)) = false
val test15_5 = reasonable_date((31, 4, 2019)) = false
val test15_6 = reasonable_date((12, 15, 2019)) = false
val test15_7 = reasonable_date((12, 1, 2019)) = true


					


