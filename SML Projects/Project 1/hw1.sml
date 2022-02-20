(* The assignment contains 12 questions with two extra challenge ones *)

(* This is a common variable used in both function what_month and
   reasonable_date. *)

val daysEachMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
			
(* Pre : Given two dates are reasonable.
   Post: Returns a bool to check if the first date is older or comes before 
   	 the second date, if so, return true, false otherwise. Also, return false 
	 if two dates are same. *)

fun is_older (firstDate : (int * int * int), secondDate : (int * int * int)) =
    if	#3 firstDate = #3 secondDate
    then
	if #2 firstDate = #2 secondDate
        then #1 firstDate < #1 secondDate
	else #2 firstDate < #2 secondDate
    else #3 firstDate < #3 secondDate

(* Pre : Takes one date list and one int value.
   Post: Returns the number of total dates in the list with 
   	 month of the given value. *)

fun number_in_month (dates : (int * int * int) list, month : int) =
    if	null dates
    then 0
    else
	if #2 (hd dates) = month
        then 1 + number_in_month(tl dates, month)
        else number_in_month(tl dates, month)

(* Pre : Takes one date list and and int list, given int list has no duplicates.
   Post: Returns the number of total dates in the list with month of all the 
   	 given values in the int list.  *)
			    
fun number_in_months (dates : (int * int * int) list, month : int list) =
    if null month
    then 0
    else number_in_month(dates, hd month) + number_in_months(dates, tl month)

(* Pre : Takes a date list and an int value.
   Post: Returns a list with all dates with month of the given value. *)
							    
fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else
	if #2 (hd dates) = month
        then hd dates :: dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)

(* Pre : Takes a date list and an int list, given int list has no duplicates.
   Post: Returns a list with all dates with month of all the given value 
   	 in the int list. *)
			   
fun dates_in_months (dates : (int * int * int) list, month : int list) =
    if null month
    then []
    else dates_in_month(dates, hd month) @ dates_in_months(dates, tl month)

(* Pre : Takes a string list and an int value n. n is a positive number. 
       	 If the given list is empty, it is okay to apply hd and tl to the empty
	 list. (from the spec) So assume given list is not empty. And int n is 
         no bigger than the length of the list.
   Post: Returns the nth element in the list.  *)
							  
fun get_nth (givenString : string list, n : int) =
    if n = 1
    then hd givenString
    else get_nth(tl givenString, n - 1)

(* Pre : Takes a date and it is reasonable.
   Post: Returns a string to represent the date. *)
		
fun date_to_string (givenDate : (int * int * int)) =
    let 
	val twelveMonths = ["January", "February", "March", "April", "May",
			    "June", "July", "August", "September", "October",
			    "November", "December"]
    in
	get_nth(twelveMonths, #2 givenDate) ^ "-" ^ Int.toString(#1 givenDate)
	^ "-" ^ Int.toString(#3 givenDate)
    end

(* Pre : Takes one int value and an int list. Int value is a positive number;
       	 given entire int list sums to more than the given int value.
   Post: Returns an int value n which the first n elements of the list add to 
   	 less than the given int value. *)
	
fun number_before_reaching_sum (sum : int, positiveNumbers : int list) =
    if sum <= 0
    then ~1
    else 1 + number_before_reaching_sum(sum - hd positiveNumbers,
					tl positiveNumbers)

(* Pre : Takes an int value of days in range between 1 and 365.
   Post: Returns an int value to show which month the give value is. *)
				       
fun what_month (day : int) =
    number_before_reaching_sum(day, daysEachMonth) + 1

(* Pre : Takes two int values. Both range from 1 to 365.
   Post: Returns an int list starting with month of the first value, 
   	 second with month of the (first value + 1), etc. with ending 
	 of month of the second value. 
	 Making the length of the list of (second value - first value + 1) 
	 or 0 if (first value) > (second value). *)
	
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

(* Pre : Takes an date list. 
   Post: Returns a date option with oldest date in the list.
   	 Evaluates NONE if the list has no dates. *)
					
fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else
	let
	    fun oldest_nonempty (dates : (int * int * int) list) =
		if null (tl dates)
		then hd dates
		else
		    let
			val oldestOne = oldest_nonempty(tl dates)
		    in
			if is_older(oldestOne, hd dates)
			then oldestOne
			else hd dates
		    end
	in
	    SOME (oldest_nonempty(dates))
	end

(* Pre : Takes an int list.
   Post: Returns a list of the partial sums of these numbers.
   	 For example: if passed [a, b, c], the return list 
	 should be [a, a + b, a + b + c]. *)
	    
fun cumulative_sum (number : int list) =
    let
	val cumulative = 0
        fun helper (cumulative : int, number : int list) =
	    if null number
	    then []
	    else
		if null (tl number)
	        then [cumulative + hd number]
	        else (hd number + cumulative) :: helper(cumulative + hd number,
							tl number)		     
    in
	helper(cumulative, number)
    end

(* The followings are the challenge problems of this assignment *)

(* Pre : Takes an int list.
   Post: Returns an int list with all duplicates removed from the given list. *)
	
fun removeDuplicates (month : int list) =
    if null month
    then []
    else
	let
	    fun isSame (month : int list, value : int) =
		if null month
		then false
		else
		    if hd month = value
		    then true
		    else isSame(tl month, value)		
	in
	    if isSame(tl month, hd month)
	    then removeDuplicates(tl month)
	    else hd month :: removeDuplicates(tl month)
	end
	    
(* Pre : Takes a date list and an int list.
   Post: Returns the number of total dates in the list with month of all 
   	 the given values in the int list. The int list may have repeated 
	 values but the function will ignore the duplicates. *)
	   
fun number_in_months_challenge (dates : (int * int * int) list,
				month : int list) =
    number_in_months(dates, removeDuplicates(month))

(* Pre : Takes a date list and an int list.
   Post: Returns a list with all dates in the list  with month of all 
   	 the given value in the int list. The int list may have repeated 
	 values but the function will ignore the duplicates. *)
		    
fun dates_in_months_challenge (dates : (int * int * int) list,
			       month : int list) =
    dates_in_months(dates, removeDuplicates(month))

(* Pre : Takes a date value.
   Post: Returns true if the given date is a valid one in the common era;
   	 false otherwise. Special case when it is a leap year is considered. *)
		   
fun reasonable_date (date : (int * int * int)) =
    if not (#2 date >= 1 andalso #2 date <= 12 andalso #3 date > 0
	    andalso #1 date >= 1)
    then false
    else
	let
	    fun nth_int (daysEachMonth : int list, n : int) =
  		if n = 1
		then hd daysEachMonth
		else nth_int(tl daysEachMonth, n - 1)
	in
	    if (#2 date = 2) andalso
	       ((#3 date mod 400 = 0) orelse
		((#3 date mod 4 = 0) andalso #3 date mod 100 <> 0))
	    then #1 date <= 29
	    else #1 date <= nth_int(daysEachMonth, #2 date)
	end

	    
