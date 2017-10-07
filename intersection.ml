let rec intersection l1 l2 =
  	 let rec contains i l = match l with
  	   | [] -> false
  	   | h::t -> if i = h then true else contains i t
  	 in
  match l1 with
  | [] -> []
  | h::t -> if (contains h l2) then h::(intersection t l2) else intersection t l2
