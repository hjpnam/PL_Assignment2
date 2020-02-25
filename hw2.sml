(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(str, strList) =
	let fun f(stringList, acc) =
		case stringList of 
			  [] => NONE
			| x :: xs => if same_string(x, str)
						 then SOME(acc @ xs)
						 else f(xs, x :: acc)
	in f(strList, [])
	end

(* string list list * string => string list *)				 
fun get_substitutions1(subs, s) =
	case subs of
		  [] => []
		| x :: xs => let val listWithOutS = all_except_option(s, x)
					 in case listWithOutS of
							  NONE => get_substitutions1(xs, s)
							| SOME exp => exp @ get_substitutions1(xs, s)
					 end

fun get_substitutions2(subs, s) =
    let fun f(subsList, acc) =
        case subsList of
              [] => acc
            | x :: xs => let val listWithOutS = all_except_option(s, x)
                         in case listWithOutS of
                              NONE => f(xs, acc)
                            | SOME exp => f(xs, acc @ exp)
                         end
    in f(subs, [])
    end
    
fun similar_names(subs, fullname) =
    let val {first=fname,middle=mname,last=lname} = fullname
    in
        let val validSubs = get_substitutions2(subs, fname)
            fun f(subList, acc) =
                    case subList of
                          [] => acc
                        | x :: xs => 
                              f(xs, {first=x,middle=mname,last=lname} :: acc)
        in
            fullname :: f(validSubs, [])
        end
    end
    

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color(card) =
    case card of
        (suit, _) => case suit of
                          Clubs => Black
                        | Diamonds => Red
                        | Hearts => Red
                        | Spades => Black

fun card_value(card) =
    case card of
        (_, rank) => case rank of
                          Jack => 10
                        | Queen => 10
                        | King => 10
                        | Ace => 11
                        | Num n => n
             
(* card list * card * exception => card list*)             
fun remove_card(cs, c, e) =
    let fun f(cList, acc) =
            case cList of
                  [] => raise e
                | x :: xs => if x = c
                             then acc @ xs
                             else f(xs, x :: acc)
    in f(cs, [])
    end
 
fun all_same_color(cs) =
    case cs of
          [] => true
        | x :: [] => true
        | x :: x' :: xs => 
              card_color(x) = card_color(x') andalso all_same_color(x' :: xs)

fun sum_cards(cs) =
    let fun f(cList, sum) =
            case cList of 
                  [] => sum
                | x :: xs => f(xs, card_value(x) + sum)
    in f(cs, 0)
    end
    
fun score(cs, goal) =
    let val sum = sum_cards cs
    in
        let val prelimScore = if sum > goal 
                              then 3 * (sum - goal) 
                              else goal - sum
        in
            if all_same_color cs
            then prelimScore div 2
            else prelimScore
        end
    end
    
fun officiate(cs, ms, goal) =
    let fun f(heldCards, deck, moveList) =
            if sum_cards heldCards > goal 
            then score(heldCards, goal)
            else 
                case moveList of
                      [] => score(heldCards, goal)
                    | (Discard c) :: xs => 
                          let val updatedHand = remove_card(heldCards, c, IllegalMove)
                          in f(updatedHand, deck, xs)
                          end
                    | Draw :: xs => 
                          (case deck of
                                [] => score(heldCards, goal)
                              | c :: cs => f(c :: heldCards, cs, xs))
    in
        f([], cs, ms)
    end
                                        
                                    
                         
