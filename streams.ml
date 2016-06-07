type 'a stream = Stream of 'a * (unit -> 'a stream)

(* NOTE: You may add the rec keyword as you wish for this file. *)


let rec take n (Stream (h,t)) =
  if n<=0 then [] else h::(take (n-1) (t ()))

let rec repeat x =
  Stream (x, fun() -> repeat x)   

let rec map f (Stream (h,t)) = 
  Stream( (f h), fun () -> map f (t ()))
          
let rec diag (Stream(h,t)) =
 match h with 
 |Stream (hd, tl) -> Stream ( hd, fun () -> diag (map (fun (Stream(a,b)) -> b ()) (t ()))) 
   
let rec suffixes s = 
  match s with
  |Stream(h,t)-> Stream( s, fun () -> suffixes (t ()))
  
let rec interleave s s' = 
  match s with
  |Stream (h,t) -> Stream( h, fun () -> interleave s' (t ()))

let rec fibs () =
  let rec helper a b = Stream (b, fun () -> helper b (a + b)) in 
  Stream(0, fun () -> helper 0 1)

(** Returns π approximation float stream that uses partial sum 
    π = 4 * Σ {n=0 to ∞}[((−1)^n) / (2n + 1)] *)
let pi () = 
  let part_sum n = 4.*.((-1.)**float_of_int(n))/.((2.*.float_of_int(n))+.1.) in
  
  let rec helper n' acc = Stream ( acc+.part_sum(n'), fun () -> 
  	                               helper (n'+1) (acc+.part_sum(n')) ) in 
  Stream (part_sum(0), fun () -> helper 1 (part_sum(0)))

(** Returns a stream of number genertated recursively; a0 = 1, and an is generated from
an−1 by reading off the digits of an−1 as follows: for each consecutive sequence of
identical digits, pronounce the number of consecutive digits and the digit itself.*)
let look_and_say () =  
  let rec helper acc prev count l  =  
    match l with 
    | [] -> (match prev with 
            | None -> Stream (List.rev (acc) , fun () -> helper [] None 0 (List.rev (acc)))
            | Some x -> Stream (List.rev (x::count::acc) , 
                             fun () -> helper [] None 0 (List.rev (x::count::acc))))
    | h::t ->  match prev with 
                   |None ->  helper acc (Some h) (1) t
                   |Some x -> if (x = h) then helper acc prev (count+1) t 
                              else helper (x::count::acc) (Some h) 1 t  in 

  Stream ( [1] , fun () -> helper [] None 0 [1] )


(** testing branching feature *)

  