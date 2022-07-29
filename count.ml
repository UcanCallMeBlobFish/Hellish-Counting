(*syncronized one *)
#directory "+threads";;
#load "unix.cma";;
#load "threads.cma";;
open Thread
open Event



open Event
open Thread

let  spawn_counter n = 

  let rec aux (curr,n)  = 
   
    if curr > n then () else  
    let k = Printf.sprintf "Thread ID: %d: %d" (Thread.id(Thread.self ())) curr in
        print_endline (k);
  aux ((curr+1),n)  
  in 
  Thread.create aux (0,n)

let rec stop list = 
  match list with
 | [] -> ()
 | h::t -> let _ = join h in
 stop t

 
let run_counters m n =

    let ktimethread k ~f =
    
    if n < 0 then raise (Invalid_argument "ktimethread");
    let rec loop i accum =
      if i = 0 then accum
      else loop (i-1) (f (i-1) :: accum)
    in
    loop k []
    in
    let threadlist = ktimethread m (fun _ -> spawn_counter n)
  
  in

  threadlist |> stop ;
  print_newline ()
;;


(*not syncronizedone*)

open Event
open Thread


let spawn_counter n = 

  let ch = Event.new_channel()

  in

  let rec aux  rn=
    let _ = Event.sync(Event.receive ch)
    in
  if rn > n then Event.sync(Event.send ch true)
  else 
  (* let s = print_string("Thread ID:" ^ (Thread.id(Thread.self ())) ^ rn ) *)
    let l =  Printf.sprintf "Thread ID: %d: %d" (Thread.id(Thread.self ())) rn
    in 
    let _ = print_endline l
    in
    let _ = Event.sync(Event.send ch false)
    in 
    aux (rn+1)
  in
  let _ = Thread.create aux 0
  in 
  ch
  ;;

  let run_counters m n =

    let ktimethread k ~f =
    
      if n < 0 then raise (Invalid_argument "ktimethread");
      let rec loop i accum =
        if i = 0 then accum
        else loop (i-1) (f (i-1) :: accum)
      in
      loop k []
      in
      let threadlist = ktimethread m (fun _ -> spawn_counter n)
    
    in

    let rec aux  = function 
      |[] -> ()
      |x::xs -> let _ = Event.sync(Event.send x true)
      in
      match Event.sync(Event.receive x) with
      | true -> aux xs
      | false -> aux (xs@[x])
  
      
      in aux threadlist
  
  

