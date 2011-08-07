(* fcdll.ml - Functional circular doubly linked lists
 *  This library is licensed under the terms of the GNU LGPL version 3.
 *  - lib: ocamlopt fcdll.mli fcdll.ml -a -o fcdll.cmxa
 *  - doc: ocamldoc -html -colorize-code -stars -d html -t "Fcdll 1.0" fcdll.mli
 *)

type 'a cell = {
  data : unit -> 'a; 
  prev : unit -> 'a cell; 
  next : unit -> 'a cell;
}

external id : 'a -> 'a = "%identity"

module Cell =
 struct
  let prev t = (t ()).prev
  let next t = (t ()).next
  let data t = (t ()).data
  let contents t = data t ()
  let choose = function
    | true -> prev, prev
    | false -> id, next
  let rec n_next t = function 
    | 0 -> t 
    | i -> n_next (next t) (i - 1)
  let rec n_prev t = function
    | 0 -> t 
    | i -> n_prev (prev t) (i - 1)
  let choose_both rev = if rev then prev, next, prev else next, prev, id
  let choose2 rev = if rev then prev, prev, prev, prev else next, next, id, id
 end

type 'a fcdll = Null | Circ of (int * (unit -> 'a cell))

let ( <-- ) i n = if i = 0 then n - 1 else i - 1
let ( --> ) i n = if i = n - 1 then 0 else i + 1

let empty = Null
let is_empty t = t = Null

let length = function Null -> 0 | Circ (n, _) -> n

let make n x =
  if n = 0 then Null else
  if n < 0 then invalid_arg "Fcdll.make" else
  let rec next () = {data = (fun () -> x); next; prev = next} in
  Circ (n, next)

let init n f =
  if n = 0 then Null else
  if n < 0 then invalid_arg "Fcdll.init" else
  let rec loop i () = {
    data = (fun () -> f i);
    prev = loop (i <-- n);
    next = loop (i --> n);
  } in Circ (n, loop 0)

let repeat k = function
  | Null -> Null
  | Circ (n, h) -> Circ (k * n, h)

let iterate n f x =
  if n = 0 then Null else
  if n < 0 then invalid_arg "Fcdll.iterate" else
  let rec loop i = 
    let rec me () =   {
      data = (fun () -> if i = 0 then x else f Cell.(contents (prev me)));
      next = loop (i --> n);
      prev = loop (i <-- n);
    } in me
  in Circ (n, loop 0)



(* ----- Common functions ----- *)

let compare = function
  | Null -> (function Null -> 0 | _ -> -1)
  | Circ (n1, h1) -> (function
    | Null -> 1
    | Circ (n2, h2) -> let r = Pervasives.compare n1 n2 in
      if r <> 0 then r else 
        let rec loop i t1 t2 = Cell.(
          if i = n1 then 0 else
            let r = Pervasives.compare (contents t1) !(contents t2) in
            if r = 0 then loop (i + 1) (next t1) (next t2) else r
        ) in loop 0 h1 h2)


let move f = function Null -> Null | Circ (n, h) -> Circ (n, f h)  
let succ t = move Cell.next t
let pred t = move Cell.prev t

let head = function
  | Null -> invalid_arg "Fcdll.head"
  | Circ (_, h) -> Cell.contents h
  
let last = function
  | Null -> invalid_arg "Fcdll.last"
  | Circ (_, h) -> Cell.contents (Cell.prev h)
  
let rotate = function
  | 0 -> id
  | k -> (function
    | Null -> Null
    | Circ (n, h) -> let rotf = Cell.(if k < 0 then n_prev else n_next) in
      Circ (n, rotf h (abs k)))

let blit = 
  let aux k = function
    | Null -> id
    | Circ (m, w) as src -> (function
      | Null -> src
      | Circ (n, x) when n >= m ->
        let rec f i x y () = {
          data = Cell.data (if i < k then y else x);
          next = f (i --> n) (Cell.next x) (if i < k then Cell.next y else w);
          prev = f (i <-- n) (Cell.prev x) (if i < k then Cell.prev y else w);
        } in Circ (n, f 0 x w)
      | _ -> invalid_arg "Fcdll.blit")
  in (fun ~src ~src_pos ~dst ~dst_pos ~len -> 
    rotate (-dst_pos) (aux len (rotate src_pos src) (rotate dst_pos dst)))

let fill = 
  let aux = function
    | Null -> make
    | Circ (n, h) -> (fun m x ->
      if m < 0 || m > n then invalid_arg "Fcdll.fill" else
      let rec f i t () = {
        data = if i < m then (fun () -> x) else Cell.data t;
        prev = f (i <-- n) (Cell.prev t);
        next = f (i --> n) (Cell.next t);
      } in Circ (n, f 0 h))
  in (fun t ~pos ~len x -> rotate (-pos) (aux (rotate pos t) len x))

let extract = 
  let aux = function
    | Null -> (fun _ -> invalid_arg "Fcdll.sub")
    | Circ (n, h) -> (fun k -> 
      if k = 0 then Null else
      (*if abs k > n then invalid_arg "Fcdll.sub" else*)
      let ff, gg, hh = Cell.(if k < 0 then prev, n_prev, next else next, n_next, prev) in
      let rec f i t () = {
        data = Cell.data t;
        next = f (i --> abs k) (if i = k' then h else ff t);
        prev = f (i <-- abs k) Cell.(if i = 0 then gg h k' else hh t);
      } and k' = abs k - 1 in Circ (abs k, f 0 h))
  in (fun t ~pos ~len -> aux (rotate pos t) len)

let take len = function
  | Null -> invalid_arg "Fcdll.take"
  | t -> extract t ~pos:(if len < 0 then -1 else 0) ~len

let take_while ?(rev = false) p = function
  | Null -> Null
  | Circ (n, h) as c -> let g, f = Cell.choose rev in
    let rec loop i k t =
      if i < n && p (Cell.contents t) then loop (i + 1) (k + 1) (f t)
      else extract c ~pos:(if rev then -1 else 0) ~len:(if rev then -k else k)
    in loop 0 0 (g h)

let drop k = function
  | Null -> invalid_arg "Fcdll.drop"
  | Circ (n, _) as t -> if abs k >= n then Null else
    extract t 
      ~pos:(if k < 0 then k - 1 else k)
      ~len:(if k < 0 then - n - k else n - k)

let drop_while ?(rev = false) p = function
  | Null -> Null
  | Circ (n, h) as c -> let g, f = Cell.choose rev in
    let rec loop i k t =
      if i < n && p (Cell.contents t) then loop (i + 1) (k + 1) (f t) else
      extract c ~pos:(if rev then - k - 1 else k) 
        ~len:(if rev then k - n else n - k)
    in loop 0 0 (g h)

let split_at = function
  | 0 -> (fun x -> Null, x)
  | k -> (function
    | Null -> invalid_arg "Fcdll.split_at"
    | t -> take k t, drop k t)

let tail = function
  | Null -> invalid_arg "Fcdll.tail"
  | Circ (1, _) -> Null
  | Circ (n, h) -> let n' = n - 1 in
    let rec loop i t () =
      let x = Cell.prev t and y = Cell.next t in
      {(t ()) with
        prev = loop ((if i = 1 then n else i) - 1) 
          (if i = 1 then Cell.prev x else x);
        next = loop (if i = n' then 1 else i + 1)
          (if i = n' then Cell.next y else y);
    } in Circ (n', loop 1 (Cell.next h))
  
let set x = function
  | Null -> make 1 x
  | Circ (n, h) ->
    let rec loop i t () = {
      data = if i = 0 then (fun () -> x) else Cell.data t; 
      prev = loop (i <-- n) (Cell.prev t);
      next = loop (i --> n) (Cell.next t);
    } in Circ (n, loop 0 h)

let cons x = function
  | Null -> make 1 x
  | Circ (n, h) -> let m = n + 1 in
    let rec loop i t () = {
      data = if i = 0 then (fun () -> x) else Cell.data t;
      prev = loop (if i = 0 then n else i <-- m) (Cell.prev t);
      next = if i = 0 then loop 1 t else loop (i --> m) (Cell.next t);
    } in Circ (m, loop 0 h)

let ( & ) = cons

let append = function
  | Null -> (fun c2 -> c2)
  | Circ (n, h1) as c1 -> (function
    | Null -> c1
    | Circ (m, h2) -> let n' = n + m in
      let rec loop i t () = {
        data = Cell.data t;
        next = loop (i --> n')
          (if i = n - 1 then h2 else if i = n' - 1 then h1 else Cell.next t);
        prev = loop (i <-- n')
          (Cell.prev (if i = 0 then h2 else if i = n then h1 else t));
      } in Circ (n', loop 0 h1))

let insert x ~pos = function
  | Null -> invalid_arg "Fcdll.insert"
  | t -> rotate (-pos) (cons x (rotate (if pos < 0 then pos + 1 else pos) t))

let intersperse ?(rev = false) x = function
  | Null -> Null
  | Circ (n, h) -> let f_next, f_prev, f_init = Cell.choose_both rev in 
    let rec loop i t ok () = {
      data = if ok then (fun () -> x) else Cell.data t;
      prev = if ok then loop i t false else loop (i --> n) (f_prev t) true;
      next = if ok then loop i t false else loop (i <-- n) (f_next t) true;
    } in Circ (n lsl 1, loop 0 (f_init h) false)
 
let rev = function
  | Null -> Null
  | Circ (n, h) -> Cell.(
    let rec loop t () =
      {(t ()) with prev = loop (next t); next = loop (prev t)}
    in Circ (n, loop (prev h)))



(* ----- Conversion ----- *)

module ToListImpl =
 struct
  let empty_list _ = []
  let goto_next f i t =
    let d = Cell.contents t in
    d :: f (i - 1) (Cell.next t)
  let goto_prev f i t =
    let d = Cell.contents t in
    d :: f (i - 1) (Cell.prev t)
  let forward = function
    | Null -> []
    | Circ (n, h) ->
      let rec loop = function
        | 0 -> empty_list
        | i -> goto_next loop i
      in loop n h
  let reverse = function
    | Null -> []
    | Circ (n, h) ->
      let rec loop = function
        | 0 -> empty_list
        | i -> goto_prev loop i
      in loop n (Cell.prev h)
 end

module OfArrayImpl =
 struct
  let forward t =
    let n = Array.length t in
    if n > 0 then (
      let rec loop i () = {
        data = (fun () -> Array.unsafe_get t i);
        next = loop (i --> n);
        prev = loop (i <-- n);
      } in Circ (n, loop 0)
    ) else Null
  let reverse t =
    let n = Array.length t in
    if n > 0 then (
      let rec loop i () = {
        data = (fun () -> Array.unsafe_get t i);
        next = loop (i <-- n);
        prev = loop (i --> n);
      } in Circ (n, loop (n - 1))
    ) else Null
 end

let to_list ?(rev = false) t =
  ToListImpl.(if rev then reverse else forward) t

let of_array ?(rev = false) t =
  OfArrayImpl.(if rev then reverse else forward) t
      
let of_list ?rev t = of_array ?rev (Array.of_list t)
let to_array ?rev t = Array.of_list (to_list ?rev t)



(* ----- List scanning ----- *)

let for_all ?(rev = false) p = function
  | Null -> true
  | Circ (n, h) -> let init, next = Cell.choose rev in
    let rec loop i t =
      i = 0 || (p (Cell.contents t) && loop (i - 1) (next t))
    in loop n (init h)

let exists ?(rev = false) p = function
  | Null -> false
  | Circ (n, h) -> let init, next = Cell.choose rev in
    let rec loop i t =
      i > 0 && (p (Cell.contents t) || loop (i - 1) (next t))
    in loop n (init h)

let for_all2 ?(rev = false) f = function
  | Null -> (function Null -> true | _ -> invalid_arg "Fcdll.for_all2")
  | Circ (n1, h1) -> (function
    | Null -> invalid_arg "Fcdll.for_all2"
    | Circ (n2, h2) -> let n = max n1 n2 in
      let f1, f2, g1, g2 = Cell.choose2 rev in
      let rec loop i t r =
        i = n || Cell.(f (contents t) (contents r) && loop (i + 1) (f1 t) (f2 r))
      in loop 0 (g1 h1) (g2 h2))

let exists2 ?(rev = false) f = function
  | Null -> (function Null -> false | _ -> invalid_arg "Fcdll.exists2")
  | Circ (n1, h1) -> (function
    | Null -> invalid_arg "Fcdll.exists2"
    | Circ (n2, h2) -> let n = max n1 n2 in
      let f1, f2, g1, g2 = Cell.choose2 rev in
      let rec loop i t1 t2 =
        i < n && Cell.(f (contents t1) (contents t2) || loop (i + 1) (f1 t1) (f2 t2))
      in loop 0 (g1 h1) (g2 h2))

let mem ?(rev = false) ?(eq = (=)) x = function
  | Null -> false
  | Circ (n, h) -> let g, f = Cell.choose rev in
    let rec loop i t =
      i < n && (eq x (Cell.contents t) || loop (i + 1) (f t))
    in loop 0 (g h)



(* ----- List Searching ----- *)

let find ?(rev = false) p = function
  | Null -> None
  | Circ (n, h) -> let init, next = Cell.choose rev in
    let rec loop t = function
      | 0 -> None
      | i -> let x = Cell.contents t in
        if p x then Some x else loop (next t) (i - 1)
    in loop (init h) n

let find_all ?(rev = false) p = function
  | Null -> Null
  | Circ (n, h) -> let init, next = Cell.choose rev in
    let rec loop r t = function
      | 0 -> of_list ~rev:true r
      | i -> let x = Cell.contents t in
        loop (if p x then x :: r else r) (next t) (i - 1)
    in loop [] (init h) n

let index ?(rev = false) p = function
  | Null -> None
  | Circ (n, h) -> let init, next = Cell.choose rev in
    let rec loop t = function
      | 0 -> None
      | i -> if p (Cell.contents t) then Some (n - i) 
        else loop (next t) (i - 1)
    in loop (init h) n

let indexes ?(rev = false) p = function
  | Null -> Null
  | Circ (n, h) -> let init, next = Cell.choose rev in
    let rec loop r t i =
      if i = n then of_list ~rev:true r else
      loop (if p (Cell.contents t) then i :: r else r) (next t) (i + 1)
    in loop [] (init h) 0

let partition ?(rev = false) p = function
  | Null -> Null, Null
  | Circ (n, h) -> let init, next = Cell.choose rev in
    let rec loop r s t = function
      | 0 -> of_list ~rev:true r, of_list ~rev:true s
      | i -> let x = Cell.contents t in
        (if p x then loop (x :: r) s else loop r (x :: s)) (next t) (i - 1)
    in loop [] [] (init h) n



(* ----- Association lists ----- *)

let assoc ?(rev = false) ?(eq = ( = )) x = function
  | Null -> raise Not_found
  | Circ (n, h) -> let g, f = Cell.choose rev in
    let rec loop i t =
      if i = n then raise Not_found else
        let a, b = (Cell.contents t) in
        if eq x a then b else loop (i + 1) (f t)
    in loop 0 (g h)

let mem_assoc ?(rev = false) ?(eq = ( = )) x = function
  | Null -> false
  | Circ (n, h) -> let g, f = Cell.choose rev in
    let rec loop i t =
      i < n && (eq x (fst (Cell.contents t)) || loop (i + 1) (f t))
    in loop 0 (g h)
    
let split ?(rev = false) = function
  | Null -> Null, None
  | Circ (n, h) -> 
    let f1, f2, g = Cell.(if rev then prev, next, prev else next, prev, id) in
    let rec loop i t =
        (fun () ->
        { data = (fun () -> fst (Cell.contents t)); 
          next = fst (loop (i --> n) (f1 t));
          prev = fst (loop (i <-- n) (f2 t));
        }), (fun () -> { 
          data = (fun () -> snd (Cell.contents t));
          next = snd (loop (i --> n) (f1 t));
          prev = snd (loop (i <-- n) (f2 t));
     }) in let a, b = loop 0 (g h) in Circ (n, a), Some (n, b)

let combine ?(rev = false) = function
  | Null -> (function Null -> Null | _ -> invalid_arg "Fcdll.combine")
  | Circ (n1, h1) -> (function
    | Null -> invalid_arg "Fcdll.empty"
    | Circ (n2, h2) -> let n = max n1 n2 in
      let f1, f2, g1, g2 = Cell.choose2 rev in
      let f3, f4, _, _ = Cell.choose2 (not rev) in
      let rec loop i t1 t2 () = {
        data = (fun () -> Cell.(contents t1, contents t2));
        next = loop (i --> n) (f1 t1) (f2 t2);
        prev = loop (i <-- n) (f3 t1) (f4 t2);
      } in Circ (n, loop 0 (g1 h1) (g2 h2)))
   
    

(* ----- Iterators ----- *)

let iter ?(rev = false) f = function
  | Null -> ()
  | Circ (n, h) -> let init, next = Cell.choose rev in
    let rec loop t = function
      | 0 -> ()
      | i -> let () = f (Cell.contents t) in
        loop (next t) (i - 1)
    in loop (init h) n

let iteri ?(rev = false) f = function
  | Null -> ()
  | Circ (n, h) -> let init, next = Cell.choose rev in
    let rec loop t i =
      if i < n then
        let () = f i (Cell.contents t) in
        loop (next t) (i + 1)
    in loop (init h) 0

let map ?(rev = false) f = function
  | Null -> Null
  | Circ (n, h) -> 
    let f1, f2, g = Cell.(if rev then next, prev, prev else prev, next, id) in
    let rec loop i t () = {
      data = (fun () -> f (Cell.contents t));
      prev = loop (i <-- n) (f1 t);
      next = loop (i --> n) (f2 t);
    } in Circ (n, loop 0 (g h))

let mapi ?(rev = false) f = function
  | Null -> Null
  | Circ (n, h) ->
    let f1, f2, g = Cell.(if rev then next, prev, prev else prev, next, id) in
    let rec loop i t () = {
      data = (fun () -> f i (Cell.contents t));
      prev = loop (i <-- n) (f1 t);
      next = loop (i --> n) (f2 t);
    } in Circ (n, loop 0 (g h))

let fold ?(rev = false) f e = function
  | Null -> e
  | Circ (n, h) -> let init, next = Cell.choose rev in
    let rec loop r t = function
      | 0 -> r
      | i -> loop (f r (Cell.contents t)) (next t) (i - 1)
    in loop e (init h) n

let flatten ?rev t = fold ?rev append empty t

let foldi ?(rev = false) f e = function
  | Null -> e
  | Circ (n, h) -> let init, next = Cell.choose rev in
    let rec loop r t i =
      if i = n then r
      else loop (f i r (Cell.contents t)) (next t) (i + 1)
    in loop e (init h) 0

let scan ?(rev = false) f e = function
  | Null -> make 1 e
  | Circ (n, h) -> let m = n + 1 in 
    let f_next, f_prev, f_init = Cell.choose_both rev in
    let rec loop i t = 
      let rec me () = {
        data = (fun () -> if i = 0 then e else 
          Cell.(f (contents (prev me)) (contents t)));
        next = loop (i --> m) (if i = 0 then t else f_next t);
        prev = loop (i <-- m) (f_prev t);
      } in me 
     in Circ (m, loop 0 (f_init h)) 

let move f = function
  | Null -> ()
  | Circ (n, h) ->
    let rec loop t i =
      let r = f i (Cell.contents t) in
      if r < 0 then loop (Cell.prev t) (i <-- n) else
      if r > 0 then loop (Cell.next t) (i --> n)
    in loop h 0



(* ----- Iterators for two-argument functions ----- *)

let iter2 ?(rev = false) p = function
  | Null -> (function Null -> () | _ -> invalid_arg "Fcdll.iter2")
  | Circ (n1, h1) -> (function
    | Null -> invalid_arg "Fcdll.iter2"
    | Circ (n2, h2)-> let n = max n1 n2 in
      let f1, f2, g1, g2 = Cell.choose2 rev in
      let rec loop i t1 t2 =
        if i < n then
          let () = Cell.(p (contents t1) (contents t2)) in
          loop (i + 1) (f1 t1) (f2 t2)
      in loop 0 (g1 h1) (g2 h2))

let map2 f = function
  | Null -> (function Null -> Null | _ -> invalid_arg "Fcdll.map2")
  | Circ (n1, h1) -> (function
    | Null -> invalid_arg "Fcdll.map2"
    | Circ (n2, h2) ->
      let rec loop t1 t2 () = Cell.({
        data = (fun () -> f (contents t1) (contents t2));
        next = loop (next t1) (next t2);
        prev = loop (prev t1) (prev t2);
      }) in Circ (max n1 n2, loop h1 h2))
  
let fold2 ?(rev = false) p e = function
  | Null -> (function Null -> e | _ -> invalid_arg "Fcdll.fold2")
  | Circ (n1, h1) -> (function
    | Null -> invalid_arg "Fcdll.fold2"
    | Circ (n2, h2) -> let n = max n1 n2 in
      let f1, f2, g1, g2 = Cell.choose2 rev in
      let rec loop i r t1 t2 =
        if i = n then r else
          let r' = Cell.(p r (contents t1) (contents t2)) in
          loop (i + 1) r' (f1 t1) (f2 t2)
      in loop 0 e (g1 h1) (g2 h2))


(* ----- Sorting ----- *)

module HeapSort =
 struct
  type 'a heap = Empty | Node of 'a heap * 'a * 'a heap

  let root = function
    | Empty -> invalid_arg "Fcdll.heap_sort"
    | Node (_, x, _) -> x

  let rec remove cmp = function
    | Empty -> Empty
    | Node (fg, _, fd) -> 
      if fg = Empty then fd else
      if fd = Empty then fg else
        let x = root fg and y = root fd in
        if cmp x y > 0 then Node (remove cmp fg, x, fd)
        else  Node (fg, y, remove cmp fd)

  let create x = Node (Empty, x, Empty)

  let rec insert cmp = function
    | Empty -> create
    | Node (fg, x, fd) -> (fun y ->
      if cmp x y > 0 then Node (fd, x, insert cmp fg y)
      else Node (fd, y, insert cmp fg x))

  let of_cdll cmp t = fold (insert cmp) Empty t

  let run ?(cmp = Pervasives.compare) t =
    let rec loop t = function
      | Empty -> of_list t
      | heap -> loop (root heap :: t) (remove cmp heap)
    in loop [] (of_cdll cmp t)
 end
 
let heap_sort ?cmp t = HeapSort.run ?cmp t
