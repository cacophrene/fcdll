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

let ( ! ) f = f ()
external id : 'a -> 'a = "%identity"

module Cell =
 struct
  let from x () = x
  let prev t = !t.prev
  let next t = !t.next
  let data t = !t.data
  let choose rev = if rev then prev, prev else next, id
  let choose2 rev = if rev then prev, prev, prev, prev else next, next, id, id
  let rec nextN t = function 0 -> t | i -> nextN !t.next (i - 1)
  let rec prevN t = function 0 -> t | i -> prevN !t.prev (i - 1)
 end

type 'a fcdll = None | Some of int * (unit -> 'a cell)

let ( <-- ) i n = if i = 0 then n - 1 else i - 1
let ( --> ) i n = if i = n - 1 then 0 else i + 1

let empty = None
let is_empty t = t = None

let length = function None -> 0 | Some (n, _) -> n

let make n x =
  if n = 0 then None else
  if n < 0 then invalid_arg "Fcdll.make" else
  let rec next () = {data = Cell.from x; next; prev = next} in
  Some (n, next)

let init n f =
  if n = 0 then None else
  if n < 0 then invalid_arg "Fcdll.init" else
  let rec loop i () = {
    data = (fun () -> f i);
    prev = loop (i <-- n);
    next = loop (i --> n);
  } in Some (n, loop 0)

let repeat k = function
  | None -> None
  | Some (n, h) -> Some (k * n, h)

let compare = function
  | None -> (function None -> 0 | _ -> -1)
  | Some (n1, h1) -> (function
    | None -> 1
    | Some (n2, h2) -> let r = Pervasives.compare n1 n2 in
      if r <> 0 then r else 
        let rec loop i t1 t2 =
          if i = n1 then 0 else
            let r = compare !(Cell.data t1) !(Cell.data t2) in
            if r = 0 then loop (i + 1) (Cell.next t1) (Cell.next t2) else r
        in loop 0 h1 h2)

let move f = function None -> None | Some (n, h) -> Some (n, f h)  
let succ t = move Cell.next t
let pred t = move Cell.prev t

let head = function
  | None -> invalid_arg "Fcdll.head"
  | Some (_, h) -> Cell.data h ()
  
let rotate = function
  | 0 -> id
  | k -> (function
    | None -> None
    | Some (n, h) -> Some (n, Cell.(if k < 0 then prevN else nextN) h (abs k)))

let blit = 
  let aux k = function
    | None -> id
    | Some (m, w) as src -> (function
      | None -> src
      | Some (n, x) when n >= m ->
        let rec f i x y () = {
          data = Cell.data (if i < k then y else x);
          next = f (i --> n) (Cell.next x) (if i < k then Cell.next y else w);
          prev = f (i <-- n) (Cell.prev x) (if i < k then Cell.prev y else w);
        } in Some (n, f 0 x w)
      | _ -> invalid_arg "Fcdll.blit")
  in (fun ~src ~src_pos ~dst ~dst_pos ~len -> 
    rotate (-dst_pos) (aux len (rotate src_pos src) (rotate dst_pos dst)))

let fill = 
  let aux = function
    | None -> make
    | Some (n, h) -> (fun m x ->
      if m < 0 || m > n then invalid_arg "Fcdll.fill" else
      let rec f i t () = {
        data = Cell.(if i < m then from x else data t);
        prev = f (i <-- n) (Cell.prev t);
        next = f (i --> n) (Cell.next t);
      } in Some (n, f 0 h))
  in (fun t ~pos ~len x -> rotate (-pos) (aux (rotate pos t) len x))

let sub = 
  let aux = function
    | None -> (fun _ -> invalid_arg "Fcdll.sub")
    | Some (n, h) -> (fun k -> 
      if k < 0 || k = 0 then None else
      if abs k > n then invalid_arg "Fcdll.sub" else
      let rec f i t () = {
        data = Cell.data t;
        next = f (i --> k) (if i = k' then h else Cell.next t);
        prev = f (i <-- k) Cell.(if i = 0 then nextN h k' else prev t);
      } and k' = k - 1 in Some (k, f 0 h))
  in (fun t ~pos ~len -> aux (rotate pos t) len)

let tail = function
  | None -> invalid_arg "Fcdll.tail"
  | Some (1, _) -> None
  | Some (n, h) -> let n' = n - 1 in
    let rec loop i t () =
      let x = Cell.prev t and y = Cell.next t in
      {!t with
        prev = loop ((if i = 1 then n else i) - 1) 
          (if i = 1 then Cell.prev x else x);
        next = loop (if i = n' then 1 else i + 1)
          (if i = n' then Cell.next y else y);
    } in Some (n', loop 1 (Cell.next h))
  
let set x = function
  | None -> make 1 x
  | Some (n, h) ->
    let rec loop i t () = {
      data = Cell.(if i = 0 then from x else data t); 
      prev = loop (i <-- n) (Cell.prev t);
      next = loop (i --> n) (Cell.next t);
    } in Some (n, loop 0 h)

let cons x = function
  | None -> make 1 x
  | Some (n, h) -> let m = n + 1 in
    let rec loop i t () = {
      data = Cell.(if i = 0 then from x else data t);
      prev = loop (if i = 0 then n else i <-- m) (Cell.prev t);
      next = if i = 0 then loop 1 t else loop (i --> m) (Cell.next t);
    } in Some (m, loop 0 h)

let ( & ) = cons

let append = function
  | None -> (fun c2 -> c2)
  | Some (n, h1) as c1 -> (function
    | None -> c1
    | Some (m, h2) -> let n' = n + m in
      let rec loop i t () = {
        data = Cell.data t;
        next = loop (i --> n')
          (if i = n - 1 then h2 else if i = n' - 1 then h1 else Cell.next t);
        prev = loop (i <-- n')
          (Cell.prev (if i = 0 then h2 else if i = n then h1 else t));
      } in Some (n', loop 0 h1))

let rev = function
  | None -> None
  | Some (n, h) ->
    let rec loop t () =
      {!t with prev = loop (Cell.next t); next = loop (Cell.prev t)}
    in Some (n, loop (Cell.prev h))



(* ***** Conversion ***** *)

module ToListImpl =
 struct
  let empty_list _ = []
  let goto_next f i {data; next; _} =
    let d = !data in
    d :: f (i - 1) !next
  let goto_prev f i {data; prev; _} =
    let d = !data in
    d :: f (i - 1) !prev
  let forward = function
    | None -> []
    | Some (n, h) ->
      let rec loop = function
        | 0 -> empty_list
        | i -> goto_next loop i
      in loop n !h
  let reverse = function
    | None -> []
    | Some (n, h) ->
      let rec loop = function
        | 0 -> empty_list
        | i -> goto_prev loop i
      in loop n !(!h.prev)
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
      } in Some (n, loop 0)
    ) else None
  let reverse t =
    let n = Array.length t in
    if n > 0 then (
      let rec loop i () = {
        data = (fun () -> Array.unsafe_get t i);
        next = loop (i <-- n);
        prev = loop (i --> n);
      } in Some (n, loop (n - 1))
    ) else None
 end

let to_list ?(rev = false) t =
  ToListImpl.(if rev then reverse else forward) t

let of_array ?(rev = false) t =
  OfArrayImpl.(if rev then reverse else forward) t
      
let of_list ?rev t = of_array ?rev (Array.of_list t)
let to_array ?rev t = Array.of_list (to_list ?rev t)



(* ***** List scanning ***** *)

let for_all ?(rev = false) p = function
  | None -> true
  | Some (n, h) -> let f, g = Cell.choose rev in
    let rec loop i t =
      i = n || (p !(Cell.data t) && loop (i + 1) (f t))
    in loop 0 (g h)

let exists ?(rev = false) p = function
  | None -> false
  | Some (n, h) -> let f, g = Cell.choose rev in
    let rec loop i t =
      i < n && (p !(Cell.data t) || loop (i + 1) (f t))
    in loop 0 (g h)

let for_all2 ?(rev = false) f = function
  | None -> (function None -> true | _ -> invalid_arg "Fcdll.for_all2")
  | Some (n1, h1) -> (function
    | None -> invalid_arg "Fcdll.for_all2"
    | Some (n2, h2) -> let n = max n1 n2 in
      let f1, f2, g1, g2 = Cell.choose2 rev in
      let rec loop i t1 t2 =
        i = n || Cell.(f !(data t1) !(data t2) && loop (i + 1) (f1 t1) (f2 t2))
      in loop 0 (g1 h1) (g2 h2))

let exists2 ?(rev = false) f = function
  | None -> (function None -> false | _ -> invalid_arg "Fcdll.exists2")
  | Some (n1, h1) -> (function
    | None -> invalid_arg "Fcdll.exists2"
    | Some (n2, h2) -> let n = max n1 n2 in
      let f1, f2, g1, g2 = Cell.choose2 rev in
      let rec loop i t1 t2 =
        i < n && Cell.(f !(data t1) !(data t2) || loop (i + 1) (f1 t1) (f2 t2))
      in loop 0 (g1 h1) (g2 h2))

let mem ?(rev = false) ?(eq = (=)) x = function
  | None -> false
  | Some (n, h) -> let f, g = Cell.choose rev in
    let rec loop i t =
      i < n && (eq x !(Cell.data t) || loop (i + 1) (f t))
    in loop 0 (g h)



(* ***** Association lists ***** *)

let assoc ?(rev = false) ?(eq = ( = )) x = function
  | None -> raise Not_found
  | Some (n, h) -> let f, g = Cell.choose rev in
    let rec loop i t =
      if i = n then raise Not_found else
        let a, b = !(Cell.data t) in
        if eq x a then b else loop (i + 1) (f t)
    in loop 0 (g h)

let mem_assoc ?(rev = false) ?(eq = ( = )) x = function
  | None -> false
  | Some (n, h) -> let f, g = Cell.choose rev in
    let rec loop i t =
      i < n && (eq x (fst !(Cell.data t)) || loop (i + 1) (f t))
    in loop 0 (g h)
    
let split ?(rev = false) = function
  | None -> None, None
  | Some (n, h) -> 
    let f1, f2, g = Cell.(if rev then prev, next, prev else next, prev, id) in
    let rec loop i t =
      let d = Cell.data t in
        (fun () ->
        { data = (fun () -> fst !d); 
          next = fst (loop (i --> n) (f1 t));
          prev = fst (loop (i <-- n) (f2 t));
        }), (fun () -> { 
          data = (fun () -> snd !d);
          next = snd (loop (i --> n) (f1 t));
          prev = snd (loop (i <-- n) (f2 t));
     }) in let a, b = loop 0 (g h) in Some (n, a), Some (n, b)

let combine ?(rev = false) = function
  | None -> (function None -> None | _ -> invalid_arg "Fcdll.combine")
  | Some (n1, h1) -> (function
    | None -> invalid_arg "Fcdll.empty"
    | Some (n2, h2) -> let n = max n1 n2 in
      let f1, f2, g1, g2 = Cell.choose2 rev in
      let f3, f4, _, _ = Cell.choose2 (not rev) in
      let rec loop i t1 t2 () = {
        data = (fun () -> !(Cell.data t1), !(Cell.data t2));
        next = loop (i --> n) (f1 t1) (f2 t2);
        prev = loop (i <-- n) (f3 t1) (f4 t2);
      } in Some (n, loop 0 (g1 h1) (g2 h2)))
    


(* ***** List searching ***** *)

let index ?(rev = false) ?(eq = (=)) x = function
  | None -> raise Not_found
  | Some (n, h) -> let f, g = Cell.choose rev in
    let rec loop i t =
      if i = n then raise Not_found else
        let y = Cell.data t () in
        if eq x y then i else loop (i + 1) (f t)
    in loop 0 (g h)

let find ?(rev = false) p = function
  | None -> raise Not_found
  | Some (n, h) -> let f, g = Cell.choose rev in
    let rec loop i t =
      if i = n then raise Not_found else
        let x = !(Cell.data t) in
        if p x then x else loop (i + 1) (f t)
    in loop 0 (g h)

let find_all ?(rev = false) p = function
  | None -> None
  | Some (n, h) -> let f, g = Cell.choose (not rev) in
    let rec loop acc i t =
      if i = n then of_list acc else
        let x = !(Cell.data t) in
        loop (if p x then x :: acc else acc) (i + 1) (f t) 
    in loop [] 0 (g h)

let partition ?(rev = false) p = function
  | None -> None, None
  | Some (n, h) -> let f, g = Cell.choose (not rev) in
    let rec loop yes no i t =
      if i = n then (of_list yes, of_list no) else
        let x = !(Cell.data t) in
        (if p x then loop (x :: yes) no else loop yes (x :: no)) (i + 1) (f t)
    in loop [] [] 0 (g h)



(* ***** Iterators ***** *)

let iter ?(rev = false) p = function
  | None -> ()
  | Some (n, h) -> let f, g = Cell.choose rev in
    let rec loop i t =
      if i < n then
        let () = p !(Cell.data t) in
        loop (i + 1) (f t)
    in loop 0 (g h)

let iteri ?(rev = false) p = function
  | None -> ()
  | Some (n, h) -> let f, g = Cell.choose rev in
    let rec loop i t =
      if i < n then
        let () = p i !(Cell.data t) in
        loop (i + 1) (f t)
    in loop 0 (g h)

let map ?(rev = false) f = function
  | None -> None
  | Some (n, h) -> 
    let f1, f2, g = Cell.(if rev then next, prev, prev else prev, next, id) in
    let rec loop i t () = {
      data = (fun () -> f !(Cell.data t));
      prev = loop (i <-- n) (f1 t);
      next = loop (i --> n) (f2 t);
    } in Some (n, loop 0 (g h))

let mapi ?(rev = false) f = function
  | None -> None
  | Some (n, h) ->
    let f1, f2, g = Cell.(if rev then next, prev, prev else prev, next, id) in
    let rec loop i t () = {
      data = (fun () -> f i !(Cell.data t));
      prev = loop (i <-- n) (f1 t);
      next = loop (i --> n) (f2 t);
    } in Some (n, loop 0 (g h))

let fold ?(rev = false) p e = function
  | None -> e
  | Some (n, h) -> let f, g = Cell.choose rev in
    let rec loop i r t =
      if i = n then r else
        let r' = p r !(Cell.data t) in
        loop (i + 1) r' (f t)
    in loop 0 e (g h)

let flatten ?rev t = fold ?rev append empty t

let foldi ?(rev = false) p e = function
  | None -> e
  | Some (n, h) -> let f, g = Cell.choose rev in
    let rec loop i r t =
      if i = n then r else
        let r' = p i r !(Cell.data t) in
        loop (i + 1) r' (f t)
    in loop 0 e (g h)

let move f = function
  | None -> ()
  | Some (n, h) ->
    let rec loop i {data; prev; next} =
      let r = f i !data in
      if r < 0 then loop (i <-- n) !prev else
      if r > 0 then loop (i --> n) !next
    in loop 0 !h



(* ***** Iterators for two-argument functions ***** *)

let iter2 ?(rev = false) p = function
  | None -> (function None -> () | _ -> invalid_arg "Fcdll.iter2")
  | Some (n1, h1) -> (function
    | None -> invalid_arg "Fcdll.iter2"
    | Some (n2, h2)-> let n = max n1 n2 in
      let f1, f2, g1, g2 = Cell.choose2 rev in
      let rec loop i t1 t2 =
        if i < n then
          let () = p !(Cell.data t1) !(Cell.data t2) in
          loop (i + 1) (f1 t1) (f2 t2)
      in loop 0 (g1 h1) (g2 h2))

let map2 f = function
  | None -> (function None -> None | _ -> invalid_arg "Fcdll.map2")
  | Some (n1, h1) -> (function
    | None -> invalid_arg "Fcdll.map2"
    | Some (n2, h2) ->
      let rec loop t1 t2 () = {
        data = (fun () -> f !(Cell.data t1) !(Cell.data t2));
        next = loop (Cell.next t1) (Cell.next t2);
        prev = loop (Cell.prev t1) (Cell.prev t2);
      } in Some (max n1 n2, loop h1 h2))
  
let fold2 ?(rev = false) p e = function
  | None -> (function None -> e | _ -> invalid_arg "Fcdll.fold2")
  | Some (n1, h1) -> (function
    | None -> invalid_arg "Fcdll.fold2"
    | Some (n2, h2) -> let n = max n1 n2 in
      let f1, f2, g1, g2 = Cell.choose2 rev in
      let rec loop i r t1 t2 =
        if i = n then r else
          let r' = p r !(Cell.data t1) !(Cell.data t2) in
          loop (i + 1) r' (f1 t1) (f2 t2)
      in loop 0 e (g1 h1) (g2 h2))
  


(* ***** Sorting ***** *)

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
