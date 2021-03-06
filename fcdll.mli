(* fcdll.mli - Functional circular doubly linked lists
 *  This library is licensed under the terms of the GNU LGPL version 3.
 *  - lib: ocamlopt fcdll.mli fcdll.ml -a -o fcdll.cmxa
 *  - doc: ocamldoc -html -colorize-code -stars -d html -t "Fcdll 1.0" fcdll.mli
 *)

(** The module [Fcdll] implements circular doubly linked lists in OCaml. Unlike
  * many other modules available on the web, this implementation use 
  * {i functional} (immutable) lists with some kind of {i deferred computation} 
  * (laziness). This has three main consequences you should consider when using 
  * these lists in your applications :
  *   - Values are {i not} computed once and for all.
  *   - As a consequence, side effects should be avoided.
  *   - Huge lists (up to [max_int] elements) are easy to create. 
  *
  * {b Important note} : in OCaml [List] module, some functions need two lists 
  * of same length (say, for example, [iter2]). In this module, however, there 
  * is no need for such a constraint because it is always possible to cycle to 
  * the first element. Please keep this in mind when playing with such 
  * functions. *)

type 'a fcdll
(** The type for functional (immutable) circular doubly linked lists. *)



(** {2 List creation} 
  * These functions are directly inspired from the [Array] module of OCaml
  * standard library. *)

val empty : 'a fcdll
(** The empty list. *)

val make : int -> 'a -> 'a fcdll
(** [Fcdll.make n x] returns a fresh list of length [n], initialized with [x].
  * All the elements of this new list are initially physically equal to [x] (in
  * the sense of the [==] predicate). Consequently, if [x] is mutable, it is
  * shared among all elements of the list, and modifying [x] through one of the 
  * list entries will modify all other entries at the same time.
  * @raise Invalid_argument if [n < 0]. *)

val init : int -> (int -> 'a) -> 'a fcdll
(** [Fcdll.init n f] returns a fresh list of length [n], with element number [i]
  * initialized to the result of [f i]. In other terms, [Fcdll.init n f] builds
  * a list from the results of [f] applied to integers from [0] to [n - 1]. If
  * [n] is negative, the result is equal to [Fcdll.(rev (init (-n) f))]. *)

val repeat : int -> 'a fcdll -> 'a fcdll
(** [Fcdll.repeat n t] repeats [n] times the elements of [t]. {b Note} : 
  * If [t] contains mutable elements, they will be shared by all occurrences in 
  * the list, and modifying them through one of the list entries will modify all 
  * other entries at the same time. *)

val iterate : int -> ('a -> 'a) -> 'a -> 'a fcdll
(** [Fcdll.iterate n f x] returns [\[x; f x; f (f x); ...; f (... (f x) ...)\]].
  * @raise Invalid_argument if [n < 0]. 
  * {b Note} : this function is as fast as any other function in this library, 
  * even for huge lists. However, be careful when trying to access to the last
  * element, because it requires forcing every intermediate computation, which 
  * may be very long. *)



(** {2 Common functions} *)

val is_empty : 'a fcdll -> bool
(** [Fcdll.is_empty t] returns [true] if [t] is empty, [false] otherwise. *)

val length : 'a fcdll -> int
(** [Fcdll.length t] returns the number of {i distinct} elements in list [t]. Two 
  * elements [x] and [y] are considered distinct if and only if [x != y] is 
  * true. This function runs in constant time. *)

val compare : 'a fcdll -> 'a fcdll -> int
(** [Fcdll.compare t1 t2] compares the given lists [t1] and [t2] and returns a
  * result which follows the same specifications as [Pervasives.compare]. That
  * is, it returns [0] when [t1] and [t2] are equal, [1] when [t1] is greater
  * than [t2], and [-1] otherwise. *)

val head : 'a fcdll -> 'a
(** [Fcdll.head t] returns the value stored in the first element of the list [t].
  * @raise Invalid_argument if the list is empty. *)

val last : 'a fcdll -> 'a
(** [Fcdll.last t] returns the value which occupy the last position in the list 
  * [t].
  * @raise Invalid_argument if the list is empty. *)

val tail : 'a fcdll -> 'a fcdll
(** [Fcdll.tail t] returns the given list [t] without its first element. 
  * @raise Invalid_argument if the list is empty. *)
  
val rev : 'a fcdll -> 'a fcdll
(** List reversal. *)



(** {2 Rotation} 
  * You should consider that circular lists do not have beginning. In the 
  * functions below, the {i first element} is the actual element which is 
  * directly accessible to [Fcdll.head]. This can be changed at any time by 
  * rotating the list (see [Fcdll.succ], [Fcdll.pred] and [Fcdll.rotate]). *)

val pred : 'a fcdll -> 'a fcdll
(** Rotates the given list so that the last element becomes the first one.
  * Does nothing if the list is empty. *)

val succ : 'a fcdll -> 'a fcdll
(** Rotates the given list so that the second element becomes the first one.
  * Does nothing if the list is empty. *)
  
val rotate : int -> 'a fcdll -> 'a fcdll
(** [rotate n t] rotates [n] times the list [t] using [succ] when [n > 0], 
  * [pred] otherwise. *)



(** {2 List edition} *)

val set : 'a -> 'a fcdll -> 'a fcdll
(** [Fcdll.set x t] sets [x] as the value stored in the first element of the list
  * [t]. The empty list is replaced by [Fcdll.make 1 x]. *)

val cons : 'a -> 'a fcdll -> 'a fcdll
(** [Fcdll.cons x t] adds [x] in first position to list [t]. To add [x] to the 
  * end of the list, just use [Fcdll.(succ (cons x t))]. *)

val ( & ) : 'a -> 'a fcdll -> 'a fcdll
(** Same as [cons] (see above). The symbol [(&)] is a deprecated synonym for
  * [(&&)] in OCaml. We decided to reuse it in [Fcdll]. *)

val append : 'a fcdll -> 'a fcdll -> 'a fcdll
(** [Fcdll.append t1 t2] returns a fresh list containing the concatenation of 
  * lists [t1] and [t2]. *)

val insert : 'a -> pos:int -> 'a fcdll -> 'a fcdll
(** [Fcdll.insert x ~pos:i t] inserts the value [x] at index [i] in list [t].
  * Negative values of [i] are allowed.
  * @raise Invalid_argument if [t] is empty. *)

val fill : 'a fcdll -> pos:int -> len:int -> 'a -> 'a fcdll
(** [fill t ~pos ~len x] stores [x] in elements from [pos] to [pos + len - 1] in
  * list [t]. Negative values for parameter [pos] are allowed.
  * @raise Invalid_argument if [len < 0] or [len > length t]. *)

val blit : 
  src:'a fcdll -> 
  src_pos:int -> dst:'a fcdll -> dst_pos:int -> len:int -> 'a fcdll
(** [blit ~src ~src_pos ~dst ~dst_pos len] stores [len] elements of list [src]
  * starting at [src_pos] in list [dst] starting at [dst_pos]. Both [src_pos]
  * and [dst_pos] can be negative and [len] can be greater than [length src] (in
  * this case, the function simply cycle to the beginning).
  * @raise Invalid_argument if [length src > length dst]. *)

val flatten : ?rev:bool -> 'a fcdll fcdll -> 'a fcdll
(** [Fcdll.flatten t] concatenates the given list of lists [t]. The elements of 
  * the argument are all concatenated together in the same order to give the 
  * result. {b Note} : this function is equivalent to 
  * [Fcdll.(fold append empty)]. *)

val intersperse : ?rev:bool -> 'a -> 'a fcdll -> 'a fcdll
(** [Fcdll.intersperse x t] inserts [x] between each element of list [t]. Please
  * note that since [t] is circular, and additional occurrence of [x] is added 
  * between the last and the first elements. For example, 
  * [Fcdll.(to_list (intersperse 0 (of_list \[1; 2; 3\])))] returns the
  * list [\[1; 0; 2; 0; 3; 0\]]. {b Note} : this function is derived from 
  * Haskell [intersperse] function. *)



(** {2 Sublist extraction} *)

val extract : 'a fcdll -> pos:int -> len:int -> 'a fcdll
(** [Fcdll.extract t ~pos:p ~len:k] returns a fresh list composed of the [k] elements
  * of list [t] starting at index [p]. Negative values of [p] are allowed. When
  * [k] is negative, elements are returned in reverse order. For example, the 
  * expression [extract \[1; 2; 3\] ~pos:(-1) ~len:(-3)] returns [\[3; 2; 1\]].
  * If [k > length t], the resulting list will be longer, with recycled values.
  * @raise Invalid_argument if the given list is empty. *)

val subseq : ?rev:bool -> 'a fcdll -> 'a fcdll fcdll
(** [Fcdll.subseq t] returns the list of all sublists of [t] in increasing 
  * order. For example, [subseq \[1; 2\]] returns [\[\[\]; \[1\]; \[1; 2\]\]].
  * If [rev] is set to [true], sublist extraction is performed in reverse order,
  * as in [subseq (rev t)]. *)

val take : int -> 'a fcdll -> 'a fcdll
(** [Fcdll.take k t] returns the [k] first elements of list [t]. If [k < 0], 
  * elements are returned in reverse order. if [k > length t], the returned list
  * contains recycled elements.
  * @raise Invalid_argument if [t] is empty. *)

val drop : int -> 'a fcdll -> 'a fcdll
(** [Fcdll.drop k t] returns the remaining suffix after [Fcdll.take k t].
  * @raise Invalid_argument if [t] is empty. *)

val take_while : ?rev:bool -> ('a -> bool) -> 'a fcdll -> 'a fcdll
(** [Fcdll.take_while f t] returns the longest prefix of [t] which elements
  * satisfy predicate [f]. *)

val drop_while : ?rev:bool -> ('a -> bool) -> 'a fcdll -> 'a fcdll
(** [Fcdll.drop_while f t] returns the suffix remaining after 
  * [Fcdll.take_while f t]. *)

val split_at : int -> 'a fcdll -> 'a fcdll * 'a fcdll
(** [Fcdll.split_at k t] splits the given list [t] at position [k] and returns 
  * the two sublists. Negative values of [k] are allowed and result in reversed
  * sublists. If [abs k > length t], the first sublist contains recycled values 
  * and the second is empty.
  * @raise Invalid_argument when [k <> 0 && is_empty t] is true. *)

val span : ?rev:bool -> ('a -> bool) -> 'a fcdll * 'a fcdll
(** [Fcdll.span p t] returns a couple of lists composed of the longest prefix of
  * [t] of elements which satisfy [p], and the remaining elements. This function
  * is equivalent to [Fcdll.(take_while p t, drop_while p t)]. 
  * @raise Invalid_argument if [t] is empty. *)



(** {2 List searching}
  * These functions have an optional [rev] argument. If set to [true], searching
  * is performed in reverse order, from last to first element. *)

val find : ?rev:bool -> ('a -> bool) -> 'a fcdll -> 'a option
(** [Fcdll.find p t] returns the first element of list [t] that satisfies the 
  * predicate [p]. Returns [None] if there is no value that satisfies [p] in the
  * list [t]. *)

val find_all : ?rev:bool -> ('a -> bool) -> 'a fcdll -> 'a fcdll
(** [Fcdll.find_all p t] returns all the elements of the list [t] that satisfy 
  * the predicate [p]. The order of the elements in the input list is 
  * preserved. *)

val partition : ?rev:bool -> ('a -> bool) -> 'a fcdll -> 'a fcdll * 'a fcdll
(** [Fcdll.partition p t] returns a pair of lists [(t1, t2)], where [t1] is the 
  * list of all the elements of [t] that satisfy the predicate [p], and [t2] is 
  * the list of all the elements of [t] that do not satisfy [p]. The order of 
  * the elements in the input list is preserved. *)

val index : ?rev:bool -> ('a -> bool) -> 'a fcdll -> int
(** [Fcdll.index p t] returns the index of the first value that satisfies
  * predicate [p] in list [t].
  * @raise Not_found if there is no value that satisfies [p] in list [t]. *)

val indexes : ?rev:bool -> ('a -> bool) -> 'a fcdll -> int fcdll



(** {2 List scanning} 
  * Scanning functions have an optional [rev] argument which determine the sense
  * of the traversal. Use [true] for reverse order. Default value is [false]. *)

val for_all : ?rev:bool -> ('a -> bool) -> 'a fcdll -> bool
(** [Fcdll.for_all f t] checks if all elements of the given list [t] satisfy the
  * predicate f. That is, it returns [(f t1) && (f t2) && ... && (f tn)]. *)

val exists : ?rev:bool -> ('a -> bool) -> 'a fcdll -> bool
(** [Fcdll.exists f t] checks if at least one element of the list satisfies the 
  * predicate [f]. That is, it returns [(f t1) || (f t2) || ... || (f tn)]. *)

val for_all2 : ?rev:bool -> ('a -> 'b -> bool) -> 'a fcdll -> 'b fcdll -> bool
(** Same as [Fcdll.for_all], but for a two-argument predicate.
  * @raise Invalid_argument if the two lists have different lengths. *)
  
val exists2 : ?rev:bool -> ('a -> 'b -> bool) -> 'a fcdll -> 'b fcdll -> bool
(** Same as [Fcdll.exists], but for a two-argument predicate.
  * @raise Invalid_argument if the two lists have different lengths. *)

val mem : ?rev:bool -> ?eq:('a -> 'a -> bool) -> 'a -> 'a fcdll -> bool
(** [mem a t] is true if and only if [a] is equal to an element of [t]. *)



(** {2 Association lists} 
  * The following functions can be refined with two optional parameters : [rev]
  * is used for reverse order iteration and [equal] let you choose your equality
  * function (for instance [(=)] or [(==)]). *)

val assoc :
  ?rev:bool -> 
  ?eq:('a -> 'a -> bool) -> 'a -> ('a * 'b) fcdll -> 'b
(** [assoc x t] returns the value associated with [x] in list [t], if any, or
  * raise [Not_found]. *)

val mem_assoc :
  ?rev:bool -> 
  ?eq:('a -> 'a -> bool) -> 'a -> ('a * 'b) fcdll -> bool

val split : ?rev:bool -> ('a * 'b) fcdll -> 'a fcdll * 'b fcdll
(** [Fcdll.split t] transforms the list of pairs [t] into a pair of lists. The
  * first list contains the first elements of the pairs, and the second list
  * contains the second elements of the pairs. *)

val combine : ?rev:bool -> 'a fcdll -> 'b fcdll -> ('a * 'b) fcdll
(** [Fcdll.combine t1 t2] transforms a pair of lists [t1] and [t2] into a list 
  * of pairs [(t1i, t2i)] with [0 <= i < max (length t1) (lengtht t2)].
  * @raise Invalid_argument if [is_empty t1 xor is_empty t2] is true. *)



(** {2 Iterators} *)

val iter : ?rev:bool -> ('a -> unit) -> 'a fcdll -> unit
(** [iter f t] applies function [f] in turn to all distincts elements of [t].
  * The optional parameter [rev] indicates if the iteraction is done in normal
  * (default) or reverse order. *)

val iteri : ?rev:bool -> (int -> 'a -> unit) -> 'a fcdll -> unit
(** Same as [iter], but [f] receives the index of the element as first argument. 
  * The head element has index [0]. *)

val map : ?rev:bool -> ('a -> 'b) -> 'a fcdll -> 'b fcdll
(** [map f t] builds a new list by applying function [f] to every element of 
  * [t]. *)

val mapi : ?rev:bool -> (int -> 'a -> 'b) -> 'a fcdll -> 'b fcdll
(** Same as [map], but [f] receives the index of the element as first 
  * argument. *)

val fold : ?rev:bool -> ('a -> 'b -> 'a) -> 'a -> 'b fcdll -> 'a
(** [fold f e t] computes [f (... (f (f e x0) x1) ...) xN]. The optional 
  * parameter [rev] indicates if the iteraction is done in normal (default) or 
  * reverse order. *)

val foldi : ?rev:bool -> (int -> 'a -> 'b -> 'a) -> 'a -> 'b fcdll -> 'a
(** Same as [fold], but [f] receives the index of the element as first argument. 
  * The head element has index [0]. *)

val scan : ?rev:bool -> ('a -> 'b -> 'a) -> 'a -> 'b fcdll -> 'a fcdll
(** [Fcdll.scan f e t] is similar to [Fcdll.fold], but returns the list of 
  * successive reduced values. For example, [scan f e (of_list \[x1; x2; x3\])] 
  * returns [\[e; f e x1; f (f e x1) x2; f (f (f e x1) x2) x3\]]. This function
  * is inspired from Haskell function [scan].  {b Note} : this function runs
  * fast on huge lists but be careful when trying to get values from the end of
  * the returned list. The effective computation of these values require forcing 
  * every intermediate reduced value, which can be quiet long. If you do need
  * the last value, consider using [Fcdll.fold] instead. *)

val move : (int -> 'a -> int) -> 'a fcdll -> unit
(** [move f t] applies [f] to elements of [t]. The function [f] returns [1] to 
  * go to the next element, [-1] to return to the previous element, and [0] to
  * stop the iteration. *)



(** {2 Iterators on two lists} 
  * These functions can be applied to lists of different sizes. However, if one
  * of the two lists is empty, then the second {i must} also be empty, or the
  * function will raise [Invalid_argument]. *)

val iter2 : ?rev:bool -> ('a -> 'b -> unit) -> 'a fcdll -> 'b fcdll -> unit
(** Same as [Fcdll.iter], but for a two-argument function.
  * @raise Invalid_argument if [is_empty t1 xor is_empty t2] is true. *)

val map2 : ('a -> 'b -> 'c) -> 'a fcdll -> 'b fcdll -> 'c fcdll
(** Same as [Fcdll.map], but for a two-argument function.
  * @raise Invalid_argument if [is_empty t1 xor is_empty t2] is true. *)

val fold2 : ?rev:bool -> 
  ('a -> 'b -> 'c -> 'a) -> 'a -> 'b fcdll -> 'c fcdll -> 'a
(** Same as [Fcdll.fold], but for a two-argument function.
  * @raise Invalid_argument if [is_empty t1 xor is_empty t2] is true. *)


(** {2 Sorting} 
  * Benchmark on medium-sized lists (say [50_000] elements for example) showed
  * that the current implementation is not really efficient due to heavy GC 
  * work (mainly [mark_slice], [caml_oldify_one] and [sweep_slice]). Any help 
  * in solving this problem will be appreciated. *)

val heap_sort : ?cmp:('a -> 'a -> int) -> 'a fcdll -> 'a fcdll
(** [heap_sort ~cmp:f t] sorts the elements of list [t] according to the sorting
  * function [f]. The current implementation uses Heap Sort. Implementating 
  * Merge Sort using direct adaptation from [List.sort] code is useless since it 
  * requires hard GC work. *)



(** {2 Conversion functions} 
  * Use the optional argument [rev] in the following functions to get reverse
  * order iterations. *)

val of_list : ?rev:bool -> 'a list -> 'a fcdll
(** [of_list t] builds a doubly linked list from the OCaml list [t]. *)

val to_list : ?rev:bool -> 'a fcdll -> 'a list
(** Converts the given list to OCaml list. Not tail-recursive.
  * @raise Stack_overflow if the given list is too big. *)

val of_array : ?rev:bool -> 'a array -> 'a fcdll
(** [of_array t] builds a doubly linked list from the OCaml array [t]. *)

val to_array : ?rev:bool -> 'a fcdll -> 'a array
(** Converts the given list to OCaml array. Not tail-recursive.
  * @raise Stack_overflow if the given list is too big. *)
