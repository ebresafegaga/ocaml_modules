
module type X_int = sig val x : int end 

module Increment (X : X_int) : X_int = struct 
    let x = X.x + 1 
end 

module Three = struct let x = 3 end 

module Four = Increment (Three)

let one = Four.x - Three.x

module Three_and_more = struct
    let x = 3
    let more = "more"
end 

module AnotherFour = Increment (Three_and_more)

module type Comparable = sig
    type t 
    val compare: t -> t -> int 
end 

module type Interval_intf = sig 
    type t 
    type endpoint 
    val create: endpoint -> endpoint -> t 
    val is_empty: t -> bool
    val contains: t -> endpoint -> bool
    val intersect: t -> t -> t
end 

module Make_interval (Endpoint: Comparable) : Interval_intf with type endpoint := Endpoint.t = struct 
    type t = Interval of Endpoint.t * Endpoint.t | Empty 
    type endpoint = Endpoint.t

    let create low high = 
        if Endpoint.compare low high > 0 then Empty
        else Interval (low, high)

    let is_empty = function 
        | Empty -> true 
        | Interval (x, y) -> false

    let contains t x = 
        match t with 
        | Empty -> false 
        | Interval (l, h) -> 
            Endpoint.compare l x >= 0 && Endpoint.compare h x <= 0 
    
    let intersect t1 t2 = 
        match t1, t2 with 
        | Empty, i | i, Empty -> i
        | Interval (a, b), Interval (c, d) -> 
            let low = if Endpoint.compare a c < 0 then a else c in (* min *)
            let high = if Endpoint.compare b d < 0 then d else b in (* max *)
            Interval (low, high)
end 

module IntComparable : Comparable = struct 
    type t = int32 
    let compare = Int32.compare 
end

module Int_interval = Make_interval (IntComparable)

let f = Int_interval.create

module String_interval = Make_interval (String)

module type Int_interval_intf = Interval_intf with type endpoint = int

let i1 = String_interval.create "dede" "DEde"

module type Sexpable = sig 
    type t 
end

module Fqueue = struct 
    type 'a t = 'a list * 'a list 

    let empty = ([], [])

    let enqueue (in_list, out_list) x = 
        (x :: in_list, out_list)
    
    let dequeue (in_list, out_list) = 
        match out_list with 
        | x :: xs -> Some (x, (in_list, xs))
        | [] -> 
            match List.rev in_list with 
            | [] -> None
            | x :: xs -> Some (x, ([], xs))

    let fold (in_list, out_list) ~init ~f =
        let after_out = List.fold_left f out_list init in 
        List.fold_right (fun x acc -> f acc x) in_list after_out
end 

module type S = sig 
    type 'a t
    val fold: 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
end 

module type Extension = sig 
    type 'a t
    val iter: 'a t -> f:('a -> unit) -> unit 
    val length : 'a t -> int 
    val count: 'a t -> f:('a -> bool) -> int 
    val for_all : 'a t -> f:('a -> bool) -> bool 
    val exists : 'a t -> f:('a -> bool) -> bool 
end

module Extend (Arg: S) : (Extension with type 'a t := 'a Arg.t) = struct 
    open Arg

    let iter t ~f = 
        fold t ~init:() ~f:(fun () a -> f a)

    let length t = 
        fold t ~init:0 ~f:(fun l x -> 1 + l)
    
    let count t ~f = 
        fold t ~init:0 ~f:(fun s x -> s + if f x then 1  else 0)

    exception Short_circuit 

    let for_all c ~f = 
        try iter c ~f:(fun x -> if not (f x) then raise Short_circuit else ()); true 
        with Short_circuit -> false 

    let exists l ~f = 
        try iter l ~f:(fun x -> if f x then raise Short_circuit else ()); true 
        with Short_circuit -> false
end 

module type FqueueExtsig = sig 
    
    include (module type of Fqueue)
    include Extension with type 'a t := 'a t 

end

module FqueueExt = struct 
    include Fqueue

end