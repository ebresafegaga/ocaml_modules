

module type X_int = sig val x: int end 

module Three : X_int = struct let x = 3 end 

let var = Three.x

let three = (module Three : X_int)

module Four : X_int = struct let x = 4 end 

let numbers = [three; (module struct let x = 45 end) ]

module T3 = (val three: X_int)

let f = T3.x

module type Y_int = X_int

let five = (module struct let x = 5 end : Y_int)

let ns = [three; five]


let to_int m =
    let module M = (val m: X_int) in 
    M.x

let to_int2 ((module M : X_int) as q) = 
    M.x, q

let plus m n =
    let module A = (val m : X_int) in 
    let module B = (val n : X_int) in 
    (module struct let x = A.x + B.x end: X_int)

let six = plus three three

module List = ListLabels


let v = to_int (List.fold_left ~init:six ~f:plus [three; three])  

module type Bumpable = sig 
    type t 
    val bump: t -> t
end 

module Int_bumper  = struct 
    type t = int 
    let bump n = n + 1
end 

module Float_bumper = struct 
    type t = float 
    let bump n = n +. 1.
end 

let int_bumper = (module Int_bumper : Bumpable with type t = int)

let float_bumper = (module Float_bumper : Bumpable with type t = float)

let a = 
    let (module Bumpable) = int_bumper in Bumpable.bump 4

let bump_list (type a) 
              (module B: Bumpable with type t = a) 
              (l: a list) =
    List.map ~f:B.bump l

let l = bump_list int_bumper [1;3;4;5]

let wrap_in_list (type a) (x:a) = [x]


module type Comparable = sig 
    type t 
    val compare: t -> t -> int 
end 

let create_comparable (type a) compare = 
    (module struct type t = a 
            let compare = compare end : Comparable with type t = a)

let c1 = create_comparable Int32.compare
let c2 = create_comparable Int64.compare

type universal = (module Comparable) list
