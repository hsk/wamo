let rec nub = function
  | []          ->  []
  | x::xs       ->  x :: nub (List.filter (fun y -> not (x = y)) xs)

let concatMap f ls = List.concat (List.map f ls)

let rec init : 'a list-> 'a list = function
  | []    -> failwith "empty list"
  | [_]   -> []
  | x::xs -> x::init xs

let rec last : 'a list-> 'a = function
  | []    -> failwith "empty list"
  | [x]   -> x
  | _::xs -> last xs

let subtract a b =
  let rec loop = function
    | [] -> []
    | x::xs when List.mem x b -> loop xs
    | x::xs -> x::loop xs
  in
  loop a

let rec delete a = function
  | [] -> []
  | x::xs when a = x -> xs
  | x::xs -> x::delete a xs

let rec maximum x = function
  | []               -> x
  | y::ys when x > y -> maximum x ys
  | y::ys            -> maximum y ys

let rec gen s e =
  if s >= e then [e] else s:: gen (s + 1) e

let vcat = String.concat "\n"

let rec zip xs ys = match xs,ys with
  | [],_ -> []
  | _,[] -> []
  | x::xs,y::ys -> (x,y)::zip xs ys

let rec pow a = function
  | 1 -> a
  | n -> a * pow a (n - 1)
