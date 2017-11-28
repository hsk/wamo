type varId = string
type term =
  | T of string * term list
  | V of varId
  | V2 of varId * term list
type clause  = term * term list
type goal    = term list
type program = goal * clause list

let rec show = function
  | T(s,ts) -> Printf.sprintf "T(%S,%s)" s (show_terms ts)
  | V(s) -> Printf.sprintf "V(%S)" s
  | V2(s,ts) -> Printf.sprintf "V2(%S,%s)" s (show_terms ts)
and show_terms ts = "[" ^ String.concat "," (List.map show ts) ^ "]"
let show_clause (t,ts) = Printf.sprintf "(%s,%s)" (show t) (show_terms ts)
let show_clauses cs = "[" ^ String.concat "," (List.map show_clause cs) ^ "]"
let show_goal g = show_terms g
let show_program (g,cs) = Printf.sprintf "(%s,%s)" (show_goal g) (show_clauses cs)

let varsTerm t =
  let rec vars' = function
    | T (_, ts) -> Util.concatMap vars' ts
    | V v       -> [v]
  in
  Util.nub (vars' t)

let varsClause (t, ts) = Util.nub (varsTerm t @ Util.concatMap varsTerm ts)
let varsGoal ts = Util.nub (Util.concatMap varsTerm ts)
let preds cs = Util.nub (List.map (fun (T (s,args),_) -> (s, List.length args)) cs)
let defs cs (p,n) = List.filter (fun (T (s,args),_) -> s = p && List.length args = n) cs
let args (T (_, x)) = x
