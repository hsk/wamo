open Ir

let perms : Ast.clause -> Ast.varId list =
  fun (t, ts) ->
    let varsHead = Ast.varsTerm t in
    let varsBody = List.map Ast.varsTerm ts in
    let lst = match varsBody with
              | [] -> [varsHead]
              | v::vs -> (Util.nub (varsHead @ v))::vs
    in
    let rec aux = function
      | [] -> []
      | l::ls ->
        let l' = List.concat ls in
        (List.filter (fun v -> List.mem v l') l) @ aux ls
    in
    Util.nub (aux lst)

let safe : Ast.clause -> Ast.varId list = function
  | (h, []) -> Ast.varsTerm h
  | (h, b)  ->
    (*let b' = Util.init b in*)
    let l  = Util.last b in
    let vl = Ast.varsTerm l in
    let varsNotL = Util.subtract (Ast.varsClause (h,b)) vl in
    let rec inCompound = function
      | [] -> []
      | Ast.T (s,args)::ts -> Util.nub (List.map Ast.varsTerm args @ inCompound ts)
      | Ast.V _ :: ts -> inCompound ts
    in
    let varsInCompound = List.concat (inCompound (Util.concatMap Ast.args (h::b))) in
    Util.nub (Ast.varsTerm h @ varsNotL @ varsInCompound)

let unsafe : Ast.clause -> Ast.varId list =
  fun c -> Util.subtract (Ast.varsClause c) (safe c)

let unWrapVar = function
  | Temp i -> i
  | Perm i -> i

let isPerm = function
  | Perm _ -> true
  | _      -> false

let isTemp v = not (isPerm v)

let termToLabel : Ast.term -> label = function
  | Ast.T (s, args) -> (s, List.length args)
  | _               -> failwith "cannot convert to label"

let local env init body =
  let save = !env in
  env := init !env;
  let r = body () in
  env := save;
  r

type symbolTable = (Ast.varId * register) list
type compEnv = {
  perms     : Ast.varId list  (* permanent variables *)
}
type compState = {
  symbolTbl : symbolTable;    (* mapping between clause variables and registers *)
  unsafe    : Ast.varId list; (* unsafe variables *)
}
let env   = ref { perms = [] }
let state = ref { symbolTbl = []; unsafe = [] }

let compile f =
  state := { symbolTbl = []; unsafe = [] };
  env := { perms = [] };
  f ()

let newPerm () =
  let tbl = !state.symbolTbl in
  let p = List.map unWrapVar (List.filter isPerm (List.map snd tbl)) in
  let n = Util.maximum 0 p in
  Perm (n + 1)

let newTemp r m =
  let t = List.filter isTemp (List.map snd !state.symbolTbl) in
  let p = List.map unWrapVar (t @ r) in
  let n = Util.maximum m p in
  Temp (n + 1)

let newVar r n v =
  let p = if List.mem v !env.perms then newPerm () else newTemp r n in
  state := {!state with symbolTbl = (v,p)::!state.symbolTbl};
  p

(* WAM Compilation *)

let opValue     h = if h then GetValue                else PutValue
let opConstant  h = if h then fun x -> GetConstant  x else fun x -> PutConstant  x
let opStructure h = if h then fun x -> GetStructure x else fun x -> PutStructure x
let opVariable  h = if h then GetVariable             else PutVariable

let rec compileLit h n = function
  | [], _  -> []
  | Ast.T (s, [])::ts,r::rs -> (opConstant h s, [r]) :: compileLit h n (ts, rs)
  | (Ast.T (_, args)as t)::ts,(Temp i as r)::rs when i > n ->
    (GetStructure (termToLabel t), [r])::compileTerm h n (ts, rs) args
  | (Ast.T (_, args)as t)::ts,r::rs ->
    (opStructure h (termToLabel t), [r])::compileTerm h n (ts, rs) args
  | Ast.V v::ts,r::rs when List.mem_assoc v !state.symbolTbl ->
    let z = List.assoc v !state.symbolTbl in
    if List.mem v !state.unsafe then (
      state := {!state with unsafe = Util.delete v !state.unsafe};
      (PutUnsafeValue, [z;r]) :: compileLit h n (ts, rs)
    ) else (opValue h, [z;r]) :: compileLit h n (ts, rs)
  | Ast.V v::ts,r::rs ->
    let z = newVar (r::rs) n v in
    (opVariable h, [z;r]) :: compileLit h n (ts, rs)

and compileTerm h n (ts, rs) = function
  | []                  -> compileLit h n (ts, rs)
  | Ast.T (s,[])::as_   -> (UnifyConstant s, []) :: compileTerm h n (ts, rs) as_
  | (Ast.T _ as a)::as_ ->
    let r' = newTemp rs n in
    (UnifyVariable, [r']) :: compileTerm h n (a::ts, r'::rs) as_
  | Ast.V v::as_ when List.mem_assoc v !state.symbolTbl ->
    let z = List.assoc v !state.symbolTbl in
    (UnifyValue, [z]) :: compileTerm h n (ts, rs) as_
  | Ast.V v::as_ ->
    let z = newVar rs n v in
    (UnifyVariable, [z])::compileTerm h n (ts, rs) as_

let compileHead n ts =
  let n' = List.length ts in
  let xs = List.map (fun x -> Temp x) (List.rev (Util.gen 1 n')) in
  compileLit true n (ts, xs)

let compileGoal ts =
  let n  = List.length ts in
  let xs = List.map (fun x -> Temp x) (List.rev (Util.gen 1 n)) in
  compileLit false n (ts, xs)

let rec compileBody e = function
  | []  -> [(Proceed, [])]
  | [g] ->
    let c = [(Execute (termToLabel g), [])] in
    let c' = if e then (Deallocate,[])::c else c in
    compileGoal (Ast.args g) @ c'
  | g::gs ->
      let c = compileGoal (Ast.args g) in
      let c' = c @ [(Call (termToLabel g), [])] in
      state := {!state with symbolTbl = List.filter (fun x -> isPerm(snd x)) !state.symbolTbl};
      c' @ compileBody e gs

let compileClause ((h,b) as cl) =
  (* n registers are reserved for arguments of first literal in body (?) *)
  let isFact       = List.length b < 1 in
  let notSingleton = List.length b > 1 in
  let n = if isFact then 0 else List.length (Ast.args (List.hd b)) in
  let headArgs = Ast.args h in
  let permans = perms cl in
  let unsafes = unsafe cl in
  local env (fun _ -> {perms = permans}) (fun () ->
    state := {symbolTbl = []; unsafe = unsafes};
    let g = compileHead n headArgs in
    let g' = if notSingleton then (Allocate (List.length permans), [])::g else g in
    g' @ compileBody notSingleton b
  )

let rec compileAlterPredicates i = function
  | [l]   -> (TrustMe,[])::compileClause l
  | l::ls -> let c = compileClause l in
             let j = i + List.length c + 1 in
             (RetryMeElse j,[])::c @ compileAlterPredicates j ls

let compilePredicate i = function
  | []     -> [(Backtrack, [])]
  | [d]    -> compileClause d
  | d::ds  -> let c = compileClause d in
              let j = i + List.length c + 1 in
              (TryMeElse j, [])::c @ compileAlterPredicates j ds

let rec compileDefs p i = function
  | []    -> []
  | q::qs -> let c = compilePredicate i (Ast.defs p q) in
             c :: compileDefs p (i + List.length c) qs

let compileProg : Ast.clause list -> program =
  fun p ->
    let ps = Ast.preds p in
    let cs = compile (fun () -> compileDefs p 1 ps) in
    mkDB (Util.zip ps cs)

let compileGoal : Ast.goal -> int -> goal =
  fun g i ->
    let vg  = Ast.varsGoal g in
    let vg' = List.map (fun v -> Ast.V v) vg in
    let g'  = (Ast.T ("?", vg'), g) in
    (List.rev vg, compile (fun () -> compilePredicate i [g']))
