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

(* WAM Compilation *)

type symbolTable = (Ast.varId * register) list
type compEnv = {
  perms'    : Ast.varId list  (* permanent variables *)
}
type compState = {
  symbolTbl : symbolTable;    (* mapping between clause variables and registers *)
  unsafe'   : Ast.varId list; (* unsafe variables *)
}
let env   = ref { perms' = [] }
let state = ref { symbolTbl = []; unsafe' = [] }

let compile f =
  state := { symbolTbl = []; unsafe' = [] };
  env := { perms' = [] };
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
	let pe = !env.perms' in
	let p = if List.mem v pe then newPerm () else newTemp r n in
	state := {!state with symbolTbl = (v,p)::!state.symbolTbl};
	p

let rec compileLit : bool          (* h is a bool - if true then compilation is a "get" else is a "put" mode *)
              -> Ast.term list     (* a list of literals to compile *)
              -> register list  (* a list of wam registers to assign to literals (one register for one literal) *)
              -> int               (* a maximum integer used to assign new variables *)
              -> instrSeq =     (* the output sequence of wam instructions *)
  fun h t r n -> match h,t,r,n with
  | h,[], _, _  -> []
  | h,t::ts,r::rs,n ->
    let opValue     = if h then GetValue                else PutValue in
    let opConstant  = if h then fun x -> GetConstant x  else fun x -> PutConstant x in
    let opStructure = if h then fun x -> GetStructure x else fun x -> PutStructure x in
    let opVariable  = if h then GetVariable             else PutVariable in
    match t with
    | Ast.T (s, []) -> (opConstant s, [r]) :: compileLit h ts rs n
    | Ast.T (_, args) ->
        begin match r with (* not an argument Temp 1...Temp n is reserved for procedural calls *)
        | Temp i when i > n -> (GetStructure (termToLabel t), [r])::compileTerm h args ts rs n
        | _                 -> (opStructure  (termToLabel t), [r])::compileTerm h args ts rs n
        end
    | Ast.V v ->
      try
        let z = List.assoc v !state.symbolTbl in
        if List.mem v !state.unsafe' then (
          state := {!state with unsafe' = Util.delete v !state.unsafe'};
          (PutUnsafeValue, [z;r]) :: compileLit h ts rs n
        ) else (opValue,   [z;r]) :: compileLit h ts rs n
      with Not_found ->
        let z = newVar (r::rs) n v in
				(opVariable, [z;r]) :: compileLit h ts rs n

and compileTerm : bool             (* h is a bool - if true then compilation is a "get" else is a "put" mode *)
               -> Ast.term list    (* a list of terms to compile *)
               -> Ast.term list    (* a list of literals to continue compilation after the compilation of the first argument *)
               -> register list (* a list of wam registers to assign to literals (one register for one literal) *)
               -> int              (* a minimum lower bound integer used to assign new variables *)
               -> instrSeq =    (* the output sequence of wam instructions *)
  fun h l ts rs n ->
  match h,l,ts,rs,n with
  | h,[],ts,rs,n     -> compileLit h ts rs n
  | h,a::as_,ts,rs,n ->
    match a with
    | Ast.T (s,[]) -> (UnifyConstant s, []) :: compileTerm h as_ ts rs n
    | Ast.T (s, args) ->
      let r' = newTemp rs n in
      (UnifyVariable, [r']) :: compileTerm h as_ (a::ts) (r'::rs) n
    | Ast.V v ->
      try
        let z = List.assoc v !state.symbolTbl in
        (UnifyValue, [z]) :: compileTerm h as_ ts rs n
      with Not_found ->
        let z = newVar rs n v in
        (UnifyVariable, [z])::compileTerm h as_ ts rs n

let compileHeadLit ts n =
  let n' = List.length ts in
  let xs = List.map (fun x -> Temp x) (List.rev (Util.gen 1 n')) in
  compileLit true ts xs n

let compileGoalLit ts =
  let n  = List.length ts in
  let xs = List.map (fun x -> Temp x) (List.rev (Util.gen 1 n)) in
  compileLit false ts xs n

let rec compileBody l e =
  match l,e with
  | [],_  -> [(Proceed, [])]
  | [g], e ->
    let c = [(Execute (termToLabel g), [])] in
    let c' = if e then (Deallocate,[])::c else c in
    compileGoalLit (Ast.args g) @ c'
  | g::gs, e ->
      let c = compileGoalLit (Ast.args g) in
      let c' = c @ [(Call (termToLabel g), [])] in
      state := {!state with symbolTbl = List.filter (fun x -> isPerm(snd x)) !state.symbolTbl};
      c' @ compileBody gs e

let compileClause ((h,b) as cl) =
  (* n registers are reserved for arguments of first literal in body (?) *)
  let isFact       = List.length b < 1 in
  let notSingleton = List.length b > 1 in
  let n = if isFact then 0 else List.length (Ast.args (List.hd b)) in
  let headArgs = Ast.args h in
  let permans = perms cl in
  let unsafes = unsafe cl in
  local env (fun _ -> {perms' = permans}) (fun () ->
    state := {symbolTbl = []; unsafe' = unsafes};
    let g = compileHeadLit headArgs n in
    let g' = if notSingleton then (Allocate (List.length permans), [])::g else g in
    g' @ compileBody b notSingleton
  )

let rec compileAlters (l::ls) i =
  let c = compileClause l in
  match ls with
  | [] -> (TrustMe,[])::c
  | _  ->	let j = i + List.length c + 1 in
					(RetryMeElse j,[])::c @ compileAlters ls j

let compilePredicate l i =
  match l with
  | []     -> [(Backtrack, [])]
  | [d]    -> compileClause d
  | d::ds  ->	let c  = compileClause d in
							let j  = i + List.length c + 1 in
							(TryMeElse j, [])::c @ compileAlters ds j 

let rec compileDefs : label list    (* list of predicate names to compile *)
									 -> Ast.clause list  (* clauses of program *)
									 -> int              (* offset to start *)
									 -> instrSeq list (* returns a list of instruction sequence, one for each predicate *)
  = fun l p i -> match l with
    | [] -> []
    | q::qs ->
      let c = compilePredicate (Ast.defs p q) i in
      let j = i + List.length c in
      c :: compileDefs qs p j

let compileProg : Ast.clause list -> program =
  fun p ->
    let ps = Ast.preds p in
    let cs = compile (fun () -> compileDefs ps p 1) in
    mkDB (Util.zip ps cs)

let compileGoal : Ast.goal -> int -> goal =
  fun g i ->
    let vg  = Ast.varsGoal g in
    let vg' = List.map (fun v -> Ast.V v) vg in
    let g'  = (Ast.T ("?", vg'), g) in
    (List.rev vg, compile (fun () -> compilePredicate [g'] i))
