open Prolog
open Wam

type wamSymbolTable = (varId * wamRegister) list
type wamCompEnv = {
  perms'    : varId list       (* ^ permanent variables *)
}
type wamCompState = {
  symbolTbl : wamSymbolTable; (* mapping between clause variables and wamregisters *)
  unsafe'   : varId list;     (* unsafe variables *)
}
let env   = ref { perms' = [] }
let state = ref { symbolTbl = []; unsafe' = [] }

let perms : clause -> varId list =
  fun (t, ts) ->
    let varsHead = varsTerm t in
    let varsBody = List.map varsTerm ts in
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

let safe : clause -> varId list = function
  | (h, []) -> varsTerm h
  | (h, b)  ->
    (*let b' = Util.init b in*)
    let l  = Util.last b in
    let vl = varsTerm l in
    let varsNotL = Util.subtract (varsClause (h,b)) vl in
    let rec inCompound = function
      | [] -> []
      | T (s,args)::ts -> Util.nub (List.map varsTerm args @ inCompound ts)
      | V _ :: ts -> inCompound ts
    in
    let varsInCompound = List.concat (inCompound (Util.concatMap args (h::b))) in
    Util.nub (varsTerm h @ varsNotL @ varsInCompound)

let unsafe : clause -> varId list =
  fun c -> Util.subtract (varsClause c) (safe c)

let unWrapVar = function
  | Temp i -> i
  | Perm i -> i

let isPerm = function
  | Perm _ -> true
  | _      -> false

let isTemp v = not (isPerm v)

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

let termToLabel : term -> wamLabel = function
  | T (s, args) -> (s, List.length args)
  | _           -> failwith "cannot convert to wamlabel"

(* WAM Compilation *)

let rec wamCompileLit : bool       (* h is a bool - if true then compilation is a "get" else is a "put" mode *)
              -> term list         (* a list of literals to compile *)
              -> wamRegister list  (* a list of wam registers to assign to literals (one register for one literal) *)
              -> int               (* a maximum integer used to assign new variables *)
              -> wamInstrSeq =     (* the output sequence of wam instructions *)
  fun h t r n -> match h,t,r,n with
  | h,[], _, _  -> []
  | h,t::ts,r::rs,n ->
    let opValue     = if h then GetValue                else PutValue in
    let opConstant  = if h then fun x -> GetConstant x  else fun x -> PutConstant x in
    let opStructure = if h then fun x -> GetStructure x else fun x -> PutStructure x in
    let opVariable  = if h then GetVariable             else PutVariable in
    match t with
    | T (s, []) -> (opConstant s, [r]) :: wamCompileLit h ts rs n
    | T (_, args) ->
        begin match r with (* not an argument Temp 1...Temp n is reserved for procedural calls *)
        | Temp i when i > n -> (GetStructure (termToLabel t), [r])::wamCompileTerm h args ts rs n
        | _                 -> (opStructure  (termToLabel t), [r])::wamCompileTerm h args ts rs n
        end
    | V v ->
      try
        let z = List.assoc v !state.symbolTbl in
        if List.mem v !state.unsafe' then (
          state := {!state with unsafe' = Util.delete v !state.unsafe'};
          (PutUnsafeValue, [z;r]) :: wamCompileLit h ts rs n
        ) else (opValue,        [z;r]) :: wamCompileLit h ts rs n
      with Not_found ->
        let z = newVar (r::rs) n v in
        (opVariable, [z;r]) :: wamCompileLit h ts rs n
and wamCompileTerm : bool               (* h is a bool - if true then compilation is a "get" else is a "put" mode *)
               -> term list             (* a list of terms to compile *)
               -> term list             (* a list of literals to continue compilation after the compilation of the first argument *)
               -> wamRegister list      (* a list of wam registers to assign to literals (one register for one literal) *)
               -> int                   (* a minimum lower bound integer used to assign new variables *)
               -> wamInstrSeq =         (* the output sequence of wam instructions *)
  fun h l ts rs n ->
  match h,l,ts,rs,n with
  | h,[],ts,rs,n     -> wamCompileLit h ts rs n
  | h,a::as_,ts,rs,n ->
    match a with
    | T (s,[]) -> (UnifyConstant s, []) :: wamCompileTerm h as_ ts rs n
    | T (s, args) ->
      let r' = newTemp rs n in
      (UnifyVariable, [r']) :: wamCompileTerm h as_ (a::ts) (r'::rs) n
    | V v ->
      try
        let z = List.assoc v !state.symbolTbl in
        (UnifyValue, [z]) :: wamCompileTerm h as_ ts rs n
      with Not_found ->
        let z = newVar rs n v in
        (UnifyVariable, [z])::wamCompileTerm h as_ ts rs n

let wamCompileHeadLit ts n =
  let n' = List.length ts in
  let xs = List.map (fun x -> Temp x) (List.rev (Util.gen 1 n')) in
  wamCompileLit true ts xs n

let wamCompileGoalLit ts =
  let n  = List.length ts in
  let xs = List.map (fun x -> Temp x) (List.rev (Util.gen 1 n)) in
  wamCompileLit false ts xs n

let rec wamCompileBody l e =
  match l,e with
  | [],_  -> [(Proceed, [])]
  | [g], e ->
    let c = [(Execute (termToLabel g), [])] in
    let c' = if e then (Deallocate,[])::c else c in
    wamCompileGoalLit (args g) @ c'
  | g::gs, e ->
      let c = wamCompileGoalLit (args g) in
      let c' = c @ [(Call (termToLabel g), [])] in
      state := {!state with symbolTbl = List.filter (fun x -> isPerm(snd x)) !state.symbolTbl};
      c' @ wamCompileBody gs e

let local f f2 =
  let temp = !env in
  env := f !env;
  let r = f2 () in
  env := temp;
  r

let wamCompileClause ((h,b) as cl) =
  (* n registers are reserved for arguments of first literal in body (?) *)
  let isFact       = List.length b < 1 in
  let notSingleton = List.length b > 1 in
  let n = if isFact then 0 else List.length (args (List.hd b)) in
  let headArgs = args h in
  let permans = perms cl in
  let unsafes = unsafe cl in
  local (fun _ -> {perms' = permans}) (fun () ->
    state := {symbolTbl = []; unsafe' = unsafes};
    let g = wamCompileHeadLit headArgs n in
    let g' = if notSingleton then (Allocate (List.length permans), [])::g else g in
    g' @ wamCompileBody b notSingleton
  )

let rec wamCompileAlters (l::ls) i =
  let c = wamCompileClause l in
  match ls with
  | [] -> (TrustMe,[])::c
  | _  ->
    let j = i + List.length c + 1 in
    (RetryMeElse j,[])::c @ wamCompileAlters ls j

let wamCompilePredicate l i =
  match l with
  | []    -> [(Backtrack, [])]
  | [d]   -> wamCompileClause d
  | d::ds ->
    let c  = wamCompileClause d in
    let j  = i + List.length c + 1 in
    (TryMeElse j, [])::c @ wamCompileAlters ds j 

let rec wamCompileDefs : wamLabel list (* list of predicate names to compile *)
               -> clause list          (* clauses of program *)
               -> int                  (* offset to start *)
               -> wamInstrSeq list =   (* returns a list of instruction sequence, one for each predicate *)
  fun l p i -> match l with
    | [] -> []
    | q::qs ->
      let c = wamCompilePredicate (defs p q) i in
      let j = i + List.length c in
      c :: wamCompileDefs qs p j

let wamCompile f =
  state := { symbolTbl = []; unsafe' = [] };
  env := { perms' = [] };
  f ()

let wamCompileProg : clause list -> wamProgram =
  fun p ->
    let ps = preds p in
    let i  = 1 in
    let cs = wamCompile (fun () ->wamCompileDefs ps p i) in
    mkDB (Util.zip ps cs)

let wamCompileGoal : goal -> int -> wamGoal =
  fun g i ->
    let vg  = varsGoal g in
    let vg' = List.map (fun v -> V v) vg in
    let g'  = (T ("?", vg'), g) in
    (List.rev vg, wamCompile (fun () -> wamCompilePredicate [g'] i))
