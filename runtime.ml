open Wam
open Runtime_mem
open Runtime_trace

type 'c wamResult = (Prolog.varId * 'c) list

let create_unbound i = change_cell i (Var i)
let push_structure (f,n) =
  let h = !state.reg_h in
  change_cell (h+1) (Str (h+2));
  change_cell (h+2) (Struct (f,n));
  for i=h+3 to h+n+2 do create_unbound i done;
  state := {!state with reg_h = h + n + 2; reg_s = h + 3}

(* code control *)

let jump p' = state := {!state with reg_p = p'}
let advance n = jump (!state.reg_p + n)
let readinstr n = get_instr n
let set_return_address () = state := {!state with reg_c = !state.reg_p}
let set_structure_pointer s = state := {!state with reg_s = s}

(* register management *)

let save_arg    i a = change_cell i (get_temp a)
let restore_arg i a = set_temp a (get_cell i)

(* arity management *)

let set_arity n = state := {!state with reg_a = n}
let next_arg () = state := {!state with reg_s = !state.reg_s + 1}

(* error *)

let unexpected msg = failwith msg
let hasChoicePoint () = !state.reg_b > 1000

(* unify *)
let rec deref a =
  match a with
  | Var x ->
    let unBound x = function 
      | Var y -> x = y
      | _ -> false
    in
    let c = get_cell x in
    if not (unBound x c) then deref c else a
  | _ -> a

let bind a x =
  let trail a =
    if hasChoicePoint () then (
      let Addr ss = get_cell (!state.reg_b-2) in
      if a < ss || a <= !state.reg_b then (
        let t = !state.reg_t in
        state := {!state with reg_t = t + 1};
        change_cell (t + 1) (Addr a)
      )
    )
  in
  trail a;
  change_cell a x

let bind' i x =
  match i with
  | Temp i -> set_temp i x
  | Perm i -> bind (get_perm_real_addr i) x

let rec unify_lists x y:bool =
	let precedes x y =
		match x,y with
		| Var x, Var y -> x < y
		| _, _ -> unexpected "cell content not variables"
	in
	(* occur check not implemented *)
	let occursin x y = false in
  match x,y with
  | [],_ -> true
  | x::xs, y::ys ->
    let unify' x y = match x,y with
      | Var a,Var b ->
        (if precedes x y then bind b x else bind a y);
        unify_lists xs ys
      | Var a, y -> if occursin x y then false else (bind a y ; unify_lists xs ys)
      | x, Var b -> if occursin y x then false else (bind b x ; unify_lists xs ys)
      | Str a, Str b ->
        let Struct (f,n) = get_cell a in
        let Struct (g,m) = get_cell b in
        if f = g && n = m then (
          let fs = get_cells (a+1) n in
          let gs = get_cells (b+1) m in
          unify_lists (fs @ xs) (gs @ ys)
        ) else false
      | _, _ -> false
    in
    let x' = deref x in
    let y' = deref y in
    if x' = y' then unify_lists xs ys else unify' x' y'

(* instructions *)

let allocate n =
  let k = max !state.reg_b !state.reg_e in
  let c = !state.reg_c in
  let e = !state.reg_e in
  for i=k+1 to k+n do create_unbound i done;
  change_cell (k+n+1) (Addr c);
  change_cell (k+n+2) (Addr e);
  state := {!state with reg_e = k + n + 2}

let deallocate () =
  let k = !state.reg_e in
  (*(Addr k)  = get_cell e*)
  let Addr e' = get_cell k in
  let Addr c' = get_cell (k-1) in
  state := {!state with reg_e = e'; reg_c = c'}

let proceed () = jump !state.reg_c

let try_me_else p =
  let a = !state.reg_a in
  let k = max !state.reg_b !state.reg_e in
  for i=1 to a do save_arg (k+i) i done;
  let ka = k + a in
  save_reg (ka+1) !state.reg_a;
  save_reg (ka+2) !state.reg_e;
  save_reg (ka+3) !state.reg_t;
  save_reg (ka+4) !state.reg_c;
  save_reg (ka+5) !state.reg_h;
  save_reg (ka+6) !state.reg_b;
  change_cell (ka+7) (Addr p);
  state := {!state with reg_b = ka + 7}

let retry_me_else p =
  change_cell !state.reg_b (Addr p)

let trust_me_else_fail () =
  let Addr b' = get_cell (!state.reg_b-1) in
	state := {!state with reg_b = b'}

let backtrack () =
	let unwind t =
		let t' = !state.reg_t in
		if t' > t then (
			let ts = List.map get_cell (Util.gen (t+1) t') in
			List.iter create_unbound (List.map (fun (Addr a)->a) ts);
			state := {!state with reg_t = t}
		)
	in
  let k = !state.reg_b in
  let Addr p = get_cell k in
  (*let Addr b = get_cell (k-1) in*)
  let Addr h = get_cell (k-2) in
  let Addr c = get_cell (k-3) in
  let Addr t = get_cell (k-4) in
  let Addr e = get_cell (k-5) in
  let Addr a = get_cell (k-6) in
  for i=1 to a do restore_arg (k-7-a+i) i done;
  unwind t;
  state := {!state with reg_p = p; reg_h = h; reg_c = c; reg_e = e; reg_a = a}

let execute (q,n) =
	let procedure_address q =
		let preds = !state.idx in
		List.assoc q preds
	in
  set_arity n;
  try jump (procedure_address (q,n))
  with Not_found -> backtrack ()

let call q = set_return_address () ; execute q

let execute_variable r n =
  let rec execute' x n =
    match deref x with
    | App(a, m) ->
      let app = get_cell a in
      for i=1 to m do restore_arg (a+m+1-i) (n+i) done;
      execute' app (n+m)
    | Struct str -> execute str
    | Var v -> unexpected "Uninstantiated higher order variable"
  in
  execute' (get_content r) n

let call_variable x n =
  set_return_address (); execute_variable x n

let unify (x:wamCell) (y:wamCell):unit =
	if not (unify_lists [x] [y]) then backtrack ()

let unify_constant c =
  let content = get_cell !state.reg_s in
  next_arg ();
	unify c content
let unify_value z =
  let b = get_cell !state.reg_s in
  next_arg ();
  unify (get_content z) b
let unify_variable z =
  bind' z (get_cell (!state.reg_s));
	next_arg ()

let get_constant (c:wamCell) (z:wamRegister):unit = unify c (get_content z)
let get_variable z x = bind' z (get_content x)
let get_value x z = unify (get_content x) (get_content z)
let get_structure str x =
  let dx = deref (get_content x) in
  match dx with
  | Var _ ->
    let h = !state.reg_h in
    push_structure str;
    unify dx (get_cell (h+1))
  | Str a ->
    if Struct str = get_cell a then (set_structure_pointer a ; next_arg ())
    else backtrack ()
	| _ -> backtrack ()

let put_value z x =	bind' x (get_content z)

let put_unsafe_value y x =
	let is_unstable = function
		| Var r -> let Addr e' = get_cell !state.reg_e in e' < r
		| _ -> false
	in
	let stabilize a b =
		let h = !state.reg_h in
		create_unbound (h+1);
		state := {!state with reg_h = h+1};
		let c = get_cell (h+1) in
		bind' a c;
		bind b c
	in
  let dy = deref (get_content y) in
  if is_unstable dy then let Var r = dy in stabilize x r
  else bind' x dy

let put_variable z x =
	let stabilize' u v =
		let h = !state.reg_h in
		create_unbound (h+1);
		state := {!state with reg_h = h+1};
		let c = get_cell (h+1) in
		bind' u c;
		bind' v c
	in
  match z with
  | Temp i -> stabilize' z x
  | Perm i ->
    let a = get_perm_real_addr i in
    create_unbound a;
    let a' = get_cell a in
    set_content z a';
    (*set_content x a';*)
		bind' x a'

let put_structure f z =
  let h = !state.reg_h in
  push_structure f;
	bind' z (get_cell (h+1))

let put_constant c z = bind' z c

let sem : wamInstr -> unit =
  function 
  | (Allocate n, _)         -> allocate n
  | (Deallocate, _)         -> deallocate ()
  | (Proceed, _)            -> proceed ()
  | (TryMeElse n, _)        -> try_me_else n
  | (RetryMeElse n, _)      -> retry_me_else n
  | (TrustMe, _)            -> trust_me_else_fail ()
  | (Backtrack, _)          -> backtrack ()
  | (Execute s, _)          -> execute s
  | (Call s, _)             -> call s
  | (UnifyConstant c,_)     -> unify_constant (Cons c)
  | (UnifyValue, [x])       -> unify_value x
  | (UnifyVariable, [x])    -> unify_variable x
  | (GetConstant s, [z])    -> get_constant (Cons s) z
  | (GetVariable, [x;z])    -> get_variable x z
  | (GetValue, [x;z])       -> get_value x z
  | (GetStructure f, [x])   -> get_structure f x
  | (PutValue, [x;z])       -> put_value x z
  | (PutUnsafeValue, [x;z]) -> put_unsafe_value x z
  | (PutVariable, [x;z])    -> put_variable x z
  | (PutStructure f, [x])   -> put_structure f x
  | (PutConstant c, [x])    -> put_constant (Cons c) x
  | x,_                     -> unexpected ("unknown instruction " (* ^ show x todo *))

let wamExecute trace p (vars, goal) =
  let g_arity = List.length vars in
  let g_addr  = List.length p.wamCode + 1 in
  let init_prog (i:wamInstrSeq):unit =
    init_code i;
    state := {!state with idx = p.wamIndex; reg_p = 0; reg_c = 0; reg_s = 0}
  in
  let init () =
    init_prog (p.wamCode @ goal);
    init_mem (Util.pow 2 20) 100
  in
  let init_goal () =
    set_arity g_arity;
    for i = 1 to g_arity do create_unbound i ; restore_arg i i done;
    state := {!state with reg_h = !state.reg_h + g_arity};
    jump g_addr
  in
	let rec loop () =
		if trace then traceCommand (!state.reg_p, get_instr !state.reg_p);
		let (i:wamInstr) = readinstr !state.reg_p in
		advance 1;
		sem i;
    if not (!state.reg_p = 0) then loop () (* (!state.reg_p <= m) loop *)
  in
  init ();
  init_goal ();
  loop ();
  Util.zip vars (get_cells 1 g_arity)
