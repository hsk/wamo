open Wam

(* aux *)

let pprInstrAux name args = Printf.sprintf "%s(%s)" name (String.concat ", " args)
let pprPredAux predname arity = Printf.sprintf "%s/%d" predname arity

let pprReg = function
	| Temp i -> Printf.sprintf "x(%d)" i
	| Perm i -> Printf.sprintf "y(%d)" i

let pprRegs xs = List.map pprReg xs

(* dump *)

let pprInstr = function
	| (Allocate n, _)          -> pprInstrAux "allocate" [string_of_int n]
	| (Deallocate, _)          -> pprInstrAux "deallocate" []
	| (Execute (p,n), _)       -> pprInstrAux "execute" [pprPredAux p n]
	| (Call (p,n), _)          -> pprInstrAux "call" [pprPredAux p n]
	| (Proceed, [])            -> pprInstrAux "proceed" []
	| (GetStructure (s,n), xs) -> pprInstrAux "get_structure" ((pprPredAux s n)::pprRegs xs)
	| (PutStructure (s,n), xs) -> pprInstrAux "put_structure" ((pprPredAux s n)::pprRegs xs)
	| (GetConstant  s, xs)     -> pprInstrAux "get_constant" (s::pprRegs xs)
	| (PutConstant  s, xs)     -> pprInstrAux "put_constant" (s::pprRegs xs)
	| (UnifyConstant  s, xs)   -> pprInstrAux "unify_constant" (s::pprRegs xs)
	| (GetVariable, xs)        -> pprInstrAux "get_variable" (pprRegs xs)
	| (PutVariable, xs)        -> pprInstrAux "put_variable" (pprRegs xs)
	| (UnifyVariable, xs)      -> pprInstrAux "unify_variable" (pprRegs xs)
	| (GetValue, xs)           -> pprInstrAux "get_value"    (pprRegs xs)
	| (PutValue, xs)           -> pprInstrAux "put_value"    (pprRegs xs)
	| (PutUnsafeValue, xs)     -> pprInstrAux "put_unsafe_value"  (pprRegs xs)
	| (UnifyValue, xs)         -> pprInstrAux "unify_value"    (pprRegs xs)
	| (TryMeElse n, _)         -> pprInstrAux "try_me_else" [string_of_int n]
	| (RetryMeElse n, _)       -> pprInstrAux "retry_me_else" [string_of_int n]
	| (TrustMe, _)             -> pprInstrAux "trust_me_or_fail" []

let pprIdx idx =
	let pprIdxEntry ((s,n),i) =
		Printf.sprintf "(%s)" (String.concat ", " [ pprPredAux s n; string_of_int i ])
	in
	Util.vcat (List.map pprIdxEntry idx)

let pprProg p = Printf.sprintf "%s\n%s" (Util.vcat (List.map pprInstr p.wamCode)) (pprIdx p.wamIndex)

let wamEmitInstr i    = pprInstr i
let wamEmitProg (_,p) = pprProg p
