open Ir

let auxPred name arity = Printf.sprintf "%s/%d" name arity
let auxInstr name args = Printf.sprintf "%s(%s)" name (String.concat ", " args)

let pprReg = function
  | Temp i -> Printf.sprintf "x(%d)" i
  | Perm i -> Printf.sprintf "y(%d)" i
let pprRegs xs = List.map pprReg xs
let pprInstr = function
  | (Allocate n, _)          -> auxInstr "allocate" [string_of_int n]
  | (Deallocate, _)          -> auxInstr "deallocate" []
  | (Execute (p,n), _)       -> auxInstr "execute" [auxPred p n]
  | (Call (p,n), _)          -> auxInstr "call" [auxPred p n]
  | (Proceed, [])            -> auxInstr "proceed" []
  | (GetStructure (s,n), xs) -> auxInstr "get_structure" ((auxPred s n)::pprRegs xs)
  | (PutStructure (s,n), xs) -> auxInstr "put_structure" ((auxPred s n)::pprRegs xs)
  | (GetConstant  s, xs)     -> auxInstr "get_constant" (s::pprRegs xs)
  | (PutConstant  s, xs)     -> auxInstr "put_constant" (s::pprRegs xs)
  | (UnifyConstant  s, xs)   -> auxInstr "unify_constant" (s::pprRegs xs)
  | (GetVariable, xs)        -> auxInstr "get_variable" (pprRegs xs)
  | (PutVariable, xs)        -> auxInstr "put_variable" (pprRegs xs)
  | (UnifyVariable, xs)      -> auxInstr "unify_variable" (pprRegs xs)
  | (GetValue, xs)           -> auxInstr "get_value"    (pprRegs xs)
  | (PutValue, xs)           -> auxInstr "put_value"    (pprRegs xs)
  | (PutUnsafeValue, xs)     -> auxInstr "put_unsafe_value"  (pprRegs xs)
  | (UnifyValue, xs)         -> auxInstr "unify_value"    (pprRegs xs)
  | (TryMeElse n, _)         -> auxInstr "try_me_else" [string_of_int n]
  | (RetryMeElse n, _)       -> auxInstr "retry_me_else" [string_of_int n]
  | (TrustMe, _)             -> auxInstr "trust_me_or_fail" []
let pprIdx idx =
  idx |>
  List.map (fun ((s,n),i) -> "(" ^ (String.concat ", " [auxPred s n; string_of_int i]) ^ ")") |>
  Util.vcat
let pprProg (index,code) = Printf.sprintf "%s\n%s" (Util.vcat (List.map pprInstr code)) (pprIdx index)

let emitInstr i    f = Printf.fprintf f "%s\n" (pprInstr i)
let emitProg (_,p) f = Printf.fprintf f "%s\n" (pprProg p)
