open Ir

(* trace *)

let rec deref' (a:State.cell) =
  let unBound x = function
    | State.Var y -> x = y
    | _ -> false
  in
  match a with
  | State.Var x ->
    let (c:State.cell) = State.get_cell x in
    if not (unBound x c) then deref' c else a
  | _ -> a

let rec pprCell i =
    let v = deref' i in
    match v with
    | State.Var i -> Printf.sprintf "X%d" i
    | State.Str i ->
      let State.Struct (funct, arity) = State.get_cell i in
      let cs = State.get_cells (i+1) arity in
      let ss = List.map pprCell cs in
      Printf.sprintf "%s(%s)" funct (String.concat ", " ss)
    | State.Cons a -> a
    | _ -> failwith ("cannot print " ^ State.show v)

let dumpCell i = pprCell i

let trim n p =
  if String.length p <= n then p else String.sub p 0 (n-3) ^ "..."

let traceRegs regs =
    let regReps = List.map (fun (r, c) -> (Emit.pprReg r, pprCell c)) regs in
    Printf.sprintf "[%s]" (String.concat "," (List.map (fun (r,c)-> Printf.sprintf "%s = %s" r (trim 20 c)) regReps))

let traceCommand (p,i) =
    let cont = List.map State.get_content (getReg i) in
    let regs = traceRegs (Util.zip (getReg i) cont) in
    Printf.printf "%-5d" p;
    Printf.printf "%-35s" (Emit.pprInstr i);
    Printf.printf "%s\n" regs
