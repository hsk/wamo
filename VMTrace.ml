open WAMIR
open WAMEmit
open VMMemory

(* trace *)

let rec deref' (a:wamCell) =
  let unBound x = function
    | Var y -> x = y
    | _ -> false
  in
  match a with
  | Var x ->
    let (c:wamCell) = get_cell x in
    if not (unBound x c) then deref' c else a
  | _ -> a

let rec pprCell i =
    let v = deref' i in
    match v with
    | Var i -> Printf.sprintf "X%d" i
    | Str i ->
      let Struct (funct, arity) = get_cell i in
      let cs = get_cells (i+1) arity in
      let ss = List.map pprCell cs in
      Printf.sprintf "%s(%s)" funct (String.concat ", " ss)
    | Cons a -> a
    | _ -> failwith ("cannot print " ^ show v)

let dumpCell i = pprCell i

let trim n p =
  if String.length p <= n then p else String.sub p 0 (n-3) ^ "..."

let traceRegs regs =
    let regReps = List.map (fun (r, c) -> let c' = pprCell c in (pprReg r, c')) regs in
    Printf.sprintf "[%s]" (String.concat "," (List.map (fun (r,c)-> Printf.sprintf "%s = %s" r (trim 20 c)) regReps))

let traceCommand (p,i) =
    let cont = List.map get_content (getReg i) in
    let regs = traceRegs (Util.zip (getReg i) cont) in
    Printf.printf "%-5d" p;
    Printf.printf "%-35s" (pprInstr i);
    Printf.printf "%s\n" regs
