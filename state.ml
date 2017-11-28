open Ir

type cell =
  | Struct of label          (* (f/n) *)
  | Var of address           (* REF n *)
  | Str of address           (* STR n *)
  | Cons of string           (* CONS s *)
  | App of address * int     (* APP n m *)
  | Addr of address

let show = function
  | Struct (s,l) -> Printf.sprintf "Struct(%S,%i)" s l
  | Var i -> Printf.sprintf "Var(%d)" i
  | Str i -> Printf.sprintf "Str(%d)" i
  | Cons x -> Printf.sprintf "Cons(%S)" x
  | App(a,i) -> Printf.sprintf "App(%d,%d)" a i
  | Addr a -> Printf.sprintf "Addr(%d)" a

type mem = cell array
type code = instr array

type state = {
  idx   : index;   (* predicate index *)
  mem   : mem;     (* global space of memory *)
  code  : code;    (* instructions *)
  regs  : mem;     (* registers *)
  reg_p : address; (* register pointing  to code *)
  reg_t : address; (* register pointing at the top of trail *)
  reg_c : address; (* register to hold the last code before a call *)
  reg_h : address; (* register pointing at the top of heap (global stack) *)
  reg_b : address; (* register pointing at the top of backtrack (local stack) *)
  reg_e : address; (* register pointing at the top of the environment (local stack) *)
  reg_a : int;     (* register holding the arity of the argument *)
  reg_s : address; (* structure pointer *)
  }

let emptyState : state = {
    idx = []; mem  = [||]; code = [||]; regs = [||];
    reg_p = 0; reg_c = 0; reg_b = 0; reg_s = 0;
    reg_a = 0; reg_t = 0; reg_h = 0; reg_e = 0;
  }

let state = ref emptyState

let init_mem arraysize regnum =
  let startHeap     = 0 in
  let startAndStack = startHeap + 1000 in
  let startOrStack  = startAndStack in
  let startTrail    = startAndStack + 1000 in
  state := { !state with
    mem   = Array.make (arraysize+1) (Var 0);
    regs  = Array.make (regnum+1) (Var 0);
    reg_e = startAndStack; reg_b = startOrStack;
    reg_h = startHeap;     reg_t = startTrail;
  }

let init_code i =
  let c = Array.make (1024+1) (Backtrack,[]) in
  let rec loop n = function
    | [] -> ()
    | x::xs -> c.(n) <- x; loop (n+1) xs
  in
  loop 1 i;
  state := {!state with code = c }

(* memory management *)

let change_cell i c = !state.mem.(i) <- c
let get_cell i = !state.mem.(i)
let get_instr n = !state.code.(n)
let get_cells start count = List.map get_cell (Util.gen start (start+count-1))
let save_reg i r = change_cell i (Addr r)
let get_temp i = !state.regs.(i)
let set_temp i c = !state.regs.(i) <- c
let get_perm_real_addr i = !state.reg_e-1-i
let get_perm i = get_cell (get_perm_real_addr i)
let set_perm i c = change_cell (get_perm_real_addr i) c
let get_content = function Perm i -> get_perm i | Temp i -> get_temp i
let set_content = function Perm i -> set_perm i | Temp i -> set_temp i
