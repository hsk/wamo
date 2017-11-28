type address = int
type label = string * int

(* WAM Operator *)
type op =
  | PutVariable
  | PutValue
  | PutUnsafeValue
  | PutStructure of label
  | PutConstant of string
  | GetStructure of label
  | GetConstant of string
  | GetValue
  | GetVariable
  | UnifyConstant of string
  | UnifyValue
  | UnifyVariable
  | Call of label
  | Execute of label
  | Proceed
  | Allocate of int
  | Deallocate
  | TryMeElse of address
  | RetryMeElse of address
  | TrustMe
  | Backtrack
  | CallVariable
  | ExecuteVariable

type register = 
  | Perm of int
  | Temp of int

let showReg = function
  | Perm i -> Printf.sprintf "Perm(%d)" i
  | Temp i -> Printf.sprintf "Temp(%d)" i

let showRegs rs = Printf.sprintf "[%s]" (String.concat "," (List.map showReg rs))

(* WAM Instruction *)

type arg = register
type instr = op * arg list

let getOp  (op, _)   = op
let getReg (_, regs) = regs

type instrSeq = instr list
type index = (label * address) list
type goal = (Ast.varId list * instrSeq)

(* WAM Program *)
type program = index * instrSeq

let mkDB : (label * instrSeq) list -> program =
  fun lst ->
    let aux (idx, code) (lbl, instr) = (idx @ [(lbl, List.length code + 1)], code @ instr) in
    List.fold_left aux ([],[]) lst
