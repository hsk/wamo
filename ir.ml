type wamAddress = int
type wamLabel = string * int

(* WAM Operator *)
type wamOp =
  | PutVariable
  | PutValue
  | PutUnsafeValue
  | PutStructure of wamLabel
  | PutConstant of string
  | GetStructure of wamLabel
  | GetConstant of string
  | GetValue
  | GetVariable
  | UnifyConstant of string
  | UnifyValue
  | UnifyVariable
  | Call of wamLabel
  | Execute of wamLabel
  | Proceed
  | Allocate of int
  | Deallocate
  | TryMeElse of wamAddress
  | RetryMeElse of wamAddress
  | TrustMe
  | Backtrack
  | CallVariable
  | ExecuteVariable

type wamRegister = 
  | Perm of int
  | Temp of int

let showReg = function
  | Perm i -> Printf.sprintf "Perm(%d)" i
  | Temp i -> Printf.sprintf "Temp(%d)" i

let showRegs rs = Printf.sprintf "[%s]" (String.concat "," (List.map showReg rs))

(* WAM Instruction *)

type wamArg = wamRegister
type wamInstr = wamOp * wamArg list

let getOp  (op, _)   = op
let getReg (_, regs) = regs

type wamInstrSeq = wamInstr list
type wamIndex = (wamLabel * wamAddress) list
type wamGoal = (Ast.varId list * wamInstrSeq)

(* WAM Program *)
type wamProgram = { wamIndex : wamIndex; wamCode : wamInstrSeq }

let mkDB : (wamLabel * wamInstrSeq) list -> wamProgram =
  fun lst ->
    let aux (idx, code) (lbl, instr) = (idx @ [(lbl, List.length code + 1)], code @ instr) in
    let (idx, code) = List.fold_left aux ([],[]) lst in
    { wamIndex = idx; wamCode = code }
