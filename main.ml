let version = "0.0.1"

type options = { input : string; output : string option; compile : bool; trace : bool; verbose : bool }

let file f c b = let r = b f in c f; r

let parse filename =
	if filename = "" then exit 0;
	file (open_in filename) close_in (fun f ->
		Parser.parseprolog Lexer.token (Lexing.from_channel f)
	)

let compile (g, p) = (Compiler.compileGoal g 0, Compiler.compileProg p)

let emit opt ir = 
	match opt.output with
	| Some filename         -> file (open_out filename) close_out (Emit.emitProg ir)
	| None when opt.verbose -> Emit.emitProg ir stdout
	| _                     -> ()

let run opt (g, p) =
  if opt.compile then () else
  Vm.run opt.trace p g |>
	List.iter (fun (v,i) -> Printf.printf "%s=%s\n" v (Trace.dumpCell i))

let () =
  let opt = ref { input = ""; output = None; compile = false; trace = false; verbose = false } in
  let options = Arg.align [
    "-i", Arg.String (fun a-> opt:={!opt with input   = a}),      "<file> Input file";
    "-o", Arg.String (fun a-> opt:={!opt with output  = Some a}), "<file> Output file";
    "-c", Arg.Unit   (fun _-> opt:={!opt with compile = true}),   " Compile only (do not run)";
    "-t", Arg.Unit   (fun _-> opt:={!opt with trace   = true}),   " Show trace";
    "-v", Arg.Unit   (fun _-> opt:={!opt with verbose = true}),   " Verbose mode";
    "-V", Arg.Unit   (fun _-> raise(Arg.Help (version^"\n"))),    " Show version";
  ] in
  Arg.parse options (fun _ -> raise (Arg.Bad "Command arguments error")) "Usage: wamo <options>";
  let ir = compile (parse !opt.input) in
  emit !opt ir;
  run !opt ir
