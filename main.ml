let version = "0.0.1"

type options = {
  input : string; output : string option; compileOnly : bool;
  trace : bool; verbose : bool;
}

let parse filename =
  if filename = "" then exit 0;
  let inp = open_in filename in
  let lexer = Lexing.from_channel inp in
  let (g, p) = Parser.parseprolog Lexer.token lexer in
  close_in inp;
  (g, p)

let compile (g, p) = 
  (Wam_compiler.wamCompileGoal g 0, Wam_compiler.wamCompileProg p)

let emit opt prog = 
  if opt.verbose || opt.output <> None then (
    let ho = match opt.output with
      | Some filename -> open_out filename
      | None          -> stdout
    in
    Printf.fprintf ho "%s\n" (Wam_emit.wamEmitProg prog);
    if ho <> stdout then close_out ho
  )

let run opt (compiledGoal, compiledProg) = 
  if opt.compileOnly then () else
  let gs = Runtime.wamExecute opt.trace compiledProg compiledGoal in
  List.iter (fun (v,i) ->
    Printf.printf "%s=%s\n" v (Runtime_trace.dumpCell i)
  ) gs

let () =
  let opt = ref {
    input = ""; output = None; compileOnly = false;
    trace = false; verbose = false;
  } in
  let options = Arg.align [
    "-i", Arg.String (fun a-> opt:={!opt with input       = a}),      "<file> Input file";
    "-o", Arg.String (fun a-> opt:={!opt with output      = Some a}), "<file> Output file";
    "-c", Arg.Unit   (fun _-> opt:={!opt with compileOnly = true}),   " Compile only (do not run)";
    "-t", Arg.Unit   (fun _-> opt:={!opt with trace       = true}),   " Show trace";
    "-v", Arg.Unit   (fun _-> opt:={!opt with verbose     = true}),   " Verbose mode";
    "-V", Arg.Unit   (fun _-> raise(Arg.Help (version^"\n"))),        " Show version";
  ] in
  Arg.parse options (fun _ -> raise (Arg.Bad "Command arguments error")) ("Usage: wamo <options>");

  let ast = parse !opt.input in
  let prog = compile ast in
  emit !opt prog;
  run !opt prog
