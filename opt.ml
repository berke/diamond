(* Opt *)
(* $Id: opt.ml,v 1.5 2002/10/16 15:35:54 durak Exp $ *)

let dump_intermediate = ref false
let dump_linearized = ref false
let dump_values = ref false
let dump_tapes = ref false
let dump_call_graph = ref false
let dump_code = ref false
let trace = ref false
let run : string option ref = ref None
let filter = ref false
let no_output = ref false
let negate = ref false

let setopt x y = x := Some(y)

let specs = [
  "-dump-intermediate",Arg.Set(dump_intermediate),"dump intermediate code";
  "-dump-linearized",Arg.Set(dump_linearized),"dump linearized code";
  "-dump-values",Arg.Set(dump_values),"dump values";
  "-dump-tapes",Arg.Set(dump_linearized),"dump tapes";
  "-dump-call-graph",Arg.Set(dump_call_graph),"dump call graph";
  "-dump-code",Arg.Set(dump_code),"dump code";
  "-run",Arg.String(setopt run),"run compiled automaton on given input string with default translation table";
  "-filter",Arg.Set(filter),"filter lines from stdin using compiled automaton with default translation table";
  "-negate",Arg.Set(negate),"negate filter";
  "-trace",Arg.Set(trace),"trace execution when running";
  "-no-output",Arg.Set(no_output),"suppress output from automaton"
]

