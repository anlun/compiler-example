open Ostap
open Matcher

module Expr =
  struct
    type t  =
      | Const of int
      | Var   of string
      | Binop of string * t * t
               
    exception UnsupportedOperation
               
    let apply s =
      match s with
      | "+" -> (+)
      | "-" -> (-)
      | "*" -> ( * )
      | "/" -> (/)
      | _ -> raise UnsupportedOperation
               
    let rec eval (varst : string -> int) t =
      match t with
      | Const n -> n
      | Var   v -> varst v
      | Binop (s, lexpr, rexpr) ->
         let lval = eval varst lexpr in
         let rval = eval varst rexpr in
         apply s lval rval

    ostap (
      parse: add;

      add: l:mul suf:(("+"|"-") mul)* { List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf };
      mul: l:pri suf:(("*"|"/") pri)* { List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf };

      pri:
        v:DECIMAL { Const v }
      | v:IDENT   { Var   v }
      | -"(" parse -")"
    )
  end

module Stmt =
  struct
    type t  =
      | Skip
      | Write  of Expr.t
      | Read   of string
      | Assign of string * Expr.t
      | Seq    of t * t

    ostap (
      parse:
        l:stmt suf:(-";" r:stmt)* { List.fold_left (fun l r -> Seq (l, r)) l suf };
      
      eread:
        %"read" "(" ")" { fun x -> Read x }
      | e:!(Expr.parse) { fun x -> Assign (x, e) };
        
      stmt:
        %"skip"                          { Skip    }
      | %"write" "(" e:!(Expr.parse) ")" { Write e }
      | x:IDENT ":=" er:eread            { er    x }
    )
  end
  
let upd f x y = fun z -> if z = x then y else f z
exception FullyReduced
  
module Interpreter =
  struct
    type t = (string -> int) * int list * int list * Stmt.t 

    let rec step (t : t) : t =
      let (varst, inl, outl, stmt) = t in
      match stmt with
      | Stmt.Skip -> raise FullyReduced
      | Stmt.Write expr ->
         let value = Expr.eval varst expr in
         (varst, inl, value :: outl, Stmt.Skip)
      | Stmt.Read var ->
         let value :: inl' = inl in
         (upd varst var value, inl', outl, Stmt.Skip)
      | Stmt.Assign (var, expr) ->
         let value = Expr.eval varst expr in
         (upd varst var value, inl, outl, Stmt.Skip)
      | Stmt.Seq (Stmt.Skip, rstmt) ->
         (varst, inl, outl, rstmt)
      | Stmt.Seq (lstmt, rstmt) ->
         let (varst', inl', outl', stmt') =
           step (varst, inl, outl, lstmt) in
         (varst', inl', outl', Stmt.Seq (stmt', rstmt))
         
    let run (inl : int list) (stmt : Stmt.t) : int list =
      let st = ref ((fun (x : string) -> 0), inl, [], stmt) in
      try
        while true do
          st := step !st
        done;
        []
      with
      | FullyReduced ->
         let (_, _, out, _) = !st in
         out
  end
  
let skip      = Stmt.Skip
let read  s   = Stmt.Read s
let write e   = Stmt.Write e
let (-<)  v e = Stmt.Assign (v,e)
let seq l = List.fold_right
              (fun l r -> Stmt.Seq (l, r)) l Stmt.Skip

let const n = Expr.Const n
let var   v = Expr.Var   v 
let bop l op r = Expr.Binop (op, l, r)

let prog =
  seq [
      read "x";
      write (bop (var "x") "+" (const 1));
      skip;
      read "y";
      write (bop (var "x") "+" (var "y"))
    ]

module StackMachine =
  struct
    type t =
      | READ
      | WRITE
      | PUSH  of int
      | LD    of string
      | ST    of string
      | BINOP of string
      | NOP
      
    let rec compile_expr (e : Expr.t) : t list =
      match e with
      | Const n -> [PUSH n]
      | Var   v -> [LD   v]
      | Binop (op, l, r) ->
         (compile_expr l) @ (compile_expr r) @ [BINOP op]
      
    let rec compile_stmt (s : Stmt.t) : t list =
      match s with
      | Stmt.Skip    -> [NOP]
      | Stmt.Write e -> (compile_expr e) @ [WRITE]
      | Stmt.Read  v -> [READ; ST v] 
      | Stmt.Assign (v, e) -> (compile_expr e) @ [ST v]
      | Stmt.Seq    (l, r) -> (compile_stmt l) @ (compile_stmt r)
                            
    let compile = compile_stmt
                            
    type st = (string -> int) *
                int list * int list * int list * int * t list
            
    let step st =
      try
        let (varst, stack, inl, outl, ip, prog) = st in
        match List.nth prog ip with
        | READ   ->
           let value :: inl' = inl in
           (varst, value :: stack, inl', outl, ip + 1, prog)
        | WRITE  ->
           let value :: stack' = stack in
           (varst, stack', inl, value :: outl, ip + 1, prog)
        | PUSH n ->
           (varst, n :: stack, inl, outl, ip + 1, prog)
        | LD var ->
           (varst, (varst var) :: stack, inl, outl, ip + 1, prog)
        | ST var ->
           let value :: stack' = stack in
           (upd varst var value, stack', inl, outl, ip + 1, prog)
        | BINOP op ->
           let r :: l :: stack' = stack in
           (varst, (Expr.apply op l r) :: stack',
            inl, outl, ip + 1, prog)
        | NOP   -> (varst, stack, inl, outl, ip + 1, prog)
      with
      | Failure "nth" -> raise FullyReduced
            
    let run (inl : int list) prog =
      let st =
        ref ((fun (x : string) -> 0), [], inl, [], 0, prog) in
      try
        while true do
          st := step !st
        done;
        []
      with
      | FullyReduced ->
         let (_, _, _, out, _, _) = !st in
         out
  end

let compile_and_run () =
  StackMachine.run [7; 10] (StackMachine.compile_stmt prog)

let parse infile =
  let s = Util.read infile in
  Util.parse
    (object
       inherit Matcher.t s
       inherit Util.Lexers.ident ["read"; "write"; "skip"] s
       inherit Util.Lexers.decimal s
       inherit Util.Lexers.skip [
                   Matcher.Skip.whitespaces " \t\n";
                   Matcher.Skip.lineComment "--";
                   Matcher.Skip.nestedComment "(*" "*)"
                 ] s
     end
    )
    (ostap (!(Stmt.parse) -EOF))

let main =
  try
    let mode, filename =
      match Sys.argv.(1) with
      | "-s" -> `SM, Sys.argv.(2)
      (* | "-o" -> `X86, Sys.argv.(2) *)
      | "-i" -> `Int, Sys.argv.(2)
      | _ -> raise (Invalid_argument "invalid flag")
    in
    match parse filename with
    | `Ok stmt ->
         (match mode with
          | `SM | `Int ->
             let rec read acc =
               try
                 let r = read_int () in
                 Printf.printf "> ";
                 read (acc @ [r])
               with End_of_file -> acc
             in
             let input = read [] in
             let output =
               (match mode with
                | `SM  -> StackMachine.run input (StackMachine.compile stmt)
                | `Int -> Interpreter .run input stmt)
             in
             List.iter (fun i -> Printf.printf "%d\n" i) output
         )
    | `Fail er -> Printf.eprintf "%s\n" er
  with
  | Invalid_argument _ ->
     Printf.printf "Usage: rc <command> <name.expr>\n";
     Printf.printf "  <command> should be one of: -i, -s, -o.\n"
