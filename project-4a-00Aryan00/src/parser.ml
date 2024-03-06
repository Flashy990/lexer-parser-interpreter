open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

(*let rec parse_expr toks = failwith "unimplemented"*)


let rec parse_expr toks = match (lookahead toks) with
  | Some Tok_Let -> (parse_let_expr toks)
  | Some Tok_If -> (parse_if_expr toks)
  | Some Tok_Fun -> (parse_fun_expr toks)
  | _ -> (parse_or_expr toks)

and parse_let_expr toks = let (let_bool_val, t_after_rec_e) = (if (lookahead_many toks 1) = Some Tok_Rec then (true, (match_many toks [Tok_Let; Tok_Rec])) else (false, (match_token toks Tok_Let))) in
  match (lookahead t_after_rec_e) with
  | Some Tok_ID (tok_value) -> let t = match_many t_after_rec_e [Tok_ID (tok_value); Tok_Equal] in
                                let (t1, eval_let_expr1) = (parse_expr t) in
                                  let t2 = (match_token t1 Tok_In) in
                                    let (t3, eval_let_expr2) = (parse_expr t2) in
                                      (t3, Let(tok_value, let_bool_val, eval_let_expr1, eval_let_expr2))
  | _ -> raise (InvalidInputException("Incorrect let expression"))


and parse_if_expr toks = match lookahead toks with
  | Some Tok_If -> let t = (match_token toks Tok_If) in 
                  let (t1, eval_if_expr1) = (parse_expr t) in
                    let t2 = (match_token t1 Tok_Then) in
                      let (t3, eval_if_expr2) = (parse_expr t2) in
                        let t4 = (match_token t3 Tok_Else) in
                          let (t5, eval_if_expr3) = (parse_expr t4) in
                            (t5, If(eval_if_expr1, eval_if_expr2, eval_if_expr3))

  | _ ->  raise (InvalidInputException("Not starting with IF"))

and parse_fun_expr toks = let t = match_token toks Tok_Fun in match (lookahead t) with
  | Some Tok_ID (tok_value) -> let t1 = (match_many t [Tok_ID (tok_value); Tok_Arrow]) in
                              let (t2, eval_function_expr) = (parse_expr t1) in
                                (t2, Fun(tok_value, eval_function_expr))
  | _ ->  raise (InvalidInputException("Fun not starting with FUN"))

and parse_or_expr toks = let (t, eval_and_expr) = (parse_and_expr toks) in 
  match (lookahead t) with
    | Some Tok_Or -> let t1 = (match_token t Tok_Or) in 
                    let (t2, eval_or_expr) = (parse_or_expr t1) in 
                      (t2, Binop(Or, eval_and_expr, eval_or_expr)) 
    | _ -> (t, eval_and_expr)

and parse_and_expr toks = let (t, eval_eq_expr) = (parse_eq_expr toks) in
  match (lookahead t) with
    | Some Tok_And -> let t1 = (match_token t Tok_And) in
                      let (t2, eval_and_expr) = (parse_and_expr t1) in
                        (t2, Binop(And, eval_eq_expr, eval_and_expr))
    | _-> (t, eval_eq_expr)

and parse_eq_expr toks = let (t, eval_rel_expr) = (parse_rel_expr toks) in
  match (lookahead t) with
    | Some Tok_Equal -> let t1 = (match_token t Tok_Equal) in 
                        let (t2, eval_eq_expr) = (parse_eq_expr t1) in
                          (t2, Binop(Equal, eval_rel_expr, eval_eq_expr))
    | Some Tok_NotEqual -> let t1 = (match_token t Tok_NotEqual) in 
                            let (t2, eval_eq_expr) = (parse_eq_expr t1) in
                              (t2, Binop(NotEqual, eval_rel_expr, eval_eq_expr))
    | _ -> (t, eval_rel_expr)

and parse_rel_expr toks = let (t, eval_add_expr) = (parse_add_expr toks) in
  match (lookahead t) with
  | Some Tok_Greater -> let t1 = (match_token t Tok_Greater) in
                        let (t2, eval_rel_expr) = (parse_rel_expr t1) in
                          (t2, Binop(Greater, eval_add_expr, eval_rel_expr))
  | Some Tok_Less -> let t1 = (match_token t Tok_Less) in
                      let (t2, eval_rel_expr) = (parse_rel_expr t1) in
                        (t2, Binop(Less, eval_add_expr, eval_rel_expr))
  | Some Tok_GreaterEqual -> let t1 = (match_token t Tok_GreaterEqual) in
                              let (t2, eval_rel_expr) = (parse_rel_expr t1) in
                                (t2, Binop(GreaterEqual, eval_add_expr, eval_rel_expr))
  | Some Tok_LessEqual -> let t1 = (match_token t Tok_LessEqual) in
                            let (t2, eval_rel_expr) = (parse_rel_expr t1) in
                              (t2, Binop(LessEqual, eval_add_expr, eval_rel_expr))
  | _ -> (t, eval_add_expr)

and parse_add_expr toks = let (t, eval_mult_expr) = (parse_mult_expr toks) in
  match (lookahead t) with 
    | Some Tok_Add -> let t1 = (match_token t Tok_Add) in
                      let (t2, eval_add_expr) = (parse_add_expr t1) in
                        (t2, Binop(Add, eval_mult_expr, eval_add_expr))
    | Some Tok_Sub -> let t1 = (match_token t Tok_Sub) in
                        let (t2, eval_add_expr) = (parse_add_expr t1) in
                          (t2, Binop(Sub, eval_mult_expr, eval_add_expr))
    | _ -> (t, eval_mult_expr)

and parse_mult_expr toks = let (t, eval_concat_expr) = (parse_concat_expr toks) in
  match (lookahead t) with
    | Some Tok_Mult -> let t1 = (match_token t Tok_Mult) in
                      let (t2, eval_mult_expr) = (parse_mult_expr t1) in
                        (t2, Binop(Mult, eval_concat_expr, eval_mult_expr))
    | Some Tok_Div -> let t1 = (match_token t Tok_Div) in
                        let (t2, eval_mult_expr) = (parse_mult_expr t1) in
                          (t2, Binop(Div, eval_concat_expr, eval_mult_expr))
    | _ -> (t, eval_concat_expr)

and parse_concat_expr toks = let (t, eval_unary_expr) = (parse_unary_expr toks) in 
  match (lookahead t) with
    | Some Tok_Concat -> let t1 = (match_token t Tok_Concat) in
                        let (t2, eval_concat_expr) = (parse_concat_expr t1) in
                          (t2, Binop(Concat, eval_unary_expr, eval_concat_expr))
    | _ -> (t, eval_unary_expr)

and parse_unary_expr toks = match (lookahead toks) with
    | Some Tok_Not -> let t1 = (match_token toks Tok_Not) in
                      let (t2, eval_unary_expr) = (parse_unary_expr t1) in
                        (t2, Not(eval_unary_expr))
    | _ -> (parse_funcall_expr toks) (*(t, eval_funcall_expr)*)

and parse_funcall_expr toks = let (t, eval_prim_expr) = (parse_prim_expr toks) in
  match (lookahead t) with
  | Some Tok_Int (tok_value) -> let (t1, eval_prim_expr2) = parse_prim_expr t in (t1, FunctionCall(eval_prim_expr, eval_prim_expr2))
  | Some Tok_Bool (tok_value) -> let (t1, eval_prim_expr2) = parse_prim_expr t in (t1, FunctionCall(eval_prim_expr, eval_prim_expr2))
  | Some Tok_String (tok_value) -> let (t1, eval_prim_expr2) = parse_prim_expr t in (t1, FunctionCall(eval_prim_expr, eval_prim_expr2))
  | Some Tok_ID (tok_value) -> let (t1, eval_prim_expr2) = parse_prim_expr t in (t1, FunctionCall(eval_prim_expr, eval_prim_expr2))
  | Some Tok_LParen -> let (t1, eval_prim_expr2) = parse_prim_expr t in (t1, FunctionCall(eval_prim_expr, eval_prim_expr2))
  | _ -> (t, eval_prim_expr)

and parse_prim_expr toks = match lookahead toks with
  | Some Tok_Int (tok_value) -> let t1 = (match_token toks (Tok_Int (tok_value))) in (t1, Value(Int(tok_value)))
  | Some Tok_Bool (tok_value) -> let t1 = (match_token toks (Tok_Bool (tok_value))) in (t1, Value(Bool(tok_value)))
  | Some Tok_String (tok_value) -> let t1 = (match_token toks (Tok_String (tok_value))) in (t1, Value(String(tok_value)))
  | Some Tok_ID (tok_value) -> let t1 = (match_token toks (Tok_ID (tok_value))) in (t1, ID(tok_value))
  | Some Tok_LParen ->  let t1 = (match_token toks (Tok_LParen)) in (* For debugging (print_string (string_of_token (List.hd t1)));*)
                          let (t2, eval_after_Lparen) = parse_expr t1 in
                          (*For debugging print_string (string_of_token (List.hd t2));*)
                            let t3 = (match_token t2 Tok_RParen) in
                            (*For debugging print_string (string_of_token (List.hd t3));*)
                              (t3, eval_after_Lparen)
  | _ -> raise (InvalidInputException("end of prim expression"))

(* Part 3: Parsing mutop *)

(*let rec parse_mutop toks = failwith "unimplemented"*)
let rec parse_mutop toks = match (lookahead toks) with
  | Some Tok_DoubleSemi -> let t = (match_token toks Tok_DoubleSemi) in
                          (t, NoOp)
  | Some Tok_Def -> (parse_def_mutop_expr toks)
  | _ -> (parse_expr_mutop_expr toks)

and parse_def_mutop_expr toks = let t = (match_token toks Tok_Def) in
  match (lookahead t) with
    | Some Tok_ID (tok_value) -> let t1 = (match_many t [Tok_ID (tok_value); Tok_Equal]) in 
                                let (t2, eval_def_mutop_expr) = (parse_expr t1) in
                                  let t3 = (match_token t2 Tok_DoubleSemi) in
                                    (t3, Def(tok_value, eval_def_mutop_expr))
    | _ -> raise (InvalidInputException("No Tok_ID after Def"))

and parse_expr_mutop_expr toks = let (t, eval_expr_mutop_expr) = (parse_expr toks) in let t1 = (match_token t Tok_DoubleSemi) in
  (t1, Expr(eval_expr_mutop_expr))