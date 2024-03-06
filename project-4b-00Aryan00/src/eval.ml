open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions
  Helpful for creating an environment. You can do this with references 
  (not taught) or without. 
  You do not have to use these and you can modify them if you want. 
  If you do not use references, you will need the following data type:
*)

(*ref- replace with ref_*)

(*
As clarified on piazza, using "values" is optional. Hence, I am going to not use "values" in my implementation.   
type values = Int of int|Bool of bool|String of string
*)

(* Adds mapping [x:v] to environment [env] *)
let ref_extend env x v = (x, ref v)::env

let extend env x v = (x,v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec ref_lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else ref_lookup t x

let rec lookup env x = 
  match env with
  [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let ref_extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec ref_update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else ref_update t x v
        
(* Removes the most recent variable,value binding from the environment *)
let rec remove env x = match env with
  [] -> []
  | (var,value)::t -> if x = var then t else (var,value)::(remove t x)

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
(*let rec eval_expr env e = failwith "unimplemented"*)
let rec eval_expr env e = match e with
  | Value (val_value) -> begin (match val_value with
                          | Int (val_int) -> val_value
                          | Bool (val_bool) -> val_value
                          | String (val_string) -> val_value
                          | Closure (cl_environment, cl_var, cl_expr) ->  raise (TypeError("Invalid type found"))) end
  | ID (id_var) ->  ref_lookup env id_var

  | Fun (fun_var, fun_expr) -> Closure (env, fun_var, fun_expr)

  | Not (not_expr) -> let not_expr_val = (eval_expr env not_expr) in
    begin (
      match not_expr_val with
      | Bool (evaluated_bool_val) -> if evaluated_bool_val then Bool (false) else Bool (true)
      | _ -> raise (TypeError("Expected type bool"))
    ) end

  | Binop (binop_op, binop_expr1, binop_expr2) ->  let val_expr_1 = (eval_expr env binop_expr1) in
    let val_expr_2 = (eval_expr env binop_expr2) in 
      begin (
        match val_expr_1, val_expr_2 with
        | Int (val1), Int(val2) -> 
          begin (
            match binop_op with
            | Add -> Int (val1 + val2)
            | Sub -> Int (val1 - val2)
            | Mult -> Int (val1 * val2)
            | Div -> if (val1 = 0) then (raise (DivByZeroError)) 
                    else (if (val2 = 0) then (raise (DivByZeroError)) 
                            else (Int (val1/val2)))
            
            | Equal ->  Bool (val1 = val2)
            | NotEqual ->  Bool (val1 <> val2)
            
            | Greater -> Bool (val1 > val2)
            | Less -> Bool (val1 < val2)
            | GreaterEqual -> Bool (val1 >= val2)
            | LessEqual-> Bool (val1 <= val2)
            | _ -> raise (TypeError("Invalide types for Int_binops"))
          ) end
        | Bool (val1), Bool(val2) -> 
          begin (
            match binop_op with
              | Or -> Bool (val1 || val2)
              | And -> Bool (val1 && val2)
              | Equal -> Bool (val1 = val2)
              | NotEqual -> Bool (val1 <> val2)
              | _ -> raise (TypeError("Invalide types for Bool_Binop"))
          ) end
        | String(val1), String(val2) -> 
          begin (
            match binop_op with
              | Concat -> String (val1 ^ val2)
              | Equal -> Bool (val1 = val2)
              | NotEqual -> Bool (val1 <> val2)
              | _ -> raise (TypeError("Invalide types for String_Binop"))
          ) end
        | _ -> raise (TypeError("Invalide types for Binop"))
      ) end
                                                    
  | If (if_expr1, if_expr2, if_expr3) -> let if_bool_val = (eval_expr env if_expr1) in
    begin (
      match if_bool_val with
      | Bool (true) -> (eval_expr env if_expr2)
      | Bool (false) -> (eval_expr env if_expr3)
      | _ -> raise (TypeError("Expression does not evaluate to a bool value"))
    ) end

  | FunctionCall (fun_call_expr1, fun_call_expr2) ->  let fun_call_expr1_val = (eval_expr env fun_call_expr1) in
    begin (
      match fun_call_expr1_val with
      | Closure(a_val, x_val, e_val) -> let fun_call_expr2_val = (eval_expr env fun_call_expr2) in
        let new_a_val = (ref_extend a_val x_val fun_call_expr2_val) in 
          (eval_expr new_a_val e_val)
      | _ -> raise (TypeError("Function call expr1 does not evaluate to a closure"))
    ) end

  | Let (let_var, let_bool, let_expr1, let_expr2) ->  if (let_bool = false) then (
                                                        let val1 = (eval_expr env let_expr1) in
                                                          let new_env = (ref_extend env let_var val1) in
                                                            (eval_expr new_env let_expr2)
                                                      ) else (
                                                        let new_env1 = ref_extend_tmp env let_var in
                                                          let val1 = (eval_expr new_env1 let_expr1) in
                                                            let _ = (ref_update new_env1 let_var val1) in
                                                              (eval_expr new_env1 let_expr2)
                                                      )

(*| _ -> raise (TypeError("Invalid type found"))*)
(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
(*let eval_mutop env m = failwith "unimplemented"*)


let eval_mutop env m = match m with
| Def (def_var, def_expr) -> let new_env1 = ref_extend_tmp env def_var in
                                let def_evaluated = eval_expr new_env1 def_expr in
                                  let new_env2 = remove new_env1 def_var in
                                    let new_env3 = ref_extend new_env2 def_var def_evaluated in  
                                    (new_env3, Some def_evaluated)
| Expr (expr_to_eval) -> (env, Some (eval_expr env expr_to_eval))
| NoOp -> (env, None)
