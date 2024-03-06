open TokenTypes
(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

(*let tokenize input = failwith "unimplemented"*)

(*

"Hello there"
"Hello  there"
"Hello 
 there"
()  ||  

    


       =
       >
          <
                <>

*)
(**
SHOULD I HAVE SOME OTHER MESSAGE FOR ELSE RAISE ILLEGALEXPRESSION THING?????????????????????    
*)

(*
   let str_splitter input = Str.split (Str.regexp "\"[^\"]*\"") input
   
   *)
(*
let rec input_list_gen pos s = 
  let newline_tok = Str.regexp "\n" in 
  let tab_space = Str.regexp "\t" in
  let cr_space = Str.regexp "\r" in
  let one_space = Str.regexp " " in
  let string_tok = Str.regexp "\"[^\"]*\"" in

  if (Str.string_match newline_tok s pos) then let matched_newline = Str.matched_string s in
    let size_of_space = (String.length matched_newline) in [] @ (input_list_gen (pos + size_of_space) s)
  else if (Str.string_match tab_space s pos) then let matched_tab = Str.matched_string s in
    let size_of_space = (String.length matched_tab) in [] @ (input_list_gen (pos + size_of_space) s)
  else if (Str.string_match cr_space s pos) then let matched_cr = Str.matched_string s in
    let size_of_space = (String.length matched_cr) in [] @ (input_list_gen (pos + size_of_space) s)
  else if (Str.string_match one_space s pos) then [] @ (input_list_gen (pos + 1) s)
  else 
*)

let str_splitter input = Str.full_split (Str.regexp "\"[^\"]*\"\\|[ \t\n]+") input



let tokenize input =
  
  let input_list = List.fold_left (fun acc list_elem -> match list_elem with 
  | Str.Text my_s -> acc @ [my_s] 
  | Str.Delim del_s -> let space_regexp = Str.regexp "[ \t\r\n]+" in 
    if Str.string_match space_regexp del_s 0 then acc else acc @ [del_s]) [] (str_splitter input) in

  let r_parentok = Str.regexp ")" in
  let l_parentok = Str.regexp "(" in 
  let eq_tok = Str.regexp "=" in 
  let noteq_tok = Str.regexp "<>" in
  let greater_tok = Str.regexp ">" in 
  let less_tok = Str.regexp "<" in
  let greater_eq_tok = Str.regexp ">=" in
  let less_eq_tok = Str.regexp "<=" in
  let or_tok = Str.regexp "||" in
  let and_tok = Str.regexp "&&" in 
  let not_tok = Str.regexp "not" in
  let if_tok = Str.regexp "if" in
  let then_tok = Str.regexp "^then" in
  let else_tok = Str.regexp "^else" in
  let add_tok = Str.regexp "\\+" in
  let sub_tok = Str.regexp "-" in
  let mult_tok = Str.regexp "\\*" in
  let div_tok = Str.regexp "/" in
  let concat_tok = Str.regexp "\\^" in
  let let_tok = Str.regexp "let" in
  let rec_tok = Str.regexp "rec" in
  let in_tok = Str.regexp "in" in
  let def_tok = Str.regexp "def" in
  let fun_tok = Str.regexp "fun" in
  let arrow_tok = Str.regexp "->" in
  let double_semi_tok = Str.regexp ";;" in
  let bool_true_tok = Str.regexp "true" in
  let bool_false_tok = Str.regexp "false" in
  let int_pos_tok = Str.regexp "[0-9]+" in
  let n_int_tok = Str.regexp "(-[0-9]+)" in
  let string_tok = Str.regexp "\"[^\"]*\"" in
  let id_tok = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in

(*
  let newline_tok = Str.regexp "\n" in 
  let tab_space = Str.regexp "\t" in
  let cr_space = Str.regexp "\r" in
  let one_space = Str.regexp " " in
*)


    
    

    let rec tok_list_generator pos s = 
      if (pos >= (String.length s)) then [] else 
      
      if (Str.string_match string_tok s pos) then let matched_str = Str.matched_string s in
        let str_size = (String.length matched_str) in [Tok_String (Str.string_after (Str.first_chars (matched_str) ((String.length matched_str)-1)) 1)] @ (tok_list_generator (pos + str_size) s) else
      
      (*if (Str.string_match string_tok s pos) then let matched_str = Str.matched_string s in
      let str_size = (String.length matched_str) in 
      let new_str = global_replace regexp templ s in [Tok_String new_str] @ (tok_list_generator (pos + str_size) s) else*)

      if (Str.string_match n_int_tok s pos) then let matched_n_int = Str.matched_string s in
        let n_int_size = (String.length matched_n_int) in 
        let new_str = (Str.string_before matched_n_int (n_int_size - 1)) in
        let final_str = (Str.string_after new_str 1) in
        [Tok_Int (int_of_string final_str)] @ (tok_list_generator (pos + n_int_size) s) else
        
      (*
      if (Str.string_match id_tok s pos) then let matched_id = Str.matched_string s in 
        let id_size = (String.length matched_id) in [Tok_ID (matched_id)] @ (tok_list_generator (pos + id_size) s) else
      *)

      if (Str.string_match int_pos_tok s pos) then let matched_pos_int = Str.matched_string s in
        let pos_int_size = (String.length matched_pos_int) in [Tok_Int (int_of_string matched_pos_int)] @ (tok_list_generator (pos + pos_int_size) s) else
        
      if (Str.string_match bool_false_tok s pos) then [Tok_Bool (false)] @ (tok_list_generator (pos + 5) s) else
      
      if (Str.string_match bool_true_tok s pos) then [Tok_Bool (true)] @ (tok_list_generator (pos + 4) s) else
      
      if (Str.string_match then_tok s pos) then [Tok_Then] @ (tok_list_generator (pos + 4) s) else
      
      if (Str.string_match else_tok s pos) then [Tok_Else] @ (tok_list_generator (pos + 4) s) else
      
      if (Str.string_match let_tok s pos) then [Tok_Let] @ (tok_list_generator (pos + 3) s) else
      
      if (Str.string_match rec_tok s pos) then [Tok_Rec] @ (tok_list_generator (pos + 3) s) else
      
      if (Str.string_match def_tok s pos) then [Tok_Def] @ (tok_list_generator (pos + 3) s) else
      
      if (Str.string_match fun_tok s pos) then [Tok_Fun] @ (tok_list_generator (pos + 3) s) else
      
      if (Str.string_match not_tok s pos) then [Tok_Not] @ (tok_list_generator (pos + 3) s) else
      
      if (Str.string_match noteq_tok s pos) then [Tok_NotEqual] @ (tok_list_generator (pos + 2) s) else
      
      if (Str.string_match greater_eq_tok s pos) then [Tok_GreaterEqual] @ (tok_list_generator (pos + 2) s) else
      
      if (Str.string_match less_eq_tok s pos) then [Tok_LessEqual] @ (tok_list_generator (pos + 2) s) else
      
      if (Str.string_match or_tok s pos) then [Tok_Or] @ (tok_list_generator (pos + 2) s) else
      
      if (Str.string_match and_tok s pos) then [Tok_And] @ (tok_list_generator (pos + 2) s) else
      
      if (Str.string_match if_tok s pos) then [Tok_If] @ (tok_list_generator (pos + 2) s) else
      
      if (Str.string_match in_tok s pos) then [Tok_In] @ (tok_list_generator (pos + 2) s) else
      
      if (Str.string_match arrow_tok s pos) then [Tok_Arrow] @ (tok_list_generator (pos + 2) s) else
      
      if (Str.string_match double_semi_tok s pos) then [Tok_DoubleSemi] @ (tok_list_generator (pos + 2) s) else

      if (Str.string_match r_parentok s pos) then [Tok_RParen] @ (tok_list_generator (pos + 1) s) else 
      
      if (Str.string_match l_parentok s pos) then [Tok_LParen]	@ (tok_list_generator (pos + 1) s) else 
      
      if (Str.string_match eq_tok s pos) then [Tok_Equal] @ (tok_list_generator (pos + 1) s) else
      
      if (Str.string_match greater_tok s pos) then [Tok_Greater] @ (tok_list_generator (pos + 1) s) else
      
      if (Str.string_match less_tok s pos) then [Tok_Less] @ (tok_list_generator (pos + 1) s) else

      if (Str.string_match add_tok s pos) then [Tok_Add] @ (tok_list_generator (pos + 1) s) else
      
      if (Str.string_match sub_tok s pos) then [Tok_Sub] @ (tok_list_generator (pos + 1) s) else
      
      if (Str.string_match mult_tok s pos) then [Tok_Mult] @ (tok_list_generator (pos + 1) s) else
      
      if (Str.string_match div_tok s pos) then [Tok_Div] @ (tok_list_generator (pos + 1) s) else
      
      if (Str.string_match concat_tok s pos) then [Tok_Concat] @ (tok_list_generator (pos + 1) s) else
        
      if (Str.string_match id_tok s pos) then let matched_id = Str.matched_string s in 
        let id_size = (String.length matched_id) in [Tok_ID (matched_id)] @ (tok_list_generator (pos + id_size) s)

                      (*          
                      else if (Str.string_match newline_tok s pos) then let matched_newline = Str.matched_string s in
                        let size_of_space = (String.length matched_newline) in [] @ (tok_list_generator (pos + size_of_space) s)
                      else if (Str.string_match tab_space s pos) then let matched_tab = Str.matched_string s in
                        let size_of_space = (String.length matched_tab) in [] @ (tok_list_generator (pos + size_of_space) s)
                      else if (Str.string_match cr_space s pos) then let matched_cr = Str.matched_string s in
                        let size_of_space = (String.length matched_cr) in [] @ (tok_list_generator (pos + size_of_space) s)
                      else if (Str.string_match one_space s pos) then [] @ (tok_list_generator (pos + 1) s)
                      *)
        else raise (InvalidInputException ("Invalid tokenize input: " ^ input))
      in List.fold_left (fun acc list_elem -> acc @ (tok_list_generator 0 list_elem)) [] input_list