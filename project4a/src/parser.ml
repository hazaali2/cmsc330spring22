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

let parse_ID toks = 
  match (lookahead toks) with
    | Some Tok_ID k -> let tokens = match_token toks (Tok_ID k) in
                    (tokens, k)

    | _ -> raise (InvalidInputException "failed")


let rec parse_expr toks = 
  match (lookahead toks) with
    | Some Tok_Let -> let (tokens, expr) = parse_LetExp toks in
                    (tokens, expr)

    | Some Tok_If -> let (tokens, expr) = parse_IfExp toks in
                    (tokens, expr)

    | Some Tok_Fun -> let (tokens, expr) = parse_FunExp toks in
                    (tokens, expr)

    | _ -> parse_OrExp toks


and parse_LetExp toks = 
  let tokens1 = match_token toks Tok_Let in               
  match (lookahead tokens1) with
  | Some Tok_Rec -> let tokens2 = match_token tokens1 Tok_Rec in
                    let (tokens3, expr_ID) = parse_ID tokens2 in
                      let (tokens4, expr1) = parse_expr (match_token tokens3 Tok_Equal) in
                        let (tokens5, expr2) = parse_expr (match_token tokens4 Tok_In) in
                            (tokens5, Let (expr_ID, true, expr1, expr2))

  | _ -> let (tokens2, expr_ID) = parse_ID tokens1 in
          let (tokens3, expr1) = parse_expr (match_token tokens2 Tok_Equal) in
            let (tokens4, expr2) = parse_expr (match_token tokens3 Tok_In) in
                (tokens4, Let (expr_ID, false, expr1, expr2))


and parse_FunExp toks = 
  let tokens1 = match_token toks Tok_Fun in
    let (tokens2, expr_ID) = parse_ID tokens1 in
      let (tokens3, expr1) = parse_expr (match_token tokens2 Tok_Arrow) in
          (tokens3, Fun (expr_ID, expr1))


and parse_IfExp toks = 
  let (tokens2, expr1) = parse_expr (match_token toks Tok_If) in
    let (tokens3, expr2) = parse_expr (match_token tokens2 Tok_Then) in
      let (tokens4, expr3) = parse_expr (match_token tokens3 Tok_Else) in
          (tokens4, If (expr1, expr2, expr3))


and parse_AndExp toks = 
  let (toks_after_parse_EqualityExp, expr) = parse_EqualityExp toks in
    match (lookahead toks_after_parse_EqualityExp) with
    | Some Tok_And -> let tokens2 = match_token toks_after_parse_EqualityExp Tok_And in
                  let (tokens3, expr_after_parse_AndExp) = parse_AndExp tokens2 in
                  (tokens3, Binop (And, expr, expr_after_parse_AndExp))

    | _ -> (toks_after_parse_EqualityExp, expr)


and parse_OrExp toks = 
  let (toks_after_parse_AndExp, expr) = parse_AndExp toks in
      match (lookahead toks_after_parse_AndExp) with
      | Some Tok_Or -> let tokens2 = match_token toks_after_parse_AndExp Tok_Or in
                    let (tokens3, expr_after_parse_OrExp) = parse_OrExp tokens2 in
                    (tokens3, Binop (Or, expr, expr_after_parse_OrExp))

      | _ -> (toks_after_parse_AndExp, expr)


and parse_EqualityExp toks = 
  let (toks_after_parse_RelationalExp, expr) = parse_RelationalExp toks in
      match (lookahead toks_after_parse_RelationalExp) with
      | Some Tok_Equal -> let tokens2 = match_token toks_after_parse_RelationalExp Tok_Equal in
                    let (tokens3, expr_after_parse_EqualityExp) = parse_EqualityExp tokens2 in
                        (tokens3, Binop (Equal, expr, expr_after_parse_EqualityExp))

      | Some Tok_NotEqual -> let tokens2 = match_token toks_after_parse_RelationalExp Tok_NotEqual in
                        let (tokens3, expr_after_parse_EqualityExp) = parse_EqualityExp tokens2 in
                            (tokens3, Binop (NotEqual, expr, expr_after_parse_EqualityExp))

      | _ -> (toks_after_parse_RelationalExp, expr)


and parse_RelationalExp toks = 
  let (toks_after_parse_AddExp, expr) = parse_AddExp toks in
        match (lookahead toks_after_parse_AddExp) with
        | Some Tok_Less -> let tokens2 = match_token toks_after_parse_AddExp Tok_Less in
            let (tokens3, expr_after_parse_RelationalExp) = parse_RelationalExp tokens2 in
                (tokens3, Binop (Less, expr, expr_after_parse_RelationalExp))

        | Some Tok_Greater -> let tokens2 = match_token toks_after_parse_AddExp Tok_Greater in
            let (tokens3, expr_after_parse_RelationalExp) = parse_RelationalExp tokens2 in
                (tokens3, Binop (Greater, expr, expr_after_parse_RelationalExp))

        | Some Tok_LessEqual -> let tokens2 = match_token toks_after_parse_AddExp Tok_Less in
            let (tokens3, expr_after_parse_RelationalExp) = parse_RelationalExp tokens2 in
                (tokens3, Binop (LessEqual, expr, expr_after_parse_RelationalExp))

        | Some Tok_GreaterEqual -> let tokens2 = match_token toks_after_parse_AddExp Tok_GreaterEqual in
            let (tokens3, expr_after_parse_RelationalExp) = parse_RelationalExp tokens2 in
                (tokens3, Binop (GreaterEqual, expr, expr_after_parse_RelationalExp))

        | _ -> (toks_after_parse_AddExp, expr)


and parse_AddExp toks = 
  let (toks_after_parse_MultExp, expr) = parse_MultExp toks in
          match (lookahead toks_after_parse_MultExp) with
          | Some Tok_Add -> let tokens2 = match_token toks_after_parse_MultExp Tok_Add in
                        let (tokens3, expr_after_parse_AddExp) = parse_AddExp tokens2 in
                        (tokens3, Binop (Add, expr, expr_after_parse_AddExp))

          | Some Tok_Sub -> let tokens2 = match_token toks_after_parse_MultExp Tok_Sub in
                            let (tokens3, expr_after_parse_AddExp) = parse_AddExp tokens2 in
                            (tokens3, Binop (Sub, expr, expr_after_parse_AddExp))

          | _ -> (toks_after_parse_MultExp, expr)


and parse_MultExp toks = 
  let (toks_after_parse_Concatexp, expr) = parse_ConcatExp toks in
          match (lookahead toks_after_parse_Concatexp) with
          | Some Tok_Mult -> let tokens2 = match_token toks_after_parse_Concatexp Tok_Mult in
                        let (tokens3, expr_after_parse_MultExp) = parse_MultExp tokens2 in
                            (tokens3, Binop (Mult, expr, expr_after_parse_MultExp))

          | Some Tok_Div -> let tokens2 = match_token toks_after_parse_Concatexp Tok_Div in
                        let (tokens3, expr_after_parse_MultExp) = parse_MultExp tokens2 in
                            (tokens3, Binop (Div, expr, expr_after_parse_MultExp))

          | _ -> (toks_after_parse_Concatexp, expr)


and parse_ConcatExp toks = 
  let (toks_after_parse_UnaryExp, expr) = parse_UnaryExp toks in
      match (lookahead toks_after_parse_UnaryExp) with
      | Some Tok_Concat -> let tokens2 = match_token toks_after_parse_UnaryExp Tok_Concat in
                    let (tokens3, expr_after_parse_Concatexp) = parse_ConcatExp tokens2 in
                        (tokens3, Binop (Concat, expr, expr_after_parse_Concatexp))

      | _ -> (toks_after_parse_UnaryExp, expr)


and parse_UnaryExp toks = 
  match lookahead toks with
  | Some Tok_Not -> let tokens2 = match_token toks Tok_Not in
                let (tokens3, expr_after_parse_UnaryExp) = parse_UnaryExp tokens2 in
                    (tokens3, Not (expr_after_parse_UnaryExp))

  | _ -> parse_FunctionCallExp toks


and parse_FunctionCallExp toks = 
  let (tokens1, expr5) = parse_PrimaryExp toks in
        match (lookahead tokens1) with
        | Some Tok_Int i -> let tokens2 = match_token tokens1 (Tok_Int i) in
                      (tokens2, FunctionCall (expr5, Value (Int i)))

        | Some Tok_String j -> let tokens2 = match_token tokens1 (Tok_String j) in
                      (tokens2, FunctionCall (expr5, Value (String j)))

        | Some Tok_ID k -> let tokens2 = match_token tokens1 (Tok_ID k) in
                      (tokens2, FunctionCall (expr5, ID k))

        | Some Tok_Bool l -> let tokens2 = match_token tokens1 (Tok_Bool l) in
                      (tokens2, FunctionCall (expr5, Value (Bool l)))

        | Some Tok_LParen -> let tokens2 = match_token tokens1 (Tok_LParen) in
                      let (tokens3, expr2) = parse_expr tokens2 in
                        let tokens4 = match_token tokens3 (Tok_RParen) in
                            (tokens4, FunctionCall (expr5, expr2))

        | _ -> (tokens1, expr5)


and parse_PrimaryExp toks = 
  match (lookahead toks) with
  | Some Tok_Int i -> let tokens2 = match_token toks (Tok_Int i) in
                  (tokens2, Value (Int i))

  | Some Tok_String j -> let tokens2 = match_token toks (Tok_String j) in
                  (tokens2, Value (String j))

  | Some Tok_ID k -> let tokens2 = match_token toks (Tok_ID k) in
                  (tokens2, ID k)

  | Some Tok_Bool l -> let tokens2 = match_token toks (Tok_Bool l) in
                  (tokens2, Value (Bool l))

  | Some Tok_LParen -> let tokens2 = match_token toks (Tok_LParen) in
                      let (tokens3, expr5) = parse_expr tokens2 in
                      let tokens4 = match_token tokens3 (Tok_RParen) in
                      (tokens4, expr5)

  | _ -> raise (InvalidInputException "failed")


(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match lookahead toks with
  | Some Tok_DoubleSemi -> ([], NoOp)

  | Some Tok_Def -> let tokens2 = match_token toks (Tok_Def) in
                    let (tokens3, expr_ID) = parse_ID tokens2 in
                    let tokens4 = match_token tokens3 (Tok_Equal) in
                    let (tokens5, expr1) = parse_expr tokens4 in
                    let tokens6 = match_token tokens5 (Tok_DoubleSemi) in
                        (tokens6, Def (expr_ID, expr1))
                    
  | _ -> let (tokens2, expr1) = parse_expr toks in
          let tokens3 = match_token tokens2 (Tok_DoubleSemi) in
              (tokens3, Expr(expr1))
