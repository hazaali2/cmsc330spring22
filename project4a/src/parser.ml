
let parse_LetExp toks = 
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



