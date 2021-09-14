(* DEMO Lexer file in OCamllex for Tureasy *)
(* Using another file as tokem type generated by Parser is not currently available*)

{
    (*open Parser*)
    open Printf      (* error reporting *)
    open Lexing

    type token = 
    | LBRACE
    | RBRACE 
    | LPAREN
    | RPAREN 
    | LBRACK 
    | RBRACK
    | SEMICOLON
    | DOT
    | COLON
    | COMMA
    | TAG_BEGIN of string 
    | TAG_END of string
    | UNEG 
    | INCR
    | DECR
    | LSHIFT
    | RSHIFT
    | EXPONENT
    | MODULO
    | MULTIPLY
    | DIVIDE
    | PLUS
    | MINUS
    | UNION
    | INTERSECT
    | SETDIFF 
    | GT
    | GTE
    | LT
    | LTE
    | EQUAL
    | NOT_EQUAL
    | AND
    | OR
    | ASSIGN
    | MULT_ASSIGN
    | DIV_ASSIGN
    | PLUS_ASSIGN
    | MINUS_ASSIGN
    | EXP_ASSIGN
    | MOD_ASSIGN
    | VOID
    | BOOL
    | INT
    | LONG
    | FLOAT
    | STRING 
    | MATRIX
    | GRAPH
    | NUMSET 
    | STRSET 
    | STRUCT
    | LINK 
    | IF
    | ELSE 
    | LOOP
    | BREAK
    | CONTINUE 
    | RETURN 
    | CASE 
    | DEFAULT
    | CONST 
    | TRUE
    | FALSE 
    | NULL 
    | INT_L of int
    | FLOAT_L of float
    | STRING_L of string
    | ID of string
    
    exception EOF

    exception SyntaxError of string
    
    let error character message start = 
    sprintf "line %d: %s : %s" start.pos_lnum character message
    
    let syntax_error lexbuf message = 
    raise ( SyntaxError(error (lexeme lexbuf) (message) (lexeme_start_p lexbuf)) )

}

let letter  = ['A'-'Z' 'a'-'z']
let digit = ['0'-'9']

rule token = parse
    [' ' '\t' '\r' '\n']               { token lexbuf }        (* whitespace *)
(*| "$*"                                 { ml_comment lexbuf }   (* multi-line comments *)  *)
(*| "$"                                  { sl_comment lexbuf }   (* single-line comments *)  *)
(*                   Syntax                  *)
| '{'                                  { LBRACE }
| '}'                                  { RBRACE }
| '('                                  { LPAREN }
| ')'                                  { RPAREN }
| '['                                  { LBRACK }
| ']'                                  { RBRACK }
| ';'                                  { SEMICOLON }
| '.'                                  { DOT }
| ':'                                  { COLON }
| ','                                  { COMMA }
| "#" (letter(letter|digit)* as tag)   { TAG_BEGIN(tag) } 
| "#!" (letter(letter|digit)* as tag)  { TAG_END(tag)   }   
(*                 Operators                 *)
| '!'                                  { UNEG      }
| "++"                                 { INCR      }
| "--"                                 { DECR      }
| "<<"                                 { LSHIFT    }
| ">>"                                 { RSHIFT    }
| '^'                                  { EXPONENT  }
| '%'                                  { MODULO    }
| '*'                                  { MULTIPLY  }
| '/'                                  { DIVIDE    }
| '+'                                  { PLUS      }
| '-'                                  { MINUS     } 
| '|'                                  { UNION     }
| '&'                                  { INTERSECT }
| '~'                                  { SETDIFF   }
| '>'                                  { GT        }
| ">="                                 { GTE       }
| '<'                                  { LT        }
| "<="                                 { LTE       }
| "=="                                 { EQUAL     }
| "!="                                 { NOT_EQUAL }
| "AND"                                { AND       }
| "OR"                                 { OR        }
| '='                                  { ASSIGN    }
| "*="                                 { MULT_ASSIGN  }
| "/="                                 { DIV_ASSIGN   }
| "+="                                 { PLUS_ASSIGN  }
| "-="                                 { MINUS_ASSIGN }
| "^="                                 { EXP_ASSIGN   }
| "%="                                 { MOD_ASSIGN   }
(*                 Datatypes                 *)
| "void"                               { VOID      }
| "bool"                               { BOOL      }
| "int"                                { INT       }
| "long"                               { LONG      }
| "float"                              { FLOAT     }
| "string"                             { STRING    }
| "matrix"                             { MATRIX    }
| "graph"                              { GRAPH     }
| "numset"                             { NUMSET    }
| "strset"                             { STRSET    }
| "struct"                             { STRUCT    }
(*                 Keywords                  *)
| "link"                               { LINK      }
| "if"                                 { IF        }
| "else"                               { ELSE      }
| "loop"                               { LOOP      }
| "break"                              { BREAK     }
| "continue"                           { CONTINUE  }
| "return"                             { RETURN    }
| "case"                               { CASE      }
| "default"                            { DEFAULT   }
| "const"                              { CONST     }
(*                 Literals                  *)
| "true"                               { TRUE      }
| "false"                              { FALSE     }
| "NULL"                               { NULL      }
| ['-']?digit+ as lxm                  { INT_L(int_of_string lxm)}
| ['-']?digit+['.']digit+ as lxm       { FLOAT_L(float_of_string lxm)}
| '"' (([^ '"'] | "\\\"")* as lxm) '"' { STRING_L(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*  as lxm { ID(lxm) }
| ['0'-'9' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*      as lxm { syntax_error (lexbuf) ("Invalid identifier name " ^ lxm ^ "\n") }
| eof                                  { raise EOF }
(* Raising error for unidentified character*)
| _                                    { syntax_error (lexbuf) ("Unexpected character detected\n") }  

and ml_comment = parse
  "*$"                                 { token lexbuf }
(*  | eof                                { syntax_error (lexbuf) ("Expected '*$' before EOF\n")}  *)
  | _                                  { ml_comment lexbuf }

and sl_comment = parse
  "\n"                                 { token lexbuf }
  | eof                                { raise EOF }
  | _                                  { sl_comment lexbuf }


{
    let main () = begin
      try 
        let lexbuf = from_channel stdin in
        while true do 
          let inp_token = token lexbuf in 
          match inp_token with 
          | LBRACE -> printf "LBRACE "
          | RBRACE -> printf "RBRACE "
          | LPAREN -> printf "LPAREN "
          | RPAREN -> printf "RPAREN "
          | LBRACK -> printf "LBRACK "
          | RBRACK -> printf "RBRACK "
          | SEMICOLON -> printf "SEMICOLON "
          | DOT -> printf "DOT "
          | COMMA -> printf "COMMA "
(*          | TAG_BEGIN(str) -> printf "TAG_BEGIN  %s  "
          | TAG_END(str) -> printf "TAG_END  %s   " *)
          (*  *)
        done
      with EOF -> exit 0
    end ;;
    main () ;;
}
