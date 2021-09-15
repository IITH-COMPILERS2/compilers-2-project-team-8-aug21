(* Lexer file in OCamllex for Tureasy *)

{
    open Parser
    open Printf      (* error reporting *)

    let linecounter = ref 1

    exception SyntaxError of string
    
    let error character message lnum = 
    sprintf "line %d: %s : %s" lnum character message
    
    let syntax_error lexbuf message lnum = 
    raise ( SyntaxError(error (lexeme lexbuf) (message) (lnum)) )

}

let letter  = ['A'-'Z' 'a'-'z']
let digit = ['0'-'9']

rule token = parse
  [' ' '\t' '\r']                      { token lexbuf }        (* whitespace *)
| ['\n']                               { incr linecounter ; token lexbuf }
| "$*"                                 { ml_comment lexbuf }   (* multi-line comments *)
| "$"                                  { sl_comment lexbuf }   (* single-line comments *)
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
(*                 Datatypes                 *)
| "void"                               { VOID      }
| "bool"                               { BOOL      }
| "int"                                { INT       }
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
| ['0'-'9' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*      as lxm { syntax_error (lexbuf) ("Invalid identifier name " ^ lxm) (!linecounter) }
| eof                                  { EOF }
(* Raising error for unidentified character*)
| _                                    { syntax_error (lexbuf) ("Unexpected character detected") (!linecounter) }  

and ml_comment = parse
  "*$"                                 { token lexbuf }
| "\n"                                 { incr linecounter ; ml_comment lexbuf }
| eof                                  { syntax_error (lexbuf) ("Expected '*$' before EOF") (!linecounter) }
| _                                    { ml_comment lexbuf }

and sl_comment = parse
  "\n"                                 { incr linecounter ; token lexbuf }
| eof                                  { EOF }
| _                                    { sl_comment lexbuf }
