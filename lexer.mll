(* Lexer file in OCamllex for Tureasy *)

{
    open Parser
    open Printf      (* error reporting *)
}

let char  = ['A'-'Z' 'a'-'z']
let digit = ['0'-'9']

rule token = parse
(* whitespace *)
(* single-line comments *)
(* multi-line comments *)
(*                   Syntax                  *)
 '{'                                { LBRACE }
| '}'                               { RBRACE }
| '('                               { LPAREN }
| ')'                               { RPAREN }
| '['                               { LBRACK }
| ']'                               { RBRACK }
| ';'                               { SEMICOLON }
| '.'                               { DOT }
| ','                               { COMMA }
| "#" ( char(char|digit)* as tag)   { TAG_BEGIN(tag) } 
| "#!" ( char(char|digit)* as tag)  { TAG_END(tag)   }   
(*                 Operators                 *)
| '!'                               { UNEG      }
| "++"                              { INCR      }
| "--"                              { DECR      }
| "<<"                              { LSHIFT    }
| ">>"                              { RSHIFT    }
| '^'                               { EXPONENT  }
| '%'                               { MODULO    }
| '*'                               { MULTIPLY  }
| '/'                               { DIVIDE    }
| '+'                               { PLUS      }
| '-'                               { MINUS     } 
| '|'                               { UNION     }
| '&'                               { INTERSECT }
| '~'                               { SETDIFF   }
| '>'                               { GT        }
| ">="                              { GTE       }
| '<'                               { LT        }
| "<="                              { LTE       }
| "=="                              { EQUAL     }
| "!="                              { NOT_EQUAL }
| "AND"                             { AND       }
| "OR"                              { OR        }
| '='                               { ASSIGN    }
| "*="                              { MULT_ASSIGN  }
| "/="                              { DIV_ASSIGN   }
| "+="                              { PLUS_ASSIGN  }
| "-="                              { MINUS_ASSIGN }
| "^="                              { EXP_ASSIGN   }
| "%="                              { MOD_ASSIGN   }
(*                 Datatypes                 *)
(*                 Keywords                  *)
(*                 Literals                  *)

| eof      { EOF }
