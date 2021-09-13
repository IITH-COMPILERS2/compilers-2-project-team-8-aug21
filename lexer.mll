(* Lexer file in OCamllex for Tureasy *)

{
    open Parser
    open Printf      (* error reporting *)
}

let char  = ['A'-'Z' 'a'-'z']
let digit = ['0'-'9']

rule token = parse
    [' ' '\t' '\r' '\n']            { token lexbuf }        (* whitespace *)
| "$*"                              { ml_comment lexbuf }   (* multi-line comments *)
| "$"                               { sl_comment lexbuf }   (* single-line comments *)
(*                   Syntax                  *)
| '{'                               { LBRACE }
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
| "void"                            { VOID      }
| "bool"                            { BOOL      }
| "int"                             { INT       }
| "long"                            { LONG      }
| "float"                           { FLOAT     }
| "string"                          { STRING    }
| "matrix"                          { MATRIX    }
| "graph"                           { GRAPH     }
| "numset"                          { NUMSET    }
| "strset"                          { STRSET    }
(*                 Keywords                  *)
| "link"                            { LINK      }
| "if"                              { IF        }
| "else"                            { ELSE      }
| "loop"                            { LOOP      }
| "break"                           { BREAK     }
| "continue"                        { CONTINUE  }
| "return"                          { RETURN    }
| "case"                            { CASE      }
| "default"                         { DEFAULT   }
| "const"                           { CONST     }
| "new"                             { NEW       }
| "del"                             { DEL       }
| "struct"                          { STRUCT    }
| "public"                          { PUBLIC    }
| "private"                         { PRIVATE   }
(*                 Literals                  *)
| "true"                            { TRUE      }
| "false"                           { FALSE     }
| "NULL"                            { NULL      }
| digit+ as lxm                     { INT_L(int_of_string lxm)}
|                                   { FLOAT_L()}
|
|
| eof      { EOF }
| _ as ch { raise (Failure("illegal character detected " ^ Char.escaped ch)) }  (* Raising error for unidentified character*)

and ml_comment = parse
  "*$"                              { token lexbuf }
  | _                               { ml_comment lexbuf }

and sl_comment = parse
  ['\n']                            { token lexbuf }
  | _                               { sl_comment lexbuf }
