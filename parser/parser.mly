%{
  open Ast
%}
%token <int> NUMBER
%token <string> ID
%token INT BOOL DEF FUNDEF INPUT ARRAY
%token IF ELSE TRUE FALSE COLON SEMICOLON REF STAR WHILE RETURN FIRST SECOND
%token PLUS MINUS LESSTHAN GREATERTHAN EQ AND OR COMMA DOT
%token LEFT_PARENTHESIS RIGHT_PARENTHESIS LEFT_BRACE RIGHT_BRACE LEFT_SQ_BRACKET RIGHT_SQ_BRACKET
%token EOF

%nonassoc ARRAY
%left AND OR 
%left EQ 
%left LESSTHAN GREATERTHAN
%left PLUS MINUS 
%left STAR
%left LEFT_SQ_BRACKET
%left DOT

%type <Ast.typ> typ
%type <Ast.def> fundef
%type <Ast.expr> expr
%type <Ast.expr list> expr_list
%type <(Ast.typ * string) list> param_list
%type <Ast.stmt> stmt
%type <Ast.def list> fundef*
%type <Ast.stmt list> stmt*

%start <Ast.prog> parse
%%

parse: 
  | dl=fundef* sl=stmt* EOF { Program (dl, sl) }
  ;
fundef:
  | FUNDEF f=ID LEFT_PARENTHESIS RIGHT_PARENTHESIS COLON t=typ LEFT_BRACE sl=stmt* RIGHT_BRACE { FunDef (t, f, [], sl) }
  | FUNDEF f=ID LEFT_PARENTHESIS pl=param_list RIGHT_PARENTHESIS COLON t=typ LEFT_BRACE sl=stmt* RIGHT_BRACE { FunDef (t, f, pl, sl) }
  ;
param_list:
  | p=ID COLON t=typ { [t, p] }
  | p=ID COLON t=typ COMMA pl=param_list { (t, p) :: pl }
  ;
stmt:
  | WHILE e=expr LEFT_BRACE sl=stmt* RIGHT_BRACE { LoopStmt (e, sl) }
  | DEF x=ID COLON t=typ EQ e=expr SEMICOLON { DefStmt (t, x, e) }
  | DEF x=ID COLON t=typ EQ LEFT_BRACE el=expr_list RIGHT_BRACE SEMICOLON { DefArrInitStmt (t, x, el) }
  | x=ID EQ e=expr SEMICOLON { StoreStmt (Ref x, e) }
  | STAR e1=expr EQ e2=expr SEMICOLON { StoreStmt (e1, e2) }
  | IF e=expr LEFT_BRACE sl=stmt* RIGHT_BRACE { IfStmt (e, sl, []) }
  | IF e=expr LEFT_BRACE sl1=stmt* RIGHT_BRACE ELSE LEFT_BRACE sl2=stmt* RIGHT_BRACE { IfStmt (e, sl1, sl2) }
  | RETURN e=expr SEMICOLON { ReturnStmt e }
  | x=ID EQ INPUT LEFT_PARENTHESIS RIGHT_PARENTHESIS SEMICOLON { InputStmt x }
  | x=ID EQ f=ID LEFT_PARENTHESIS RIGHT_PARENTHESIS SEMICOLON { CallStmt (x, f, []) }
  | x=ID EQ f=ID LEFT_PARENTHESIS el=expr_list RIGHT_PARENTHESIS SEMICOLON { CallStmt (x, f, el) }
  | x=ID LEFT_SQ_BRACKET e1=expr RIGHT_SQ_BRACKET EQ e2=expr SEMICOLON { UpdateStmt (x, e1, e2) }
  ;
expr_list:
  | e=expr { [e] }
  | e=expr COMMA el=expr_list { e :: el }
  ;
expr:
  | n=NUMBER { Num n }
  | MINUS n=NUMBER { Num (n * (-1)) }
  | x=ID { Id x }
  | REF x=ID { Ref x }
  | STAR e=expr %prec STAR { Deref e }
  | t=typ LEFT_SQ_BRACKET e1=expr RIGHT_SQ_BRACKET LEFT_PARENTHESIS e2=expr RIGHT_PARENTHESIS { Array (t, e1, e2) }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | LEFT_PARENTHESIS e=expr RIGHT_PARENTHESIS { e }
  | e1=expr PLUS e2=expr { Add (e1, e2) }
  | e1=expr MINUS e2=expr { Sub (e1, e2) }
  | e1=expr LESSTHAN e2=expr { Lt (e1, e2) }
  | e1=expr GREATERTHAN e2=expr { Gt (e1, e2) }
  | e1=expr EQ EQ e2=expr { Eq (e1, e2) }
  | e1=expr AND e2=expr { And (e1, e2) }
  | e1=expr OR e2=expr { Or (e1, e2) }
  | e1=expr LEFT_SQ_BRACKET e2=expr RIGHT_SQ_BRACKET { Index (e1, e2) }
  | LEFT_PARENTHESIS e1=expr COMMA e2=expr RIGHT_PARENTHESIS { Tuple (e1, e2) }
  | e=expr DOT FIRST { First e }
  | e=expr DOT SECOND { Second e }
  ;
typ:
  | INT { TInt }
  | BOOL { TBool }
  | LEFT_PARENTHESIS t=typ RIGHT_PARENTHESIS { t }
  | t=typ STAR { TPtr t }
  | t=typ ARRAY { TArray t }
  | t1=typ STAR t2=typ { TTuple (t1, t2) }
  ;
