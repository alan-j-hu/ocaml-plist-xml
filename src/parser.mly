%token ARRAY_START
%token ARRAY_END
%token<string> DATA
%token<float * float option> DATE
%token DICT_START
%token DICT_END
%token FALSE
%token<int> INT
%token<string> KEY
%token<float> REAL
%token<string> STRING
%token TRUE
%token EOI

%start <Plist_tree.t> main

%%

let main := ~ = plist; EOI; { plist }

let plist :=
  | ARRAY_START; arr = list(plist); ARRAY_END; { `Array arr }
  | DICT_START; dict = list(kv); DICT_END; { `Dict dict }
  | FALSE; { `Bool false }
  | str = DATA; { `Data str }
  | date = DATE; { `Date date }
  | int = INT; { `Int int }
  | float = REAL; { `Float float }
  | str = STRING; { `String str }
  | TRUE; { `Bool true }

let kv := k = KEY; v = plist; { (k, v) }
