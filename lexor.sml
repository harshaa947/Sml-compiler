val args = CommandLine.arguments();

datatype TOKEN = UNMINUS of int * int 
	 | BINADD of int * int
	 | BINSUB of int * int
	 | BINDIV of int * int 
	 | BINMUL of int * int 
	 | BINMOD of int * int 
	 | NEG of int * int 
	 | AND of int * int 
	 | OR of int * int 
	 | ASSIGN of int * int 
	 | EQ of int * int 
	 | NE of int * int 
	 | LT of int * int 
	 | LTE of int * int 
	 | GT of int * int 
	 | GTE of int * int
	| LP of int * int 
	| RP of int * int 
	| LB of int * int 
	| RB of int * int 
	| EOS of int * int 
	| COMMA of int * int 
	| INT of int * int 
	| BOOL of int * int 
	| IF of int * int 
	| THEN of int * int 
	| ELSE of int * int 
	| WHILE of int * int 
	| PROC of int * int 
	| PRINT of int * int
	| READ of int * int 
	| ERROR of int * int * int 
	| INTLIT of int * int * int 
	| IDENT of int * int * string 
	| BOOLVAL of int * int * bool
	
	
	fun toString TOKEN =
		case TOKEN of
		UNMINUS(a,b) => "UNMINUS(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| BINADD(a,b) => "BINADD(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| BINSUB(a,b) => "BINSUB(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| BINDIV(a,b) => "BINDIV(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| BINMUL(a,b) => "BINMUL(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| BINMOD(a,b) => "BINMOD(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| NEG(a,b) => "NEG(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| AND(a,b) => "AND(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| OR(a,b) => "OR(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| ASSIGN(a,b) => "ASSIGN(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| EQ(a,b) => "EQ(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| NE(a,b) => "NE(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| LT(a,b) => "LT(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| LTE(a,b) => "LTE(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| GT(a,b) => "GT(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| GTE(a,b) => "GTE(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| LP(a,b) => "LP(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| RP(a,b) => "RP(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| LB(a,b) => "LB(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| RB(a,b) => "RB(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| EOS(a,b) => "EOS(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| COMMA(a,b) => "COMMA(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| INT(a,b) => "INT(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| BOOL(a,b) => "BOOL(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| IF(a,b) => "IF(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| THEN(a,b) => "THEN(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| ELSE(a,b) => "ELSE(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| WHILE(a,b) => "WHILE(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| PROC(a,b) => "PROC(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| PRINT(a,b) => "PRINT(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| READ(a,b) => "READ(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
		| ERROR(a,b,c)=>"ERROR(" ^ Int.toString a ^ "," ^ Int.toString b ^ "," ^ Int.toString c ^")\n"
		| INTLIT(a,b,c)=>"INTLIT(" ^ Int.toString a ^ "," ^ Int.toString b ^ "," ^ Int.toString c ^")\n"
		| IDENT(a,b,c)=>"IDENT(" ^ Int.toString a ^ "," ^ Int.toString b ^ "," ^ c ^ ")\n"
		| BOOLVAL(a,b,c)=>"BOOLVAL("^ Int.toString a ^ "," ^ Int.toString b ^ "," ^ Bool.toString c ^")\n"


(* val delimiterlist =[#"+",#"~",#"-",#"/",#"*",#"%",#"!",#"=",#"(",#")",#"{",#"}",#";",#",",#"&&",#"||",#":=",#"<>",#"<=",#">=",#" ",#"/t",#"/n"]; *)
val delimiterlist =[#"+",#"~",#"-",#"/",#"*",#"%",#"!",#"=",#"(",#")",#"{",#"}",#";",#",",#" ",#"\t",#"\n"];
val operator = [#"+",#"~",#"-",#"/",#"*",#"%",#"!",#"=",#"(",#")",#"{",#"}",#";",#","];
val whitespace = [#" ",#"\t",#"\n"];

local 
		fun intvalc (c) =     (* return the integer value for the character c. *)
		   ord(c) - ord (#"0");


							  (* return the integer value of the (remaining) list of *)
		fun intval2 (l,p) =   (* characters l, where p is the value of the string of *)
		   if l=nil then p    (* characters already processed from the list.         *)
			  else intval2( tl(l), 10*p + intvalc(hd(l)));

		in 
		fun intval(s) =       (* return the integer value for the string s. *)
		   intval2(explode(s),0);
end;

fun getclist (filename:string) = 
    let val f = TextIO.getInstream(TextIO.openIn filename)
	fun loop (clist, f) = 
	    case TextIO.StreamIO.input1 f of
		SOME (c, f') => loop (c::clist, f')
	      | NONE =>  (TextIO.StreamIO.closeIn; clist)
    in  rev(loop ([], f))
    end
	;
	



fun matches [] c = false |
 matches (h::t) c = if c = h then true else matches t c 
fun isSpace c = if (matches whitespace c) then true else false

fun isDelimiterop c = if (matches operator c) then true else false

fun isPotential (h ,a) = if h = #"&" andalso a = #"&" then true
					 else if h = #"|" andalso a = #"|" then true
					 else if h = #":" andalso a = #"=" then true
					 else if h = #"<" andalso (a = #">" orelse a = #"=" ) then true
					  else if h = #">" andalso a = #"=" then true
					 else if h = #"<" andalso a = #"=" then true
					 else false ;
					 
fun process d filename=
 let  
	  val st ="" ;
      val f =  TextIO.openOut filename

		 fun update (x:char, i:int ) = if i=0 then 
									  case x of 
									  #"+" => 1   |
									   #"~" => 2 |
									   #"-" => 3 |
									   #"/" => 4 |
									   #"*" => 5 |
									   #"%" => 6 |
									   #"!" => 7 |
									   #"=" => 8 |
									   #"(" => 9 |
									   #")" => 10 |
									   #"{" => 11 |
									   #"}" => 12 |
									   #";" => 13 |
									   #"," => 14 |
									   #"&" => 15 |
									   #"|" => 16 |
									   #":" => 17 |
									   #"<" => 18 |
									   #">" => 19 |
									   #"i" => 26 |
									   #"b" => 27 |
									   #"t" => 28 |
									   #"e" => 29 |
									   #"w" => 30 |
									   #"p" => 31 |
									   #"r" => 32 |
									   #"f" => 33 |
									   x =>  if Char.isDigit(x) then 35
												else if Char.isAlpha(x) then 34 
											 else ~1 
									else if i=15 andalso x = #"&" then 20
									else if i = 16 andalso x = #"|" then 21
									else if i = 17 andalso x = #"=" then 22
									else if i = 18 andalso x = #">" then 23
									else if i = 18 andalso x = #"=" then 24
									else if i = 19 andalso x = #"=" then 25
									else if i = 26 then if x = #"f" then 71 else if x = #"n" then 37 else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 27 then if x = #"o" then 39  else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 28 then if x = #"t" then 74 else if x = #"h" then 43 else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 29 then if x = #"l" then 46  else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 30 then if x = #"h" then 49  else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 31 then if x = #"r" then 53 else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 32 then if x = #"e" then 59 else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 33 then if x = #"f" then 81 else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 34 then if Char.isAlphaNum(x) then 34 else ~1 
									else if i = 35 then if Char.isDigit(x) then 35 else ~1 
									else if i >70 andalso i < 82 then  if Char.isAlphaNum(x) then 34 else ~1
									else if i = 37 then if x = #"t" then 72  else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 39 then if x = #"o" then 40  else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 40 then if x = #"l" then 73  else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 43 then if x = #"e" then 44  else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 44 then if x = #"n" then 75  else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 46 then if x = #"s" then 47 else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 47 then if x = #"e" then 76  else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 49 then if x = #"i" then 50  else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 50 then if x = #"l" then 51  else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 51 then if x = #"e" then 77  else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 53 then if x = #"o" then 54 else if x = #"i" then 56 else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 54 then if x = #"c" then 78  else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 56 then if x = #"n" then 57  else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 57 then if x = #"t" then 79  else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 59 then if x = #"a" then 60  else if Char.isAlphaNum(x) then 34 else ~1
									else if i = 60 then if x = #"d" then 80  else if Char.isAlphaNum(x) then 34 else ~1
									else ~1  ;
		fun out (i,s,r , co)=	  
		  case i of
             0 => ()|		  
			1 => TextIO.output (f, (toString (BINADD(r,co))) )| 
			2 => TextIO.output (f, (toString (UNMINUS(r,co))) )|
			3 => TextIO.output (f, (toString (BINSUB(r,co))) )|
			4 => TextIO.output (f, (toString (BINDIV(r,co))) )|
			5 => TextIO.output (f, (toString (BINMUL(r,co))) )|
			6 => TextIO.output (f, (toString (BINMOD(r,co))) )|
			7 => TextIO.output (f, (toString (NEG(r,co))) )|
			8 => TextIO.output (f, (toString (EQ(r,co))) )|
			9 => TextIO.output (f, (toString (LP(r,co))) )|
			10 => TextIO.output (f, (toString (RP(r,co))) )|
			11=> TextIO.output (f, (toString (LB(r,co))) )|
			12 => TextIO.output (f, (toString (RB(r,co))) )|
			13 => TextIO.output (f, (toString (EOS(r,co))) )|
			14 => TextIO.output (f, (toString (COMMA(r,co))) )|
			15 => TextIO.output (f, (toString (ERROR(r,co,size s))) )|
			16 => TextIO.output (f, (toString (ERROR(r,co,size s))) )|
			17 => TextIO.output (f, (toString (ERROR(r,co,size s))) )|
			18 => TextIO.output (f, (toString (LT(r,co))) )|
			19 => TextIO.output (f, (toString (GT(r,co))) )|
			20 => TextIO.output (f, (toString (AND(r,co))) )|
			21 => TextIO.output (f, (toString (OR(r,co))) )|
			22 => TextIO.output (f, (toString (ASSIGN(r,co))) )|
			23 => TextIO.output (f, (toString (NE(r,co))) )|
			24 => TextIO.output (f, (toString (LTE(r,co))) )|
			25 => TextIO.output (f, (toString (GTE(r,co))) )|
			71 => TextIO.output (f, (toString (IF(r,co))) )|
			72 => TextIO.output (f, (toString (INT(r,co))) )|
			73 => TextIO.output (f, (toString (BOOL(r,co))) )|
			74 => TextIO.output (f, (toString (BOOLVAL(r,co,true))) )|
			75 => TextIO.output (f, (toString (THEN(r,co))) )|
			76 => TextIO.output (f, (toString (ELSE(r,co))) )|
			77 => TextIO.output (f, (toString (WHILE(r,co))) )|
			78 => TextIO.output (f, (toString (PROC(r,co))) )|
			79 => TextIO.output (f, (toString (PRINT(r,co))) )|
			80 => TextIO.output (f, (toString (READ(r,co))) )|
			81 => TextIO.output (f, (toString (BOOLVAL(r,co,false))) )|
			~1 => TextIO.output (f, (toString (ERROR(r,co,size s))) )|
			35 =>TextIO.output (f, (toString(INTLIT(r,co,intval(s)))) )|
			n  => if n > 25 andalso n< 63 then TextIO.output (f, (toString(IDENT(r,co,s))))
					else TextIO.output (f, (toString (ERROR(r,co,size s))));
			 
			
		 fun parse ([] ,i:int,str , row , column)= (out(i,str,row,column-(size str));TextIO.closeOut f) 
			 | parse ([x], i:int , str ,row , column) =
									if (x = #"\n") then 
									 (out(i,str,row,column-(size str));parse( [] ,0,"",row+1,1) )   
									else 
									if Char.isSpace(x) then (out(i,str,row,column-(size str));parse( [] ,0,"",row,column+1) )
									else if isDelimiterop(x) orelse i = 0 
											then (out(i,str,row,column-(size str)) ; parse( [] ,(update(x , 0)),(Char.toString(x)),row,column+1))
									else if i<26 andalso i > ~1 andalso Char.isAlphaNum(x) 
											then (out(i,str,row,column-(size str)) ; parse( [] ,(update(x , 0)),(Char.toString(x)),row,column+1))
									else  parse( [] ,(update(x , i)),(str ^ (Char.toString(x))),row ,column+1)
			| parse ((h::(t as a::b)), i:int,str ,row , column) = 
									if (h = #"\n") then 
									(out(i,str,row,column-(size str));parse( t ,0,"",row+1,1) )
									else 
									if Char.isSpace(h) then (out(i,str,row,column-(size str));parse( t ,0,"",row,column+1) )
									else if  i = 0 orelse isPotential(h,a) orelse (isDelimiterop(h) andalso (update(h,i) > 25 orelse update(h,i) <20)) 
											then (out(i,str,row,column-(size str)) ; parse( t ,(update(h , 0)),(Char.toString(h)),row,column+1))
								    else if i<26 andalso i > ~1 andalso Char.isAlphaNum(h) 
											then (out(i,str,row,column-(size str)) ; parse( t ,(update(h , 0)),(Char.toString(h)),row,column+1))
									else  parse( t ,(update(h , i)),(str ^ (Char.toString(h))),row ,column+1)
		  in parse (d,0,st , 1, 1 ) 
 end;

fun fromString a b = 
      let val c = getclist (a);
	  in process c b 
	  end;
val myrun = fromString (List.nth(args,0)) (List.nth(args,1)) ;
val _ = OS.Process.exit(OS.Process.success);	  
	  