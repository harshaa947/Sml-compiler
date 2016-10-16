val args = CommandLine.arguments();

datatype SYMBOL = KEYWORD of string * string
| INTSYMBOL of string
| BOOLSYMBOL of string
| PROCSYMBOL of string



fun toString SYMBOL =
case SYMBOL of
KEYWORD(a,b) => a ^"\t" ^ b ^"\n "
| INTSYMBOL(a) => a ^"\tINT\n "
| BOOLSYMBOL(a) => a ^"\tBOOL\n "
| PROCSYMBOL(a) => a ^"\tPROC\n "


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
	|CALL of int * int
	
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
		|CALL(a,b) => "CALL(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"

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

local 
 open Substring
in fun fromString s = 
let fun matchp c = if c = #"(" then false else true 
fun matchc c = if c = #"," then false else true 
fun matchr c = if c = #")" then false else true 
fun matchb c = if c = #"\n" then false else true 
val isBlank = String.isSuffix "/n"  s 
val subs= if isBlank = true then ( let val (temp1,temp2) = splitl matchb (full s) in temp1 end )else (full s)
val (a,b) = splitl matchp subs 
val (c,d) = splitl matchc b
val fgh =  if string(a) = "ERROR" orelse string(a) = "INTLIT" orelse string(a) = "BOOLVAL"  orelse string(a) = "IDENT" then true else false
val (e,f) = if fgh = true  then splitl matchc (extract ((string(d)),1,NONE)) else splitl matchr d
val (g,h) = if fgh = true then splitl matchr f else ( (full "dd"),(full "dd"))
val param1 = intval (String.extract(string(c),1,NONE))
val param2 = if fgh = false then intval (String.extract(string(e),1,NONE)) else intval (String.extract(string(e),0,NONE))
val param3 =  (String.extract(string(g),1,NONE))
in case string(a) of 
     "IF" => IF (param1,param2) |
	 "BINADD" => BINADD(param1,param2) |
	 "BINSUB" => BINSUB(param1,param2) |
	 "BINMUL" => BINMUL(param1,param2) |
	 "BINDIV" => BINDIV(param1,param2) |
	 "BINMOD" => BINMOD(param1,param2) |
	 "UNMINUS" => UNMINUS(param1,param2) |
	 "OR" => OR (param1,param2) |
	 "ELSE" => ELSE (param1,param2) |
	 "EOS" => EOS (param1,param2) |
	 "EQ" => EQ (param1,param2) |
	 "LB" => LB (param1,param2 ) |
	 "LP" => LP (param1,param2 ) |
	 "LT" => LT (param1,param2 ) |
	 "LTE" => LTE (param1,param2 ) |
	 "GT" => GT (param1,param2 ) |
	 "GTE" => GTE (param1,param2 ) |
	 "RB" => RB (param1,param2 ) |
	 "READ" => READ (param1,param2 ) |
	 "RP" => RP (param1,param2 ) |
	 "PRINT" => PRINT (param1,param2 ) |
	 "PROC" => PROC (param1,param2 ) |
	 "CALL" => CALL (param1,param2 ) |
	 "COMMA" => COMMA (param1,param2 ) |
	 "THEN" => THEN (param1,param2 ) | 
	 "WHILE" => WHILE (param1,param2 ) | 
	 "NE" => NE (param1,param2 ) | 
	 "NEG" => NEG (param1,param2) |
	 "ASSIGN" => ASSIGN (param1,param2) |
	 "AND" => AND(param1,param2)|
	 "INT" => INT (param1,param2) |
	 "BOOL" => BOOL (param1,param2)  |
	 "INTLIT" => INTLIT(param1,param2,intval(param3))|
	 "IDENT" => IDENT (param1,param2,param3)|
	 "ERROR" => ERROR(param1,param2,intval(param3))|
	 "BOOLVAL" =>(let val bb = if param3 = "true" then true else false in BOOLVAL (param1,param2,bb) end ) 
	 | _ => ERROR (5,5,5)
end
end		
val ghjjjjk= fromString "INT(11,55)"		
fun readlist (infile : string) = let
  val ins = TextIO.openIn infile
  fun loop ins =
   case TextIO.inputLine ins of
      SOME line => line :: loop ins
    | NONE      => []
in
  loop ins before TextIO.closeIn ins
end



fun getTokenList str = 
let 
fun loop (s as h::t ,tok )=  if String.size h  > 5 then loop( t , ((fromString h)::tok)) else loop(t,tok)
	| loop ([],tok) = (tok)
in rev(loop(str,[])	)
end 


  fun process code =
		let 

	fun valvardef1 ([],s,z,sym,scope,bb) = ([],s,false,sym) |
			valvardef1 ((l as h::(f::g)),s,z,sym,scope,bb) =( case h of 
															COMMA(u,v) =>
															let val (a,b,c,d) =  valvardef (f::g, s ^ " COMMA , VarDef[ " ,true,sym,scope,bb) 
																in  if c = true then (a, b ^ "]", c,d) else (a,b,c,d)
															end
															
															
															|  
																	EOS(u,v) => ((f::g),s ^ " EOS",true,sym)|
															
													         _ => (l,s,false,sym)	)
			|valvardef1 (l,s,z,sym,scope,bb)= (l,s,true,sym)								
		
	   and valvardef ([],s,z,sym,scope,bb) = ([],s,false,sym) |
			valvardef((l as h::t),s,z,sym,scope,bb) =  case h of  IDENT(w,x,y) => 
														let val str = if bb = true then (y ^ " IDENT INT " ^scope) else (y ^ " IDENT BOOL " ^scope)
														val (a,b,c,d) =  valvardef1 (t, s ^ " Ident[ "  ^  y ^  " ],VarDef1[ " ,true,(str::sym),scope,bb) 
																in  if c = true then (a, b ^ "]", c,d) else (a,b,c,sym)
															end
											| _ => (l,s,false,sym)	     
       fun valIntDecl([],s,z,sym,scope) = ([],s,false,sym) |
			valIntDecl((l as h::t),s,z,sym,scope) = case  h of INT(w,x) =>
	                              		let val (a,b,c,d) =  valvardef (t, s ^ " INT , VarDef[ " ,true,sym,scope,true) 
											in  if c = true then (a, b ^ "]", c, d) else (a,b,c,sym)
											end
										| _ => (l,s,false,sym)
		       
		fun valoptIntDecl([],s,z,sym,scope) = ([],s,false,sym) |
			valoptIntDecl((l as h::t),s,z,sym,scope) = case  h of INT(w,x) =>
				let val (a,b,c,d) =			valIntDecl (l,s ^ " IntVarDecls [",true,sym,scope)	
				in if z =true then (a,b ^ "] ,",true,d) else (a,b,false,d)
				end  
					| _ => (l,s^ " IntVarDecls [EPSILON] ,",false,sym)
					
		fun valBoolDecl([],s,z,sym,scope) = ([],s,false,sym) |
			valBoolDecl((l as h::t),s,z,sym,scope) = case  h of BOOL(w,x) =>
	                              		let val (a,b,c,d) =  valvardef (t, s ^ " BOOL , VarDef[ " ,true,sym,scope,false) 
											in  if c = true then
											(a, b ^ "]", c,d) else (a,b,c,sym)
											end
										| _ => (l,s,false,sym)
		       
		fun valoptBoolDecl([],s,z,sym,scope) = ([],s,false,sym) |
			valoptBoolDecl((l as h::t),s,z,sym,scope) =  case  h of BOOL(w,x) =>
				let val (a,b,c,d) =			valBoolDecl (l,s ^ " BoolVarDecls [",true,sym,scope)	
				in if z=true then (a,b ^ "]",true,d) else (a,b,false,d)
				end 
					| _ => (l,s ^ " BoolVarDecls [EPSILON]",false,sym)
					
		fun valVarDecls([],s,z,sym,scope) = ([],s,false,sym) |
            valVarDecls((l as h::t),s,z,sym,scope) = 

                 let val (a,b,c,d) = valoptIntDecl(l, s ^ " VarDecls [  ", true, sym ,scope)
				      val (a,b,c,d)= valoptBoolDecl (a,b,c,d,scope)
	              in (a, b ^ "]",c,d)
				  end;
		 
 fun valBoolExpression ([],s,z) = ([],s,false) |
	 valBoolExpression (l,s,z) = let val (a,b,c) = valBoolE ( valBoolF (l,s ^ "BoolExpression[",z))
									in (a, b ^ "] ",c)
						end 
and valBoolF ([],s,z) = ([],s,false) |
	 valBoolF (l,s,z) = let val (a,b,c) =valBoolF1 (valBoolG(l,s ^ "BoolF[",z))
									in (a, b ^ "] ",c)
						end 
and valBoolG ([],s,z) = ([],s,false) |
	 valBoolG (l,s,z) = let val (a,b,c) =valBoolG1 (valBoolH(l,s ^ "BoolG[",z))
									in (a, b ^ "] ",c)
						end
and valBoolH ([],s,z) = ([],s,false) |
	 valBoolH (l,s,z) = let val (a,b,c) =valBoolH1 (valBoolI(l,s ^ "BoolH[",z))
									in (a, b ^ "] ",c)
						end
and valBoolF1 ([],s,z) = ([],s,false) |
	 valBoolF1 (l as h::t,s,z) = case h of 
				  AND(u,v) =>
				let val (a,b,c) =valBoolF(t,s ^ "BoolF1[ AND ,",z)
									in (a, b ^ "] ",c)
						end
				| _=> (l,s ^ "BoolF1[EPSILON]",true)
and valBoolE ([],s,z) = ([],s,false) |
	 valBoolE (l as h::t,s,z) = case h of 
				  OR(u,v) =>
				(let 
				    val (a,b,c) =  valBoolExpression(t,s ^ "BOOLE [ OR ,",z) 
						in (a, b ^ "] ",c) 
					
						end)
				| _=> (l,s ^ "BoolE[EPSILON]",true)
and valBoolH1 ([],s,z) = ([],s,false) |
	 valBoolH1 (l as h::t,s,z) = case h of 
				  LT(u,v) =>
				let val (a,b,c) =valBoolH(t,s ^ "BoolH1[ LT ,",z)
									in (a, b ^ "] ",c)
						end
				|LTE(u,v) =>
				let val (a,b,c) =valBoolH(t,s ^ "BoolH1[ LTE ,",z)
									in (a, b ^ "] ",c)
						end
			    |GT(u,v) =>
				let val (a,b,c) =valBoolH(t,s ^ "BoolH1[ GT ,",z)
									in (a, b ^ "] ",c)
						end
				|GTE(u,v) =>
				let val (a,b,c) =valBoolH(t,s ^ "BoolH1[ GTE ,",z)
									in (a, b ^ "] ",c)
						end
				| _=> (l,s ^ "BoolH1[EPSILON]",true)
and valBoolI ([],s,z) = ([],s,false) |
	 valBoolI (l as h::t,s,z) = case h of 
				  NEG(u,v) =>
				let val (a,b,c) =valBoolJ(t,s ^ "BoolI[ NEG ,",z)
									in (a, b ^ "] ",c)
						end
				| _=> let val (a,b,c) =valBoolJ(h::t,s ^ "BoolI[ ",z)
									in (a, b ^ "] ",c)
						end
and valBoolJ ([],s,z) = ([],s,false) |
	 valBoolJ (l as h::t,s,z) = case h of 
				  BOOLVAL(u,v,w) =>
				let 
				val u1 = if w = true then "tt" else "ff"
				val (a,b,c) = (t,s ^ "BoolJ[ BoolLiteral[ " ^ u1 ^ "], ",z)
									in (a, b ^ "] ",c)
						end
				| _=> let val (a,b,c) =valIntExpression(l,s ^ "BoolJ[  ",z)
									in (a, b ^ "] ",c)
						end
and valBoolG1 ([],s,z) = ([],s,false) |
	 valBoolG1 (l as h::t,s,z) = case h of 
				  EQ(u,v) =>
				let val (a,b,c) =valBoolG(t,s ^ "BoolG1[ EQ ,",z)
									in (a, b ^ "] ",c)
						end
				|NE(u,v) =>
				let val (a,b,c) =valBoolG(t,s ^ "BoolG1[ NE,",z)
									in (a, b ^ "] ",c)
						end
			    
				| _=> (l,s ^ "BoolG1[EPSILON]",true)
and valIntF ([],s,z) = ([],s,false) |
	 valIntF (l as h::t,s,z) = case h of 
				  UNMINUS(u,v) =>
				let val (a,b,c) =valIntF1(t,s ^ "IntF[ UNMINUS, ",z)
									in (a, b ^ "] ",c)
						end
				| _=> let val (a,b,c) =valIntF1(l,s ^ "IntF[  ",z)
									in (a, b ^ "] ",c)
						end
and valIntT1 ([],s,z) = ([],s,false) |
	 valIntT1 (l as h::t,s,z) = case h of 
				  BINMOD(u,v) =>
				let val (a,b,c) =valIntT(t,s ^ "IntT1[ BINMOD ,",z)
									in (a, b ^ "] ",c)
						end
				|BINMUL(u,v) =>
				let val (a,b,c) =valIntT(t,s ^ "IntT1[ BINMUL ,",z)
									in (a, b ^ "] ",c)
						end
			    |BINDIV(u,v) =>
				let val (a,b,c) =valIntT(t,s ^ "IntT1[ BINDIV ,",z)
									in (a, b ^ "] ",c)
						end
				
				| _=> (l,s ^ "IntT1[EPSILON]",true)
and valIntT ([],s,z) = ([],s,false) |
	 valIntT (l,s,z) = let val (a,b,c) =valIntT1 (valIntF(l,s ^ "IntT[",z))
									in (a, b ^ "] ",c)
						end
and valIntE ([],s,z) = ([],s,false) |
	 valIntE (l as h::t,s,z) = case h of 
				  BINADD(u,v) =>
				let val (a,b,c) =valIntExpression(t,s ^ "IntE[ BINADD ,",z)
									in (a, b ^ "] ",c)
						end
				|BINSUB(u,v) =>
				let val (a,b,c) =valIntExpression(t,s ^ "IntE[ BINSUB ,",z)
									in (a, b ^ "] ",c)
						end
			    
				
				| _=> (l,s ^ "IntE[EPSILON]",true)
and valIntExpression ([],s,z) = ([],s,false) |
	 valIntExpression (l,s,z) = let val (a,b,c) =valIntE (valIntT(l,s ^ "IntExpression[",z))
									in (a, b ^ "] ",c)
						end
and valIntF1 ([],s,z) = ([],s,false) |
	 valIntF1 (l as h::t,s,z) = case h of 
				  IDENT(u,v,w) =>
				let val (a,b,c) =(t,s ^ "IntF1[ IDENT ["  ^ w ^"] ",z)
									in (a, b ^ "] ",c)
						end
				|INTLIT(u,v,w) =>
				let val (a,b,c) =(t,s ^ "IntF1[ IntLiteral [ " ^ (Int.toString w) ^" ] ",z)
									in (a, b ^ "] ",c)
						end
			    |LP(u,v) =>
				let val (a as h::t,b,c) =valBoolExpression(t,s ^ "IntF1[ LP ,",z)
				 val (a,b,c) =  case(c,h) of (true , RP(w,x)) =>(t,b ^ ", RP ",z) | _ => (a,b,false) 
									in (a, b ^ "] ",c)
						end
				
				| _=> (l,s,false)

				
fun valExpression(l,s,z) = let val (a,b,c) = valBoolExpression (l,(s ^ "Expression  \n ["),z);
						in (a, b ^ "] ",c)
						end; 
 
		fun valCommandSeq ([],s,z) = ([],s,false) |
		    valCommandSeq ((l as h::t),s,z) = 
					case h of 
                  LB(w,x)    =>
                  let val (a as (h::t) , b  , c) =  valCommand (t,s ^ " CommandSeq \n  [ LB ,  ",z) 
					  val (a,b,c )  = case h of RB(u,v) => (t,b ^ " ,RB ",true) | _ => (a,b,false)
	              in (a,b ^ " ]",c) 
				  end
				 | _ => (l,s^ "  ",true)
		
		and  valCommand ([],s,z) = ([],s,false) |
		    valCommand ((l as h::t),s,z) = 
					case h of 
                  CALL(w,x)    =>
                  let  
					  val (a as (h::t),b,c )  = (t,s ^ " Command  \n [ CallCmd  \n [ CALL , ",true) 
					  val (a as (h::t),b,c) = case h of LP(u,v)  => (t,b^ "LP,",true) | _=> (a,b,false)
					  val (a as (h::t),b,c) = case (c,h) of (true , IDENT(w,x,y))  => (t,b^ " Ident[ "  ^  y ^  " ] ,",true) | _=> (a,b,false)
					 val (a as (h::t),b,c) = case (c,h) of (true , RP(u,v))  => (t,b^ "  RP ,",true) | _=> (a,b,false)
					 val (a as (h::t),b,c) = case (c,h) of (true , EOS(u,v))  => (t,b^ " ],EOS ,",true) | _=> (a,b,false)
					 val(a,b,c) = if c = true then valCommand(a,b,c) else (a,b,c)
	              in (a,b ^ " ]",c) 
				  end
				 | READ(w,x)    =>
                  let  
					 val (a as (h::t),b,c )  = (t,s ^ " Command  \n [  ReadCmd  \n [READ , ",true) 
					  val (a as (h::t),b,c) = case h of LP(u,v)  => (t,b^ "LP,",true) | _=> (a,b,false)
					  val (a as (h::t),b,c) = case (c,h) of (true , IDENT(w,x,y))  => (t,b^ " Ident[ "  ^  y ^  " ] ,",true) | _=> (a,b,false)
					 val (a as (h::t),b,c) = case (c,h) of (true , RP(u,v))  => (t,b^ " RP ,",true) | _=> (a,b,false)
					 val (a as (h::t),b,c) = case (c,h) of (true , EOS(u,v))  => (t,b^ " ],EOS ,",true) | _=> (a,b,false)
					 val(a,b,c) = valCommand(a,b,c)	 
					 in (a,b ^ " ]",c) 
				  end
				  | PRINT(w,x)    =>
                  let  
					  val (a as (h::t),b,c )  = (t,s ^ " Command  \n [ PrintCmd  \n [ PRINT , ",true) 
					  val (a as (h::t),b,c) = case h of LP(u,v)  => (t,b^ "LP,",true) | _=> (a,b,false)
					  val (a as (h::t),b,c) = case (c,h) of (true , IDENT(w,x,y))  => (t,b^ " Ident[ "  ^  y ^  " ] ,",true) | _=> (a,b,false)
					 val (a as (h::t),b,c) = case (c,h) of (true , RP(u,v))  => (t,b^ " RP ,",true) | _=> (a,b,false)
					 val (a as (h::t),b,c) = case (c,h) of (true , EOS(u,v))  => (t,b^ " ],EOS ,",true) | _=> (a,b,false)
					val(a,b,c) = valCommand(a,b,c)
	              in (a,b ^ " ]",c) 
				  end
				 | IDENT(w,x,y) =>
					let  
					  val (a as (h::t),b,c )  = (t,s ^ " Command  \n  [ AssignmentCmd  \n [Ident[ "  ^  y ^  " ] , ",true) 
					  val (a as (h::t),b,c) = case h of ASSIGN(u,v)  => (t,b^ "ASSIGN,",true) | _=> (a,b,false)
					  val (a as (h::t),b,c) = valExpression(a,b,c)
					 val (a as (h::t),b,c) = case (c,h) of (true , EOS(u,v))  => (t,b^ " ] ,EOS ,",true) | _=> (a,b,false)
					val(a,b,c) = valCommand(a,b,c)
	              in (a,b ^ " ]",c) 
				  end
				| IF(w,x) =>
						let  
					  val (a as (h::t),b,c )  = (t,s ^ " Command  \n [ ConditionalCmd  )\n  [IF,  ",true) 
					  val (a as (h::t),b,c) = valBoolExpression(a,b,c)
					  val (a as (h::t),b,c) = case h of THEN(u,v)  => (t,b^ "THEN,",true) | _=> (a,b,false)
					  val (a as (h::t),b,c) = valCommandSeq(a,b,c)
					  val (a as (h::t),b,c) = case h of ELSE(u,v)  => (t,b^ "ELSE,",true) | _=> (a,b,false)
					  val (a as (h::t),b,c) = valCommandSeq(a,b,c)
					val (a as (h::t),b,c) = case (c,h) of (true , EOS(u,v))  => (t,b^ " ], EOS ,",true) | _=> (a,b,false)
					val(a,b,c) = valCommand(a,b,c)
	              in (a,b ^ " ]",c) 
				  end
				 | WHILE(w,x) =>
				     let  
					  val (a as (h::t),b,c )  = (t,s ^ " Command \n [ WHILECmd \n [IF,  ",true) 
					  val (a as (h::t),b,c) = valBoolExpression(a,b,c)
					  
					  val (a as (h::t),b,c) = valCommandSeq(a,b,c)
					val (a as (h::t),b,c) = case (c,h) of (true , EOS(u,v))  => (t,b^ " ], EOS ,",true) | _=> (a,b,false)
					val(a,b,c) = valCommand(a,b,c)
	              in (a,b ^ " ]",c) 
				  end
				 | _ => (l,s^ "Command [EPSILON]",true)	

          
		  
		
		
		fun valprocDecls ([],s,z,sym,scope) = ([],s,false,sym) |
            valprocDecls((l as h::t),s,z,sym,scope) = 

              case  h of PROC(w,x) =>
	                              		let 
										val ((a as f::g),b,c) =  (t,s^ " ,ProcDecls [ \n [ PROC ",true)
											val ((a as f::g),b,c,e) =case(c,f) of (true,IDENT(w,x,y)) => (g, b ^ " , Ident["  ^  y ^"] " ,true,y) | _ => (a,b,false,"")
											val (a as f::g,b,c,d)  =  valBlock (a,b,c,(e ^ " IDENT PROC " ^ scope)::sym,scope  ^":" ^e)
											val(a,b,c) = case(c,f) of (true,EOS(w,x)) => (g, b ^ " , EOS " ,true) | _ => (a,b,false);
											val (a,b,c,d) = valoptprocDecl(a,b,c,d,scope^e^":")
											in (a,b ^ " ]" ,c,d)
											end
										| _ => (l,s,false,sym)
        				  
		and valoptprocDecl 	([],s,z,sym,scope) = ([],s,true,sym) |
			valoptprocDecl ((l as h::t),s,z,sym,scope) = 
					case h of 
                  PROC(w,x)    =>
                  let val (a , b  , c,d) =  valprocDecls (l,s,z,sym,scope)				 
	              in (a,b,c,d) 
				  end
				 | _ => (l,s^ ", ProcDecls[EPSILON]  ",true,sym)
				 
		and valDeclSeq(l,s,z,sym,scope) = (let val (a,b,c,d) = valVarDecls (l,(s ^ " DeclarationSeq \n ["),z,sym,scope);
		                                 val (a,b,c,d) =valoptprocDecl(a,b,c,d,scope) 
		in (a, b ^ "] ,",true,d)
		end)
		
		and valBlock (l,s,z,sym,scope) = 
		
		let val (a,b,c,d) = valDeclSeq (l,(s ^ "[ Block \n ["),z,sym,scope);
		val (a,b,c)=  valCommandSeq(a,b,c)
		in (a, b ^ "] ]",true,d)
		end;
		
      fun procure [] s = ([] , s, true,[]) |
		    procure (l as (h::t)) s  = 
			 let 
				val (a,b,c,d) = valBlock (l,s  ^ "[ Program \n ",true,[],"global") 
				in  (a,b ^ "]",c, rev(d))
				end; 
	in procure code "" 
 end ;
 
 val samplekeywords = "int  INT \n bool BOOL \n tt BOOLVAL \n ff BOOLVAL \n if IF \n then THEN \n else ELSE \n while WHILE \n proc PROC \n read READ \n print PRINT \n call CALL \n"
 fun todo a b c = 
      let val d = getTokenList (readlist(a))
		  val(e,f,g,h) =  process d  
				fun writeString (k :string , b) = 
						let val f = TextIO.openOut b
						in 
							(TextIO.output (f, k );TextIO.closeOut f)
						end
			fun writeFile (l,m) = 
					let fun getStrinf (strlist as h::t, str) = getStrinf(t , (str ^ h ^"\n"))  
							| getStrinf([],str) = str 
						in writeString (samplekeywords^getStrinf(l,""),m)
	   
						end
	  in (writeString (f , b); writeFile(h , c))
	 
	  end
	  
val myrun = todo (List.nth(args,0)) (List.nth(args,1)) (List.nth(args,2));
val _ = OS.Process.exit(OS.Process.success); 
 
(*   fun test (h::t) = case h  of  INT(a, b)  => true | _ => false;
	
val jkjk = test code *) 	