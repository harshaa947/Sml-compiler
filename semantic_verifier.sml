 val args = CommandLine.arguments();

 use "a5.sml" ;
 
 fun removewhitespace s =
	let val c = explode(s)
	fun isWhitespace g = if g = #" " orelse g = #"\n" orelse g = #"\t" then true else false
	fun temp ([]) = [] |
	 temp (e::f) =  if (isWhitespace(e)) = true then (temp f)  else (e::(temp(f)))
    in implode(temp c)
	end;
	
fun getNextNode s = 
	let val c = explode(s)
	fun getNode ([] ,s ) = ([],s,3) |
	    getNode ((e::f),s) = if e = #","  then (f,s,0) else if e = #"[" then (f,s,1) else if e = #"]" then (f,s,2) else getNode(f,s ^ String.str(e))
	val (a,b,d) = getNode(c,"")
	in (implode(a),b,d)
	 end;
	 (*
	  0 represent sibling after it 
	  1 represent child start after it 
	  2 represent going to upper node
	  3 reresent string ended abruptly
	 *)
exception InvalidTypeCheck;
exception NoDeclaration;
fun uptosibling str = let val (a,b,c) = getNextNode str 

							in if c = 0 then a else if c = 3 then a else  uptosibling a
						end 
fun uptonodeup str = let val (a,b,c) = getNextNode str 

							in if c = 2 orelse c =3 then a else uptosibling a
						end 						

fun toUpper(str) =  
  String.implode (map Char.toUpper (String.explode str))
fun toLower(str) =  
  String.implode (map Char.toLower (String.explode str))  
 
		
fun readlist (infile : string) = let
  val ins = TextIO.openIn infile
  fun loop ins =
   case TextIO.inputLine ins of
      SOME line => line :: loop ins
    | NONE      => []
in
  loop ins before TextIO.closeIn ins
end



local 
 open Substring
in fun removenew s = 
let fun matchp c = if c = #"(" then false else true 
fun matchc c = if c = #"," then false else true 
fun matchr c = if c = #")" then false else true 
fun matchb c = if c = #"\n" then false else true 
val isBlank = String.isSuffix "\n"  s 
val subs= if isBlank = true then ( let val (temp1,temp2) = splitl matchb (full s) in temp1 end )else (full s)
in string(subs) end end


	
fun getIdent l = let fun temp [] outpl = (rev outpl) |
						temp  (l as h::t) outpl = (let val harry =  String.tokens (Char.isSpace)  h 
													  val result = if List.nth(harry , 1) ="IDENT" then true else false
													  in if result then temp t ((removenew h)::outpl) else temp t outpl end) 
					in temp l [] end 

					

													  		
fun getSymbolTree l = let fun updatel (strscope,(x,y)) [] newl  boolj = if boolj then ((strscope,[(x,y)])::newl) else newl

| 
updatel (strscope,(x,y)) (upll as (a,l)::t) newl  boolj = if boolj then if a = strscope then ( updatel (strscope,(x,y)) t ((strscope,((x,y)::l))::newl) false)  
																	 else (updatel (strscope,(x,y)) t ((a,l)::newl)  true)
														else (updatel (strscope,(x,y)) t ((a,l)::newl)  false)
fun temp [] outpl = outpl |
							  temp (l as h::t) outpl = 	(
							  
							  let val harry =  String.tokens (Char.isSpace)  h 
													  val strscope = List.nth(harry , 3) 
													  val newl = updatel (strscope,(List.nth(harry,0),List.nth(harry,2))) outpl [] true 
													  in  temp t newl end) 
					in temp l [] end 
		
	

fun reducescope str = let val alist = explode(str)
						  fun temp []  =  [] |
							   temp (l as h::t)	 = if h = #":" then ( rev t) else temp t 
								in implode ( temp  (rev(alist)))
								end

fun checkvariable var strscope listl= let  
fun gettype var strscope []= "undefined" |
	gettype var strscope (l as (a,m)::t)= let fun findlist varname [] = "undefined" |
												   findlist varname  (l as (a,b)::t) = if a = varname then b else (findlist varname t)	
	
	in if a = strscope then let val result = (findlist var m) 
									in if result = "undefined" then let val abc = reducescope strscope
																in if abc ="" then "undefined" else (gettype var abc listl ) end
									   else result end 							
		else toLower(gettype var strscope t) 
		end		
in (gettype var strscope listl ) end ;



fun matches [] c = false |
 matches (h::t) c = if c = h then true else matches t c 	
(* val intoplist = ["+" , "-" ,"*" ,"/","%" ,"~"]	;
val booloptlist = ["<",">" ,"<=","=",">=","!" ,"<>"]; *)
val intoplist = ["PLUS","MINUS","MUL","DIV","MOD","UMINUS"];
val booloptlist = ["LT","GT","LTE","EQ","GTE","NOT","NEQ"];
val boolop = ["!"];
fun optype opt = if (matches intoplist opt) then ("int","int") else if opt = "!" then ("bool","bool") else if (matches booloptlist opt) then ("bool","both") else ("undefined","undefined")
fun uptoeqsibling str nb = let val (a,b,c) = getNextNode str 

							in (if c = 0 andalso nb = 0 then a else if c = 1 then uptoeqsibling a (nb+1) else if c = 2 then uptoeqsibling a (nb-1) else if b=""  orelse c = 3 orelse nb < 0 then a else uptoeqsibling a nb)
						end
						
fun proceed inptree symt= 
		let  fun valExpression (result,inp,out,check,symt,scope) = let val (a,b,c) = getNextNode inp 
																										     val (fa,fb,fc) = if  result ="both" then ((uptoeqsibling a 0) , out , true) else case b of 
																																	"INT" => if result = "int" then  (a,out,true)
																																				else (#1(getNextNode a),out ^"variable " ^ b ^ " is of type " ^ result^" while matching expression is int\n",true)
																																	| "BOOL" => if result = "bool" then (a,out,true)
																																				else (#1(getNextNode a),out ^"variable " ^ b ^ " is of type " ^ result^" while matching expression is bool\n",true)
																																	| opt => let val (result1,types) =(optype opt)  
																																				  val (fa,fb,fc) = if result1 = "undefined"	then let val result1 = checkvariable opt scope symt 
																																																	  val (fa,fb,fc) =(if result1 = result then (a,out,true)
																																																	                    else if result1="undefined" then (a,out ^"variable " ^ opt ^ " is undefined in scope " ^ scope^"\n",true)
																																																						else (a,out ^"variable "  ^ opt ^"  of type " ^ result^" can not be  matched expression with "^ result1 ^"\n",true)) 
																																																 in (fa,fb,fc) end  
																																									else if result1 ="bool" andalso types ="both" then if result ="bool" then (let val (a1,b1,c1) = getNextNode a
																																																												   val typedf =  case b of 
																																																												                    "INT" => "int" |
																																																																	"BOOL" => "bool" |
																																																																	opt => let val result2 = #1(optype opt)
																																																																				val result2 = if result2 = "undefined" then
																																																																				(checkvariable opt scope symt) else result2
																																																																		         in result2 end
																																																																		val (fa,fb,fc) = if typedf = "undefined" then (a1,out,true) 
																																																																							else valExpression (typedf,a^"variable " ^ opt ^ " is undefined in scope " ^ scope^"\n",out,true,symt,scope)
																																																																		val(fa,fb,fc) = if typedf ="undefined" then valExpression ("both",fa,fb,true,symt,scope) else valExpression (typedf,fa,fb,true,symt,scope)	 
																																																												in (fa,fb,fc) end)
																																																					else ((uptoeqsibling a 0),out ^"variable "  ^ "  of type " ^ result^" can not be  matched expression with "^ result1 ^"\n",true)
																																									else if result1 = result then let val (fa,fb,fc) = valExpression (result,a,out,true,symt,scope)
																																																	  val(fa,fb,fc) = 	valExpression (result,fa,fb,true,symt,scope)		
																																																		in (fa,fb,fc) end 
																																								    else (a,out ^"variable "  ^ "  of type " ^ result^" can not be  matched expression with  "^ result1 ^"\n",true)	  								
																										   in (fa,fb,fc) end
																in (fa,fb,fc) end
		
		fun valCommand (inp,out,check,symt,scope) = let val (a,b,c) = (getNextNode inp)
															 val (a,b,c) = if b ="" then getNextNode a else (a,b,c)
															 val (fa,fb,fc) = (if c = 1 then (
																case b of 
																"CallCmd" => (
																				let val (a,b,c) = getNextNode a 
																					 val result = checkvariable b scope symt
																				  val (fa,fb,fc) = if result = "undefined" then valCommand ((#1(getNextNode a)),out ^"variable " ^ b ^ " is undefined in scope " ^ scope ^"\n",true,symt,scope) 
																									else valCommand((#1(getNextNode a)),out,true,symt,scope)
																						in (fa,fb,fc) end
																)| "PrintCmd" => (
																				let val (a,b,c) = getNextNode a 
																					 val result = checkvariable b scope symt
																				  val (fa,fb,fc) = if result = "undefined" then valCommand ((#1(getNextNode a)),out ^"variable " ^ b ^ " is undefined in scope " ^ scope^"\n",true,symt,scope) 
																									else valCommand((#1(getNextNode a)),out,true,symt,scope)
																						in (fa,fb,fc) end
																)| "ReadCmd" => (
																				let val (a,b,c) = getNextNode a 
																					 val result = checkvariable b scope symt
																				  val (fa,fb,fc) = if result = "undefined" then valCommand ((#1(getNextNode a)),out ^"variable " ^ b ^ " is undefined in scope " ^ scope^"\n",true,symt,scope) 
																									else valCommand((#1(getNextNode a)),out,true,symt,scope)
																						in (fa,fb,fc) end
																)| "AssignCmd" => (
																                let val (a,b1,c) = getNextNode a 
																				val result = checkvariable b1 scope symt
																				  val (fa,fb,fc) = if result = "undefined" then valCommand ((#1(getNextNode a)),out ^"variable " ^ b1 ^ " is undefined in scope " ^ scope^"\n",true,symt,scope) 
																									else let val (fa,fb,fc) = valExpression(result,a,out,true,symt,scope)															
																									      in valCommand((#1(getNextNode fa)),fb,true,symt,scope) end
																						in (fa,fb,fc) end
																)| "WhileCmd" => (
																let  
																				val (fa,fb,fc) =(valExpression ("bool",a,out,true,symt,scope)) 
																				val (a,b,c) = (getNextNode fa) 
																				val (fa,fb,fc) = (if b ="Command" then valCommand (a,fb,true,symt,scope) else (a,fb,fc))
																				  val (fa,fb,fc) = valCommand((#1(getNextNode fa)),fb,true,symt,scope)
																						in (fa,fb,fc) end
																) | "ConditionalCmd" =>(
																let  
																				val (fa,fb,fc) =(valExpression ("bool",a,out,true,symt,scope)) 
																				val (a,b,c) = (getNextNode fa) 
																				val (fa,fb,fc) = (if b ="Command" then valCommand (a,fb,true,symt,scope) else (a,fb,fc))
																				val (a,b,c) = (getNextNode fa) 
																				val (fa,fb,fc) = (if b ="Command" then valCommand (a,fb,true,symt,scope) else (a,fb,fc))
																				  val (fa,fb,fc) = valCommand((#1(getNextNode fa)),fb,true,symt,scope)
																						in (fa,fb,fc) end
																)
																
																|_ => (inp,out,true)
															 ) else (inp,out,true))
															 in (fa,fb,fc) end 
		
		
		fun valProgram(inp,out,check,symt,scope) = let val(a,b,c) = getNextNode (uptoeqsibling (uptoeqsibling inp 0) 0)
															val (fa,fb,fc) = (if b = "proc" andalso c = 1 then let val (a,b,c) = getNextNode (a)
																												  val (fa,fb,fc) = if b ="EPSILON" then (a,out,true) else valProgram(a,out,check,symt,scope^":"^b)
																													in (fa,fb,fc) end else (a,out,true))
															 val fa = uptosibling fa 
															 val (a,b,c) = getNextNode fa 
															 val (fa,fb,fc) = (if b = "Command" andalso c = 1 then valCommand (a,fb,check,symt,scope)	else (a,fb,fc))	
																													in (fa,fb,fc) end 			
			

		fun operate inptree outptree symt = let val(a,b,c)=  getNextNode (#1(getNextNode inptree))
											val (fa,fb,fc) = if c = 1 andalso b = "Program" then valProgram (a,outptree,true,symt,"global") else (a,outptree,false)
											in (fa,fb) end
		
		in operate inptree "" symt
		end 	

fun slurp (filename:string):string =
    let val f = TextIO.getInstream (TextIO.openIn filename)
	val (s, _) = TextIO.StreamIO.inputAll f
    in  TextIO.StreamIO.closeIn; s
    end



fun proceed2 inptree = let
fun valIntdecls (inp,out,scope,pcounter) = let  val (a,b,c) = getNextNode inp 
												val (fa,fb,fc) = if c = 0 orelse c = 2 then  valIntdecls (a,("DECLARE_INT " ^ b ^ " _ _")::out,scope,(pcounter+1)) else (a,out,pcounter)
												     in (fa,fb,fc) end
fun valBooldecls (inp,out,scope,pcounter) = let  val (a,b,c) = getNextNode inp 
												val (fa,fb,fc) = if c = 0 orelse c = 2 then  valBooldecls (a,("DECLARE_BOOL " ^ b ^ " _ _")::out,scope,(pcounter+1)) else (a,out,pcounter)
												     in (fa,fb,fc) end													 
					
 fun valProgram(inp,out,pcounter,scope) = let 						val(a,b,c) = getNextNode inp
																	  val (fa,fb,fc) =(valIntdecls (inp,out,scope,pcounter) ) 
																	  val(a,b,c) = getNextNode fa
																	   val (fa,fb,fc) = (valBooldecls(a,fb,scope,fc))		
																	
																													in (fa,fb,fc) end 			
			


fun operate inptree outptree  = let val(a,b,c)=  getNextNode (#1(getNextNode inptree))
											val (fa,fb,fc) = if c = 1 andalso b = "Program" then valProgram (a,outptree,0,"global") else (a,outptree,0)
											in (fa,fb,fc) end
		
		in operate inptree []
		end



fun todo a b c d = 
      let val readlistsym = readlist b   
	  val inputsymtree = getSymbolTree (getIdent readlistsym )
		val asttree = slurp a
		val (a1,b1) = proceed (removewhitespace asttree) inputsymtree
		val (a2,b2,c2) = (print b1;proceed2 (removewhitespace asttree))
		   
				fun writeString (k :string , b) = 
						let val f = TextIO.openOut b
						in 
							(TextIO.output (f, k );TextIO.closeOut f)
						end
			fun writeFile (l,m) = 
					let fun getStrinf (strlist as h::t, str) = getStrinf(t , (str ^ h ^"\n"))  
							| getStrinf([],str) = str 
						in writeString (getStrinf(l,""),m)
	   
						end
	  in ( writeFile(b2 , c))
end

val myrun = todo (List.nth(args,0)) (List.nth(args,1)) (List.nth(args,2)) (List.nth(args,3));
(* val myrun2 = ghjklop (List.nth(args,2)) (List.nth(args,3));  *)
val _ = OS.Process.exit(OS.Process.success); 
 
	
