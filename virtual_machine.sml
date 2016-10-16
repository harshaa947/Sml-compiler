val args = CommandLine.arguments();

use "cs1140221.sml" ;

fun getItem (x, l) =   case hd (l) of
							(a,b) => if a = x then  hd(l) else getItem (x ,tl(l)) 
 
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
in fun removenew s = 
let fun matchp c = if c = #"(" then false else true 
fun matchc c = if c = #"," then false else true 
fun matchr c = if c = #")" then false else true 
fun matchb c = if c = #"\n" then false else true 
val isBlank = String.isSuffix "\n"  s 
val subs= if isBlank = true then ( let val (temp1,temp2) = splitl matchb (full s) in temp1 end )else (full s)
in string(subs) end end

fun operatelist [] m = (rev m )|
    operatelist (l as h::t) m = operatelist t ((removenew h) ::m )
	
 

fun checkvalue str = if str ="tt" orelse str ="ff" then true else let val alist = explode(str)
																		fun check x = (ord x > 47) andalso (ord x < 58);
																	   fun isNumber [] = true |
																			isNumber (h::t) = if check h then isNumber t else false 
																				in isNumber alist	
													end


							


			
signature STACK = 
    sig
      type 'a stack
      exception EmptyStack

      val empty : 'a stack
      val isEmpty : 'a stack -> bool

      val push : ('a * 'a stack) -> 'a stack
      val pop : 'a stack -> 'a stack
      val top : 'a stack -> 'a
      val map : ('a -> 'b) -> 'a stack -> 'b stack
      val app :  ('a -> unit) -> 'a stack -> unit
	  val getList : 'a stack -> 'a list
	  val fromList : 'a list -> 'a stack
	 (*  val search : (string * 'a stack) -> 'a  *) 
     (*  note: app traverses from top of stack down *)
    end  
	
	structure Stack :> STACK = 
    struct
      type 'a stack = 'a list
      exception EmptyStack

      val empty : 'a stack = []
      fun isEmpty (l:'a list): bool = 
        (case l of
           [] => true
         | _ => false)

      fun push (x:'a, l:'a stack):'a stack = x::l
      fun top (l:'a stack):'a =
			hd (l) handle List.Empty => raise EmptyStack

		fun pop (l:'a stack):'a stack =
			tl (l) handle List.Empty => raise EmptyStack

    	 fun map (f:'a -> 'b) (l:'a stack):'b stack = List.map f l
      fun app (f:'a -> unit) (l:'a stack):unit = List.app f l
	  fun getList (l : 'a stack):'a list = l
	  fun fromList (l : 'a list):'a stack = l
	  (* fun search (key : string , l : 'a stack ) = getItem(key , (rev (getList l)))  *)
	  
    end

fun getdemarck l = let val slist = Stack.getList l 
					   fun remove [] = [] |
							remove ((x,_)::t) = if x ="proc call" then t else remove t 
                        in Stack.fromList (remove slist) end	
 
fun update (abc , l )  = if Stack.isEmpty l then raise Stack.EmptyStack else 
	  
										let 
										val l = Stack.getList(l)
										fun isEqual ((x,_) ,(y,_)) = if x = y then true else false

										fun temp [] item l = (rev l) 
											| temp (lk as(h::t)) item l =  if isEqual (h,item) then rev ( (rev t)@ (item::l)) else temp t item (h::l)
	                                        in Stack.fromList(temp l abc [])
											end


val tjk = Array.fromList [6, 3, 5, 7];

(* local 
open String in
fun returnStringArray  Str = 
let 
fun matchb c = if c = #" " then true else false 
in tokens (Char.isSpace)  Str
end		 
end *)
fun toUpper(str) =  
  String.implode (map Char.toUpper (String.explode str)) 

fun proceed instr inp out = 
let 
local  in 


fun operate instr inp tau pc out = 
		 let val instruction = Array.sub(instr , pc)
		 val insarray =  Array.fromList ( String.tokens (Char.isSpace) instruction)
		 val instype = toUpper(Array.sub (insarray , 0))  
		 val op1 = (Array.sub(insarray,1)) 
		 val op2 = Array.sub(insarray,2)
		 val op3 = Array.sub(insarray,3)
		 fun tostringbool ghj = if ghj = true then "tt" else "ff"
		in case(instype) of 
			"DECLARE_INT" => operate instr inp (Stack.push((op1,"0"),tau)) (pc+1) out |
			"DECLARE_BOOL" => operate instr inp (Stack.push((op1,"ff"),tau)) (pc+1) out |
			"DECLARE_PROC" => operate instr inp (Stack.push((op1,op2),tau)) (pc+1) out |
			"PRINT" => 
			let val (opvalue1,opvalue2) = getItem (op1,(rev (Stack.getList tau))) 
			in operate instr inp tau (pc+1) (Stack.push(opvalue2,out)) 
			end |
			"READ" => 
			let val abc = Stack.top( inp);
			in 
			operate instr (Stack.pop(inp)) (update((op3,abc),tau)) (pc+1) out 
			end |
		    "CALL" =>
			let val  (opvalue1,opvalue2) = getItem (op1,(rev (Stack.getList tau))) 
			in 
			operate instr inp (Stack.push(("proc call",(Int.toString pc)),tau)) (intval opvalue2) out 
			end |
			"IF" =>
			(let val  (opvalue1,opvalue2) = getItem (op1,(rev (Stack.getList tau))) 
			in 
			if opvalue2 = "tt" then
			operate instr inp tau (pc+1) out 
			else if opvalue2 = "ff" then
			(print opvalue2;operate instr inp tau (intval op2 ) out) 
			else
			operate instr inp tau (pc+1) out end ) |
			"GOTO" => operate instr inp tau (intval op2 ) out |
			"RETURN" => let val (opvalue1,opvalue2) = getItem("proc call" ,(rev (Stack.getList tau)))
							val taup = getdemarck (tau)
							in operate instr inp taup ((intval opvalue2)+1) out end |
			"ASSIGN" => let val isVariable = not (checkvalue op1)  
							val assigned =(print (Bool.toString(isVariable));if isVariable then #2(getItem (op1,(rev (Stack.getList tau))))  else op1) 	
							val newtau = (print assigned ;update ((op3,assigned),tau)) 
							in operate instr inp newtau (pc+1) out  end |
			"PLUS" => let val  op1 = #2 ((getItem (op1,(rev (Stack.getList tau)))))
						  val  op2 = #2 ((getItem (op2,(rev (Stack.getList tau)))))	
			               val  result =  bi2str (add((str2bi op1),(str2bi op2)))	
							val newtau = update ((op3,result),tau) 	
						in operate instr inp newtau (pc+1) out end |
			"MINUS" => 	let val  op1 = #2((getItem (op1,(rev (Stack.getList tau)))))
						  val  op2 = #2((getItem (op2,(rev (Stack.getList tau)))))	
			               val result =  bi2str (sub((str2bi op1),(str2bi op2)))	
							val newtau = (print ("ioppp" ^ result);update ((op3,result),tau)) 	
						in operate instr inp newtau (pc+1) out end |
			"MULT"  => let val  op1 = #2((getItem (op1,(rev (Stack.getList tau)))))
						  val  op2 = (#2((getItem (op2,(rev (Stack.getList tau))))))	
			               val result =  bi2str (mul((str2bi op1),(str2bi op2)))	
							val newtau = update ((op3,result),tau) 	
						in operate instr inp newtau (pc+1) out end | 
			"DIV" => let val  op1 = #2 ((getItem (op1,(rev (Stack.getList tau)))))
						  val  op2 = #2 ((getItem (op2,(rev (Stack.getList tau)))))	
			               val result =  bi2str (div4bigint((str2bi op1),(str2bi op2)))	
							val newtau = update ((op3,result),tau) 	
						in operate instr inp newtau (pc+1) out end |
			"MOD" => let val  op1 = #2 ((getItem (op1,(rev (Stack.getList tau)))))
						  val  op2 = #2 ((getItem (op2,(rev (Stack.getList tau)))))	
			                val result =  bi2str (mod4bigint((str2bi op1),(str2bi op2)))	
							val newtau = update ((op3,result),tau) 	
						in operate instr inp newtau (pc+1) out end |
			"GEQ" => let val  op1 = #2 ((getItem (op1,(rev (Stack.getList tau)))))
						  val  op2 = #2 ((getItem (op2,(rev (Stack.getList tau)))))	
			               val result =   (geq((str2bi op1),(str2bi op2)))	
							val newtau = update ((op3,(tostringbool result)),tau) 	
						in operate instr inp newtau (pc+1) out end |
			"GT" => let val  op1 = #2 ((getItem (op1,(rev (Stack.getList tau)))))
						  val  op2 = #2 ((getItem (op2,(rev (Stack.getList tau)))))	
			               val result =   (gt((str2bi op1),(str2bi op2)))	
							val newtau = update ((op3,(tostringbool result)),tau) 	
						in operate instr inp newtau (pc+1) out end |
            "LEQ" => let val  op1 = #2 ((getItem (op1,(rev (Stack.getList tau)))))
						  val  op2 = #2 ((getItem (op2,(rev (Stack.getList tau)))))	
			                val result =   (leq((str2bi op1),(str2bi op2)))	
							val newtau = update ((op3,(tostringbool result)),tau) 	
						in operate instr inp newtau (pc+1) out end |
			"LT" => let val  op1 = #2 ((getItem (op1,(rev (Stack.getList tau)))))
						  val  op2 = #2 ((getItem (op2,(rev (Stack.getList tau)))))	
			               val result =   (lt((str2bi op1),(str2bi op2)))	
							val newtau = update ((op3,(tostringbool result)),tau) 	
						in operate instr inp newtau (pc+1) out end |
			"NEQ" => let val  op1 = #2 ((getItem (op1,(rev (Stack.getList tau)))))
						  val  op2 = #2 ((getItem (op2,(rev (Stack.getList tau)))))	
			               val result =   (neq((str2bi op1),(str2bi op2)))	
							val newtau = update ((op3,(tostringbool result)),tau) 	
						in operate instr inp newtau (pc+1) out end |	
			"EQ" => let val  op1 = #2((getItem (op1,(rev (Stack.getList tau)))))
						  val  op2 = (#2((getItem (op2,(rev (Stack.getList tau))))))	
			               val result = (print (op1 ^ "op2" ^ op2) ; (eq((str2bi op1),(str2bi op2))))	
							val newtau = update ((op3,(tostringbool result)),tau) 	
						in operate instr inp newtau (pc+1) out end |
			"AND" => let val result =  bi2str (unminus((str2bi op1)))	
							val newtau = update ((op3,( result)),tau) 	
						in operate instr inp newtau (pc+1) out end |
			"OR" => let val result =  bi2str (unminus((str2bi op1)))	
							val newtau = update ((op3,( result)),tau) 	
						in operate instr inp newtau (pc+1) out end |			
			"UMINUS" => let val result =  bi2str (unminus((str2bi op1)))	
							val newtau = update ((op3,( result)),tau) 	
						in operate instr inp newtau (pc+1) out end |	
			"NOT" => let val result =  bi2str (unminus((str2bi op1)))	
							val newtau = update ((op3,(result)),tau) 	
						in operate instr inp newtau (pc+1) out end |			
			"END_OF_CODE" => out |
			 
			_ => out
		end
end

in operate instr inp Stack.empty 0 out 
end

fun readlist (infile : string) = let
  val ins = TextIO.openIn infile
  fun loop ins =
   case TextIO.inputLine ins of
      SOME line => line :: loop ins
    | NONE      => []
in
  loop ins before TextIO.closeIn ins
end


	
val checkList = (operatelist (readlist "input.txt") []) ;
val checkArray = Array.fromList (checkList) ;
print "hjklm" ;
(* val checkArray = Array.fromList ["DECLARE_INT n _ _" , "DECLARE_INT zero _ _" ,"DECLARE_INT one _ _" , "DECLARE_INT ans _ _","PRINT ans _ _","END_OF_CODE _ _ _"] ; *)
val input = Stack.empty;
val inp = Stack.push("100",input);
val check = proceed checkArray inp (Stack.empty);
val x = Stack.getList check ;
val checkl = Stack.getList check ;

fun todo a b  = 
      let 
val check = proceed checkArray inp (Stack.empty)

val checkl = Stack.getList check
		   
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
	  in ( writeFile(checkl , b))
end

val myrun = todo (List.nth(args,0)) (List.nth(args,1));
val _ = OS.Process.exit(OS.Process.success); 
 