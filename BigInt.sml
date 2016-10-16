fun intvalc (c) =     (* return the integer value for the character c. *)
		   ord(c) - ord (#"0");
		   
fun charint i = let val is = Int.toString i 
					val cl = explode(is)
                   in hd(cl)
				   end
				   
fun mullistbydigit ((al as (ha::ta)) , d , carry , result) = 
						let val hai = intvalc(ha) 
						     val muli = hai * d  + carry
							 val carryi = muli div 10 
							 val resulti = muli mod 10	
							 in mullistbydigit (ta,d,carryi,((charint resulti)::(result)))
							 end 
   | mullistbydigit ([] , d , carry , result) = if carry = 0 then result else (charint carry)::result 

fun isPositive str = String.isPrefix "~" str 


					 
fun geteqzero (str , intef) = let val ad = explode(str) 
								fun fgh ([] , ino , rsl) = if ino = 0 then rsl else fgh([],ino-1,#"0"::rsl) |
									fgh (stl as h::t , ino ,rsl) =  fgh (t,ino-1,h::rsl)	
								in String.implode (fgh(rev ad,intef,[]))
								end ;
								
fun removezero [] = #"0" |
    removezero (l as h::t) = if h = #"0" then removezero (t) else l
	
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
fun isZero str = let val alist = explode(str)
					  val cgecksign = if hd(alist) = #"~" then tl(alist) else alist 
					 val remain = removezero cgecksign
					 in if(null remain) then true else false	
					 end	
	
fun addlist ( (al as (ha::ta)),(bl as (hb::tb)) , carry,result) =    
													let val hai =  intvalc(ha) 
														val hbi =  intvalc(hb)
														val addi = hai + hbi + carry 
														val carryi = addi div 10 
														val resulti = addi mod 10
															in addlist (ta,tb,carryi,((charint resulti)::(result)))
															end
												| addlist ([],[],carry,result) = if carry = 0 then result else (charint carry)::result |
												addlist ([],(bl as (hb::tb)) , carry,result) = 
														let  
														val hbi =  intvalc(hb)
														val addi = hbi + carry 
														val carryi = addi div 10 
														val resulti = addi mod 10
															in addlist ([],tb,carryi,(charint resulti)::result)
															end
															
fun sublist ( (al as (ha::ta)),(bl as (hb::tb)) , carry,result) =    
													let val hai =  intvalc(ha) 
														val hbi =  intvalc(hb)
														val addi = hai - hbi + carry 
														val carryi = addi div 10 
														val resulti = addi mod 10
															in sublist (ta,tb,carryi,((charint resulti)::(result)))
															end
												| sublist ([],[],carry,result) = if carry = 0 then result else (charint carry)::result |
												sublist ((bl as (hb::tb)) , [],carry,result) = 
														let  
														val hbi =  intvalc(hb)
														val addi = hbi + carry 
														val carryi = addi div 10 
														val resulti = addi mod 10
															in sublist (tb,[],carryi,(charint resulti)::result)
															end												
												
fun mullist ( al ,(bl as (hb::tb)),result:char list) =    
													let 
														val hbi =  intvalc(hb)
														val getlist = mullistbydigit(al , hbi , 0 , [])
														val resultadd = 
														if (length(getlist) - 1) < length (result) then 
														addlist((rev getlist) , (rev (result @ [#"0"])),0,[])	
														else addlist((rev (result @ [#"0"])),(rev getlist) ,0,[])
															in mullist (al,tb,resultadd)
															end
												| mullist (al,[],result) = result
												
															

				   
signature BigInt =
sig
type bigint
exception InvalidNumber and DividebyZero

val getbigint: int -> bigint
val bi2str : bigint -> string
val str2bi : string -> bigint
val lt : bigint * bigint ->bool
val leq : bigint * bigint -> bool
val gt : bigint * bigint -> bool
val geq : bigint * bigint -> bool
val eq : bigint * bigint -> bool
val neq : bigint * bigint -> bool
val div4bigint : bigint * bigint -> bigint
val mod4bigint : bigint * bigint -> bigint
val mul : bigint * bigint -> bigint
val add : bigint * bigint -> bigint
 val sub : bigint * bigint -> bigint

val unminus : bigint -> bigint
end

structure BigInt :> BigInt = 
    struct
	type bigint = string
	exception InvalidNumber and DividebyZero
	fun getbigint (integer:int):bigint =  Int.toString integer
	fun bi2str (yui:bigint):string = yui 
	fun str2bi (sdr:string):bigint =  sdr 
	fun unminus (a:bigint):bigint = 	let val aclist = String.explode(a) 
											val nlist =  if isPositive (a) then tl(aclist) else #"~"::aclist
										in String.implode(nlist)
											end 
	fun lt (a:bigint , b:bigint):bool = let val checka = not (isPositive a) 
														val checkb = not (isPositive b)
														val pa = if checka then a else (unminus a)
														val pb = if checkb then b else (unminus b)
														val iop = if String.size(pa) < String.size(pb) then LESS else if  String.size(pa) > String.size(pb) then GREATER else 
															String.compare(pa,pb) in
															if checka then if checkb then if iop = LESS then true else false 
																			else false
															else if checka then true 
																			else if iop = GREATER then true else false
					end
	fun leq (a:bigint , b:bigint):bool=   (lt (a , b)) orelse ( a = b )  
					
	fun gt (a:bigint , b:bigint):bool = not (leq (a , b))
	fun geq (a:bigint , b:bigint):bool = (gt (a , b)) orelse ( a = b ) 
    fun eq (a:bigint , b:bigint):bool = if a = b  then true else false 
					
	fun neq (a:bigint , b:bigint):bool = if a = b then false else true 
	
   
	
	fun removestrzero str = let val aclist = String.explode(str)
								in if hd(aclist) = #"~" then String.implode(removezero (tl(aclist))) else String.implode(removezero aclist)
								end
	fun add (a:bigint , b:bigint) :bigint = let val aclist = String.explode(a)
												val bclist = String.explode(b)
												val checka = not (isPositive a) 
												val checkb = not (isPositive b)
												val aclist = if checka = true then aclist else tl(aclist)
												val bclist = if checkb = true then bclist else tl(bclist)
												val aclist = removezero aclist
												val bclist = removezero bclist
												val stringlist = if (checka) then if checkb then if geq(str2bi(String.implode aclist) , str2bi(String.implode bclist))  then 
																							String.implode ( addlist ((rev bclist),(rev aclist),0,[])) else 
																							 String.implode (addlist ((rev aclist),(rev bclist),0,[]))
																				  else if gt(str2bi(String.implode aclist) , str2bi(String.implode bclist)) then 
																							String.implode ( sublist ((rev aclist),(rev bclist),0,[]))
																						else if eq(str2bi(String.implode aclist) , str2bi(String.implode bclist)) then "0" else String.implode (#"~"::( sublist ((rev bclist),(rev aclist),0,[])))
																else if checkb then if gt(str2bi(String.implode bclist) , str2bi(String.implode aclist)) then 
																						 String.implode ( sublist ((rev bclist),(rev aclist),0,[])) 
																						else if eq(str2bi(String.implode aclist) , str2bi(String.implode bclist)) then "0" else String.implode (#"~"::( sublist ((rev aclist),(rev bclist),0,[])))	
																		else if geq(str2bi(String.implode bclist) , str2bi(String.implode aclist)) then 
																			String.implode (#"~"::(addlist ((rev aclist),(rev bclist),0,[]))) else 
																			 String.implode (#"~"::(addlist ((rev bclist),(rev aclist),0,[])))
												



												
											in 
										(* String.implode(aclist) ^ String.implode(bclist) ^ Bool.toString(checka) ^ Bool.toString(checkb) ^  *) removestrzero stringlist
											end	
											
	fun mul (a:bigint , b:bigint) :bigint = 
											let val aclist = String.explode(a)
												val bclist = String.explode(b)
												val checka = not (isPositive a) 
												val checkb = not (isPositive b)
												val aclist = if checka = true then aclist else tl(aclist)
												val bclist = if checkb = true then bclist else tl(bclist)
												val aclist = removezero aclist
												val bclist = removezero bclist
												val stringlist = if length(aclist) < length (bclist) then 
											String.implode ( mullist ((rev bclist),( aclist),[])) else 
											String.implode ( mullist ((rev aclist),( bclist),[]))
												
											in  if checka = checkb then  removestrzero stringlist else "~" ^ (removestrzero stringlist)
											
	end
	
	
	
	
	fun sub (a:bigint , b:bigint) :bigint = add (a , unminus(b))
							
																		
	
	
	
	fun divandrem 	(a:string , b:string) = if (String.size a) <= 9 andalso (String.size b) <= 9 then let val numa = intval(a) 
																												 val numb = intval(b)
																												 in ( Int.toString( numa div numb) , Int.toString(numa mod numb))
																												end	
											else  let 
											fun divandremeq (a:string , b:string) = let fun divte (sa,sb,q) = let val ba = str2bi sa 
																		  val bb = (str2bi sb) 
																		in if (gt (ba , bb)) then divte(bi2str(sub(ba,bb)),sb,(q+1))
																			else if eq(ba,bb) then (q+1,"") else (q,bi2str(ba))
																		end
												in divte (a,b,0)
												end
											fun checkdiv (stringa,stringb,stringq) =  let  val lengtha = String.size stringa 
																									val   lengthb = String.size stringb
																									val ba = (str2bi stringa )
																									val bb = (str2bi stringb )
																									 in if(geq (ba , bb)) then let val sla1 = String.substring(stringa,0,lengthb) 
																															 val sla =  if (sla1 >= stringb)	then sla1 else String.substring(stringa,0,lengthb +1) 
																															 
																															  val (quo,rem) = (print sla ;divandremeq(sla,stringb))
																															  val qeff = if (sla1 >= stringb)	then stringq else (stringq^"0")
																															  val rem = geteqzero (rem , lengthb -1 )
																															in checkdiv (rem ^ (String.extract(stringa,(String.size sla),NONE)), stringb,qeff ^ (Int.toString quo) )
																																end		
																										else (stringq,stringa)
																									end
																in checkdiv(a,b,"")											
													end
														
	fun div4bigint 	(a:bigint , b:bigint) :bigint = if isZero b then raise DividebyZero else let val checka = not (isPositive a) 
														val checkb = not (isPositive b)
														val pa = if checka then a else (unminus a)
														val pb = if checkb then b else (unminus b)
														val (resa,resb) = divandrem ( pa, pb)
													in if (checka = checkb) then ( removestrzero resa) else ((str2bi ("~" ^(removestrzero resa))))
													end
													
	fun mod4bigint 	(a:bigint , b:bigint) :bigint = if isZero b then raise DividebyZero else let val checka = not (isPositive a) 
														val checkb = not (isPositive b)
														val pa = if checka then a else (unminus a)
														val pb = if checkb then b else (unminus b)
														val (resa,resb) = divandrem ( pa, pb)
													in if (checka) then (removestrzero resb) else ((str2bi ("~" ^(removestrzero resb))))	
													
													end							
											
	end	
	
	open BigInt ;
	
	