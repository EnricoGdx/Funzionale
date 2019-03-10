datatype evento = entra of (string * int) | esce of (string * int)

val rec ordinaevento =
         fn [] => []
	 |(x:evento)::xs =>  
          let

       		val valore = fn entra(tiz,x) => x
			      | esce(tiz,x) => x

		val rec succ = fn [] => 1000000
				|(x::xs)  => valore x

		val rec lunghezza = fn [] => 0
			             |(x::xs) => 1 + lunghezza(xs) 


		val rec pfor = fn x:int => fn n:int => 
			fn [] => []
			|(y::ys) => let 
			    val rec sfor = fn x:int => fn c:int => fn n:int => fn [] => []
							   	               |(y::ys) => if valore y < succ (ys) then sfor x c n (ys@[y])
									                   else if x = n then pfor (c-1) n (y::ys)
										                else sfor (x+1) c n (y::ys)
								   in
				
										if x = 0 then y::ys
										else sfor 0 x n (y::ys)
								   end

	in
		
				pfor (lunghezza(x::xs)) (lunghezza(x::xs)) (x::xs) 

	end;
