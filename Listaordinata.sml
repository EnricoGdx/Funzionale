val rec isor = fn [] => true
	      |x::xs =>
                
               let 

                   val succ = fn [] => 100000000
             		      |x::xs => x
 
               in

		   if x < succ xs then isor xs
                   else false

               end;
