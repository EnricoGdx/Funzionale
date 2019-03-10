val rec esame = fn x::xs =>(fn y:real => fn z:real => let 

							  val min = fn v => fn w => if v < w then v 
										    else w

							  val max = fn v => fn w => if v > w then v 
										    else w

						      in

						      if x > min y z then if x < max y z then 1 + esame xs y z
							                  else 0 + esame xs y z
					              else 0 + esame xs y z
						
						      end)

		 |[]=>(fn y:real => fn z:real => 0)
