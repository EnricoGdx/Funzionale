val rec palindroma = 
  fn [] => true
     |x::xs =>

      let
			
      val rec listainvertita = fn x::xs => (listainvertita xs)@[x]
		               |[] => []	

      in
   
      if (x::xs) = listainvertita(x::xs) then true 
      else false

      end		
