val rec z = fn [] =>( fn y => 0)
        	|x::xs => (fn y:int => if x > 0 then 1 + (z xs y)
				       else z xs y)
