datatype lambdat = espressione | lambda of (string *lambdat)

val rec vleg = fn (x:string,y:lambdat)::xs => (fn z:string => let
						
						val rec valore = fn v:string => fn [] => false
						    |(s:string,d:lambdat)::sd => if v = s then true 
											  else valore v sd
						in
						     if valore z ((x,y)::xs) then true
						     else vleg xs z

						end)
		|[] => (fn x:string => false)
