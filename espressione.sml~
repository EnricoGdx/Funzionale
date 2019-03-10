datatype espressione = costante    of int
                     | variabile   of string
                     | somma       of espressione * espressione
                     | sottrazione of espressione * espressione
                     | prodotto    of espressione * espressione
                     | divisione   of espressione * espressione
                     | var         of string      * espressione * espressione

val rec eval = fn (a:string,b:int)::ab => (fn x:espressione => 

		 let

	           val rec valore = fn s:string => fn (a:string,b:int)::ab =>(if s = a then b
									      else valore s ab)
						      |[]=> (0)

		 in

		 case x of
			costante(y:int) => y
			|variabile(y:string) => valore y ((a,b)::ab)
			|var(y:string,z:espressione,v:espressione)=> (eval ((a,b)::ab@[(y,(eval ((a,b)::ab) z))]) v)
			|somma(y:espressione,z:espressione) => (eval ((a,b)::ab) y) + (eval ((a,b)::ab) z)
			|sottrazione(y:espressione,z:espressione) => (eval ((a,b)::ab) y) - (eval ((a,b)::ab) z)
			|prodotto(y:espressione,z:espressione) => (eval ((a,b)::ab) y) * (eval ((a,b)::ab) z)
		        |divisione(y:espressione,z:espressione) => (eval ((a,b)::ab) y) div (eval ((a,b)::ab) z)
		 end)
	  

		|[] => (fn x:espressione => case x of
			 	 costante(y:int) => y
				  |somma(y:espressione,z:espressione) => (eval [] y) + (eval [] z)
				  |sottrazione(y:espressione,z:espressione) => (eval [] y) - (eval [] z)
				  |prodotto(y:espressione,z:espressione) => (eval [] y) * (eval [] z)
				  |divisione(y:espressione,z:espressione) => (eval [] y) div (eval [] z)
		)
