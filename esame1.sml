datatype Expr =  X 
                |Y  
		|Avg of Expr * Expr
		|Mul of Expr * Expr


val rec compute = fn z:Expr => fn x:int => fn y:int => (

							case z of 
									 X  => x
									|Y  => y
									|Avg  (x1,y1) => ((compute x1 x y) + (compute y1 x y)) div 2
									|Mul  (x1,y1) => (compute x1 x y) * (compute y1 x y))
		   
