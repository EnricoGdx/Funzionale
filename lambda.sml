datatype lambdat =lambda of (string *lambdat) | var of string


val rec is_free = fn a:string => fn x:lambdat => case x of 
						var(y:string) => true
						|lambda(y:string,z:lambdat)=> case z of
									      var (q:string) => if y=a then false else true
									      |lambda(q:string,w:lambdat)=> if y=a then false else is_free a w
			  
