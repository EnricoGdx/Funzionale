val rec contavere = fn x => fn ([])::zs => if (x []) = true then 1 + contavere x zs 
					   else contavere x zs
			       |(y::ys)::zs => if (x(y::ys)) = true then 1 + contavere x zs 
					       else contavere x zs
			       |[] => 0

