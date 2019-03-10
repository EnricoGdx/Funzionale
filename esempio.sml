datatype listaoerrore = vuota | cons of (int * listaoerrore) | errore;
datatype intoerrore = num of int | err;

val car = fn errore     => err
           | cons (v,_) => num v
           | vuota      => err;
val cdr = fn errore     => errore
           | cons (_,l) => l
           | vuota      => errore;
val isempty = fn vuota => true
               | _     => false;
