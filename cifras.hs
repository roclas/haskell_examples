module Main where

sumar(a,b)=a+b
restar(a,b)=a-b
multiplicar(a,b)=a*b
dividir(a,b)=a/b
f_operadores= zip [ "+","-","*","/"] [ sumar,restar,multiplicar,dividir ]
first(x,_) = x
second(_,x) = x
get_funct(symbol)=head(map (second) (filter (\i->symbol==first(i)) f_operadores))

--combinaciones (con repetición de una lista tomados de n en n)
--y permutaciones de una lista
combinar n = sequence . replicate n
permutar(l)=case(l)of{_:_:_->l>>=(\i->map(i:)(permutar$filter(i/=)l));_->[l]}

--aplica lista de operaciones a lista de elementos
aplicar([],a:b,objetivo,historia)=(a,historia)
aplicar(o:operandos,a:b:resto,objetivo,historia)=
	let r=get_funct(o)(a,b)
	in 
		if(objetivo==r) then (r,historia++o++show b)
		else aplicar(operandos,r:resto,objetivo,historia++o++show b)

--aplica una lista de operaciones a varias listas de elementos
reaplicar(_,[],objetivo,hist)=([],hist)
reaplicar(operandos,e:l_elementos,objetivo,hist)=
	let 
		a=aplicar(operandos,e,objetivo,hist)
		r=reaplicar(operandos,l_elementos,objetivo,hist)
	in ([first(a)]++first(r),second(a)++second(r))

--aplica varias listas de operaciones a cada una de varias listas de elementos
re_reaplicar([],_,objetivo,hist)=[]
re_reaplicar(o:l_operandos,l_elementos,objetivo,hist)=
	first(reaplicar(o,l_elementos,objetivo,hist))++re_reaplicar(l_operandos,l_elementos,objetivo,hist)


resultado=re_reaplicar([["+","-"],["+","+"]],[[2,3,4],[1,1,2]],9,"")
