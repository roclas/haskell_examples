module Main where

first(x,_) = x
second(_,y) = y
f_operadores= zip ["+","-","*","/"] [ \(x,y)->x+y,\(x,y)->x-y,\(x,y)->x*y,\(x,y)->x/y]
get_funct(symbol)=head(map (second) (filter (\i->symbol==first(i)) f_operadores))

combinar n = sequence . replicate n
permutar l =case(l)of{_:_:_->l>>=(\i->map(i:)(permutar$filter(i/=)l));_->[l]}

--aplica lista de operaciones a lista de elementos
aplicar(_,a:[],objetivo,historia)=(a,historia++show a)
aplicar(o:operandos,a:resto,objetivo,historia)=
	let 	r=aplicar(operandos,resto,objetivo,historia)
		ar=get_funct(o)(a,first(r))
	in	if(first(r)==objetivo) then (first(r),second(r))
		else (ar,historia++"("++show a++o++second(r)++")")

--aplica una lista de operaciones a varias listas de elementos
reaplicar(_,[],objetivo,hist)=([],[])
reaplicar(operandos,e:l_elementos,objetivo,hist)=
	let 	a=aplicar(operandos,e,objetivo,hist)
		r=reaplicar(operandos,l_elementos,objetivo,hist)
	in ([first(a)]++first(r),second(a):second(r))

--aplica varias listas de operaciones a cada una de varias listas de elementos
re_reaplicar([],_,objetivo,hist)=([],[])
re_reaplicar(o:l_operandos,l_elementos,objetivo,hist)=
	(first(reaplicar(o,l_elementos,objetivo,hist))++first(re_reaplicar(l_operandos,l_elementos,objetivo,hist)),
	second(reaplicar(o,l_elementos,objetivo,hist))++second(re_reaplicar(l_operandos,l_elementos,objetivo,hist)))

menor_distancia d (a,b) (y,z) =if(abs(d-a)<abs(d-y)) then (a,b) else (y,z)
mas_cercano(d,(resultados,historicos))= foldr (menor_distancia d) (9999,"") (zip resultados historicos)
calcular(objetivo,numeros)=mas_cercano(objetivo,re_reaplicar(combinar (length(numeros)-1) ["+","-","*","/"], permutar numeros, objetivo,"" ))
