module Cifras where

---------------------------
-- Formateo de la salida --
---------------------------

-- Esta función imprime una solución consistente en una lista de operaciones --

imprimeSol []     = []
imprimeSol (x:xs) = (imprimeOp x)++"\n"++(imprimeSol xs)

-- Esta función imprime una operación de la lista --

imprimeOp (po,so,op,res) = (numToString po)++[' ',op,' ']++(numToString so)++" = "++(numToString res)

-- Esta función transforma un número entero mayor que cero en un string --

numToString 0 = "0"
numToString x = inumToString x
 where inumToString x
        | x == 0 = ""
        | otherwise = ( inumToString ( div x 10 ) ) ++ ( charToString ( mod x 10 ) )
       charToString x
        | x == 0 = "0"
        | x == 1 = "1"
        | x == 2 = "2"
        | x == 3 = "3"
        | x == 4 = "4"
        | x == 5 = "5"
        | x == 6 = "6"
        | x == 7 = "7"
        | x == 8 = "8"
        | x == 9 = "9"

----------------------------------------------------------------------------
-- Funciones de exploracion en el grafo que nos devuelven todos los nodos --
----------------------------------------------------------------------------
exploraGrafoPr compleciones nodo = nodo:(concat ( map ( exploraGrafoPr compleciones ) ( compleciones nodo ) ))

-- Programar aquí la función "exploraGrafoAn" si se desea resolver la última cuestión sobre la práctica.
exploraGrafoAn compleciones nodo = nodo: (exploraGrafoAn' compleciones (compleciones nodo) )
exploraGrafoAn' compleciones []= []
exploraGrafoAn' compleciones xs = 
	let r=concat (map (compleciones) xs) 
	in 
		if(nodoSolucion(r)) then [mejorNodo(r)] else -- esta linea SOLO hace que sea MUCHÍSIMO más rápido al "podar" el árbol
		xs ++ exploraGrafoAn' compleciones r

-- Funcion que dada una lista con al menos dos elementos devuelve todas las parejas de elementos de dicha lista --
------------------------------------------------------------------------------------------------------------------
parejas l= quitarDuplicados( map(\(x,y)->(max x y,min x y )) (pares l))

------------------------------------------------------------------------------------------------------------
-- Función que dada una pareja de números decide qué operaciones se pueden hacer con ellos y en qué orden --
------------------------------------------------------------------------------------------------------------
operaciones x y=[]++
        (if(x>0&&y>0&&x+y/=x&&x+y/=y)then[(x,y,'+',x+y)]else [])++
        (if(x>1&&y>1)then[(x,y,'*',x*y)]else [])++
        (if(x>0&&y>0&&x-y>=0&&x-y/=x&&x-y/=y)then[(x,y,'-',x-y)]else [])++
        (if(x>1&&y>1&&(rem x y==0)&&(quot x y/=x)&&(quot x y/=y))then[(x,y,'/',quot x y)]else[])

----------------------------------------------------------
-- Funcion que dado un nodo devuelven todos sus hijos --
----------------------------------------------------------
--hijosNodo ( 100 , [1,4,5] , [] )
hijosNodo ( objetivo, lista, actualresult )=operar(objetivo,hijos_sin_operar lista,actualresult)  

-------------------------------------------------------------------------------------------------
-- Funciones que comprueban si un nodo es solucion y que comparan si un nodo es mejor que otro --
-------------------hijosNodo ( 100 , [1,4,5] , [] )
------------------------------------------------------------------------------
------------AUXILIARES----------------------------------------------
pares []=[]
pares (x:xs) =[(x,i)|i<-xs] ++ pares xs

hijos_sin_operar lista =map (\(x,y)->((x,y),quitarPrimer x (quitarPrimer y lista))) (parejas lista) 

operar (obj,[],actualresult) = []
operar (o,(((x,y),l):xs),ar) = let ops=map (\e@(_,_,_,result)->(o,result:l,e:ar))  (operaciones x y)  --filtrar operaciones con result en L
	in if(nodoSolucion(ops)) then [mejorNodo(ops)] else (operar (o,xs,ar)) ++ ops


quitarDuplicados [] = []
quitarDuplicados (x:xs) = x : quitarDuplicados (filter (\y -> not(x == y)) xs)


quitarPrimer e []=[]
quitarPrimer e (x:xs)=if (e==x) then xs else x:(quitarPrimer e xs) 

closest o f xs=foldl (\m x->if(abs(f x o)>abs(f m o))then m else x) (head(xs)) xs

distance(o,l,_)=abs$o-closest o (-) l 

revertir [] = []
revertir (x:xs) = revertir xs ++ [x]


------------FIN  AUXILIARES-----------------------------------------

nodoSolucion xs= length (filter (\(o,l,ops)->(length [x|x<-l,x==o])>0) xs ) > 0
mejorNodo xs=foldl(\m x->if(distance x>distance m) then m else x) (head(xs)) xs

---------------------------
-- Funciones principales --
---------------------------
-- Programar las funciones "cifras" (también "cifrasMin" en caso de querer resolver la última cuestión) y todas las funciones auxiliares que se requieran.

cifras l o =	let	camino=exploraGrafoPr hijosNodo ( o, l, [] )
			(_,_,result0)=foldl(\m x->mejorNodo[m,x]) (head(camino)) camino
		in 	imprimeSol(revertir result0)

cifrasMin l o =	let	camino=exploraGrafoAn hijosNodo ( o, l, [] )
			(_,_,result0)=foldl(\m x->mejorNodo[m,x]) (head(camino)) camino
		in 	imprimeSol(revertir result0)

