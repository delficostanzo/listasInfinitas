-- Listas infinitas: para poder trabajar con listas infinitas, hay que usar funciones recursivas con listas. Una funcion recursiva es aquella que en su definicion se invoca
-- a si misma. Es decir que algo se define a partir de si mismo.
-- Como las listas ya de por si tienen una estructura infinita (estan compuestas por una cabeza y una cola que es tambien una lista), se pueden encontrar varios 
-- ejemplos de como se puede aplicar.
-- Por ejemplo:
-- Si queremos crear una funcion que nos devuelva una lista infinita de muchos 5, se podria escribir de la siguiente manera:
repetirMuchoDe :: Int -> [Int]
repetirMuchoDe numero = numero:(repetirMuchoDe numero)

-- y esta funcion ya exite en la guia de lenguajes y se llama repeat
-- > repeat 5
-- > [5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,.... infinitas veces

-- Orden superior: cuando hablamos de una operacion de orden superior, nos referimos a que dicha operacion recibe otra operacion (comportamiento) 
-- por parametro, para ejecutarlo internamente.
-- Por ejemplo: Una funcion que se llame aplicarFuncionA que tome una lista de numeros y una funcion y que devuelva como resultado 
-- una lista con la funcion aplicada en todos los elementos de la lista.
aplicarFuncionA :: [Int] -> (Int -> Int) -> [Int]
aplicarFuncionA listaDeNumeros funcion = map funcion listaDeNumeros

-- > aplicarFuncionA [1,2,3,4,5,6] (+1)
-- > [2,3,4,5,6,7]

-- Ejemplo de orden superior con listas infinitas:
-- Creo el data monstruo, el cual se sabe su nombre, la fuerza que tiene y las armas que usa para matar a su enemigo (contiene el nombre 
-- y el poder de ataque)
data Monstruo = Monstruo {
    nombre :: String,
    fuerza :: Int,
    armas :: [String]
} deriving Show

type Arma = (String, Int)

mrNorris = Monstruo "Mr Norris" 100 (repeat "Espada de oro")
mrFarrel = Monstruo "Mr Farrel" 200 (repeat "Computadora")
ed = Monstruo "Ed" 70 ["Espada", "Pistola"]
voldemort = Monstruo "Lord Voldemort" 30 ["Varita de Sauco", "Serpiente"]

-- Si yo corro por consola a mrNorris como use repeat en su lista de armas, va a tener infinitas espadas de oro.

-- Ejemplo utilizando orden superior con listas infinitas:
-- Si su fuerza es mayor a 50 entonces cada arma se le agrega a su nombre la palabra "poderosa" al final.

esPoderoso :: Monstruo -> Monstruo
esPoderoso monstruo | fuerza monstruo > 50 = monstruo {armas = (map (++ " poderoso").armas) monstruo }
                    | otherwise = monstruo

-- Si utilizo la funcion esPoderoso en ed, te devuelve:
-- > esPoderoso ed      
-- > Monstruo {nombre = "Ed", fuerza = 70, armas = ["Espada poderoso","pistola poderoso"]}

-- Y si utilizo la funcion es poderoso en voldemort, te devuelve voldemort sin cambios ya que no cumple con la condicion de 
-- que su fuerza es mayor a 50:
-- > esPoderoso voldemort
-- > Monstruo {nombre = "Lord Voldemort", fuerza = 30, armas = ["Varita de Sauco","Serpiente"]}

-- Pero en el caso de MrNorris, como se cumple la condicion que tiene fuerza mayor a 50, debe agregarle a todas las armas la 
-- palabra "poderoso" y para eso tiene que recorrer toda la lista por lo que se genera un bucle infinito.
-- Ademas, esPoderoso es de orden superior porque utiliza una funcion de orden superior predifinida llamada "map".

-- Otro ejemplo podria ser una funcion que devuelva la primer arma de las listas de armas:
primerArma :: Monstruo -> String
primerArma = head.armas 

-- En este caso, si lo aplicamos con Mr Norris, como solo pide el primer elemento de la lista esta funcion romperia ese bucle
-- infinito que se formaba con la funcion esPoderoso. Este puede ser un ejemplo de lazy evaluation, ya que en esta funcion los parametros se evaluan solo si son necesarios. Al preguntar si esta palabra se encuentra en la lista, al estar de comienzo a fin, con tan solo encontrarla una vez ya
-- decide que la decision sin evaluar el resto de la lista
-- > primerElemento mrNorris
-- > "Espada de oro"

-- > primerElemento voldemort
-- > "Varita de Sauco"

tieneComputadora :: Monstruo -> Bool
tieneComputadora monstruito = elem "Computadora" (armas monstruito)

-- Y con la funcion tieneComputadora podria ser otro ejemplo de lazy evaluation ya que, si preguntamos con mrFarrel, como el primer 
-- elemento ya cumple no es necesario que evalue el resto de la lista.
-- > tieneComputadora mrFarrel
-- > True

-- Pero, si lo preguntamos con mrNorris, al ser una lista infinita y no tener una Computadora como arma, el recorrido de la lista no 
-- termina nunca y por eso no va a devolver nunca una respuesta (se genera un bucle infinito)
-- > tieneComputadora mrNorris
-- > 