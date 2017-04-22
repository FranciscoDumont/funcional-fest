{-Punto 1 
Modelar el tipo de dato cliente. Justificar el criterio utilizado. -}
data Cliente = Cliente {edad :: Int,nombre :: String,resistencia :: Int,amigos :: [Cliente]} deriving Show
{-Punto 2 
● Modelar a Rodri, que tiene 55 de resistencia y no considera a nadie como 
amigo.  
● También modelar a Marcos, un cliente que tiene resistencia 40 y considera a 
Rodri como su único amigo. 
● Tenemos a Cristian, un cliente cuya resistencia es 2, y no considera a nadie 
como amigo. 
● Por último está Ana, una cliente que tiene 120 de resistencia y considera a 
Marcos y a Rodri como amigos. -}
rodri = Cliente {edad= 20,amigos=[],nombre = "Rodri",resistencia = 55}
marcos = Cliente {edad=30,amigos = [rodri],resistencia =40,nombre = "Marcos"} 
cristian = Cliente {edad=40,amigos=[],resistencia= 2,nombre="Cristian"}
ana = Cliente {edad=35,amigos=[rodri,marcos],resistencia=120,nombre="Ana"}
{-Punto 3 
Desarrollar la función ​ comoEsta​  que dice cómo está un cliente 
● Si su resistencia es mayor a 50, está “fresco” 
● Si no está “fresco”, pero tiene más de un amigo, está “piola” 
● En caso contrario, está “duro”-}

comoEsta :: Cliente->String
comoEsta cliente 
     | ((>50).resistencia) cliente = "fresco"
     | ((>1).length.amigos) cliente ="piola"
     | otherwise ="duro"

{-Punto 4   
Hacer que un cliente reconozca como amigo a otro cliente, respetando las 
restricciones definidas por el negocio (no agregar más de una vez al mismo amigo 
ni agregarse a sí mismo como amigo, basándose en que dos clientes son iguales si 
tienen el mismo nombre). -}

{-

puedenSerAmigos:: Cliente->Cliente->Bool
puedenSerAmigos amigoNuevo cliente = ((not.elem (nombre amigoNuevo) (nombreDeAmigos cliente))&&((nombre amigoNuevo) /= (nombre cliente) )  

agregarAmigoAlCliente:: Cliente->Cliente->Cliente
agregarAmigoAlCliente amigoNuevo cliente
	| (puedenSerAmigos amigoNuevo cliente) = Cliente (edad cliente) (nombre cliente) (resistencia cliente) (amigoNuevo:(amigos cliente))
	| otherwise = cliente --no tira error, devuelve al cliente igual
-}

agregarAListaAmigos::Cliente->Cliente->Cliente
agregarAListaAmigos amigoNuevo cliente = Cliente (edad cliente) (nombre cliente) (resistencia cliente) (amigoNuevo:(amigos cliente))

nombreDeAmigos :: Cliente->[String]
nombreDeAmigos cliente = (map nombre (amigos cliente))  

esAmigoDe::Cliente->Cliente->Bool
esAmigoDe amigoNuevo cliente = elem (nombre amigoNuevo) (nombreDeAmigos cliente)

esLaMismaPersona::Cliente->Cliente->Bool
esLaMismaPersona cliente amigoNuevo = nombre amigoNuevo == nombre cliente

puedenSerAmigos::Cliente->Cliente->Bool
puedenSerAmigos cliente amigoNuevo = ((not.esLaMismaPersona amigoNuevo) cliente) && (not.esAmigoDe amigoNuevo) cliente

agregarAmigoAlCliente:: Cliente->Cliente->Cliente
agregarAmigoAlCliente amigoNuevo cliente 
     | puedenSerAmigos cliente amigoNuevo = agregarAListaAmigos amigoNuevo cliente
     | otherwise = cliente



{-Punto 5 
Representar con la abstracción que crea conveniente  
● a cada una de las bebidas mencionadas  
● y cómo queda un cliente luego de tomar cualquiera de las bebidas 
mencionadas.-}
type Bebida = Cliente->Cliente  

modificarResistencia:: Int->Cliente->Cliente
modificarResistencia cantidad (Cliente edad nombre resistencia amigos) = Cliente edad nombre (resistencia+cantidad) amigos

grogXD::Bebida
grogXD (Cliente edad nombre resistencia amigos) = Cliente edad nombre 0 amigos

listaDeResistencias::[Cliente]->[Int]
listaDeResistencias amigos = map resistencia amigos

menos10ResistenciaAmigos::Cliente->Cliente
menos10ResistenciaAmigos (Cliente edad nombre resistencia amigos) = Cliente edad nombre resistencia (map (modificarResistencia (-10)) amigos) 

jarraLoca::Bebida
jarraLoca cliente = (modificarResistencia (-10).menos10ResistenciaAmigos) cliente

klusener:: String->Bebida
klusener gusto cliente = modificarResistencia (-length gusto) cliente 

tintico :: Bebida
tintico cliente = modificarResistencia ( ( (5*).length.amigos) cliente) cliente

erupto:: Int->String
erupto fuerza = "e"++(replicate fuerza 'r')++"p"

soda:: Int->Bebida
soda fuerza (Cliente edad nombre resistencia amigos) = (Cliente edad ((erupto fuerza)++nombre) resistencia amigos)

rescatarse :: Int->Bebida
rescatarse horas cliente 
     | horas>3 = modificarResistencia 200 cliente
     | horas>0 = modificarResistencia 100 cliente
     | otherwise = cliente

{-Punto 6 
Hacer que un cliente pueda rescatarse.-}

{-Punto 7 
Escribir la ​ consulta en la consola ​  que permita realizar el siguiente itinerario con Ana: 
tomarse una jarra loca, un klusener de chocolate, rescatarse 2 horas y luego tomar 
un klusener de huevo.-}

-- ((klusener "huevo").(rescatarse 2).(klusener "chocolate").(jarraLoca)) ana

{-Casos de prueba 

Punto 3 Bien

Punto 4 
● Intentar agregar a Rodri como amigo de Rodri. ¿Qué debe pasar? 
● Hacer que Marcos reconozca a Rodri como amigo (que ya lo conoce). ¿Qué 
debe pasar? 
● Hacer que Rodri reconozca a Marcos como amigo. Debe arrancar con 0 
amigos y luego agregarlo a Rodri como único amigo. 
 
Punto 5 Bien
 
Punto 6 Bien

Punto 7 Bien -}
