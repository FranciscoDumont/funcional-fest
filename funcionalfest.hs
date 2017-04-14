{-Punto 1 
Modelar el tipo de dato cliente. Justificar el criterio utilizado. -}
data Cliente = Cliente {
	edad :: Int,
	nombre :: String,
	resistencia :: Int,
	amigos :: [Cliente]
} deriving Show
	
{-Punto 2 
● Modelar a Rodri, que tiene 55 de resistencia y no considera a nadie como 
amigo.  
● También modelar a Marcos, un cliente que tiene resistencia 40 y considera a 
Rodri como su único amigo. 
● Tenemos a Cristian, un cliente cuya resistencia es 2, y no considera a nadie 
como amigo. 
● Por último está Ana, una cliente que tiene 120 de resistencia y considera a 
Marcos y a Rodri como amigos. -}

rodri = Cliente {
	edad= 20,
	amigos=[],
	nombre = "Rodri",
	resistencia = 55
} 
	
marcos = Cliente {
	edad=30,
	amigos = [rodri],
	resistencia =40,
	nombre = "Marcos"
} 

cristian = Cliente {
	edad=40,
	amigos=[],
	resistencia= 2,
	nombre="Cristian"
} 

ana = Cliente {
	edad=35,
	amigos=[rodri,marcos],
	resistencia=120,
	nombre="Ana"
} 

{-Punto 3 
Desarrollar la función ​ comoEsta​  que dice cómo está un cliente 
● Si su resistencia es mayor a 50, está “fresco” 
● Si no está “fresco”, pero tiene más de un amigo, está “piola” 
● En caso contrario, está “duro”-}

comoEsta :: Cliente->String
comoEsta cliente
	|((>50).resistencia) cliente = "fresco"
	|((>1).length.amigos) cliente ="piola"
	| otherwise ="duro"
	
{-Punto 4   
Hacer que un cliente reconozca como amigo a otro cliente, respetando las 
restricciones definidas por el negocio (no agregar más de una vez al mismo amigo 
ni agregarse a sí mismo como amigo, basándose en que dos clientes son iguales si 
tienen el mismo nombre). -}

nombreDeAmigos :: Cliente->[String]
nombreDeAmigos cliente = (map nombre (amigos cliente))  

puedenSerAmigos:: Cliente->Cliente->Bool
puedenSerAmigos amigoNuevo cliente = ((elem (nombre amigoNuevo) (nombreDeAmigos cliente)) == False)&&((nombre amigoNuevo) /= (nombre cliente) )  

agregarAmigoAlCliente:: Cliente->Cliente->Cliente
agregarAmigoAlCliente amigoNuevo cliente
	| (puedenSerAmigos amigoNuevo cliente) = Cliente (edad cliente) (nombre cliente) (resistencia cliente) (amigoNuevo:(amigos cliente))
	| otherwise = cliente --no tira error, devuelve al cliente igual

{-Punto 5 
Representar con la abstracción que crea conveniente  
● a cada una de las bebidas mencionadas  
● y cómo queda un cliente luego de tomar cualquiera de las bebidas 
mencionadas.-}
type Bebida = Cliente->Cliente  

grogXD::Bebida
grogXD (Cliente edad nombre resistencia amigos) = Cliente edad nombre 0 amigos

listaDeResistencias::[Cliente]->[Int]
listaDeResistencias amigos = map resistencia amigos

menos10Resistencia :: Cliente->Cliente
menos10Resistencia (Cliente edad nombre resistencia amigos) = Cliente edad nombre (resistencia-10) amigos

jarraLoca::Bebida
jarraLoca (Cliente edad nombre resistencia amigos) = Cliente edad nombre (resistencia-10) (map menos10Resistencia amigos) 

klusener:: String->Bebida
klusener gusto (Cliente edad nombre resistencia amigos) = Cliente edad nombre (resistencia-(length gusto)) amigos 

tintico :: Bebida
tintico (Cliente edad nombre resistencia amigos) = Cliente edad nombre (resistencia+5*(length amigos)) amigos

erupto:: Int->String
erupto fuerza = "e"++(concat (replicate fuerza "r"))++"p"

soda:: Int->Bebida
soda fuerza (Cliente edad nombre resistencia amigos) = (Cliente edad ((erupto fuerza)++nombre) resistencia amigos)

rescatarse :: Int->Bebida
rescatarse horas (Cliente edad nombre resistencia amigos) 
    | horas>3 = Cliente edad nombre (resistencia+200) amigos
	| horas>0 = Cliente edad nombre (resistencia+100) amigos
	| otherwise = (Cliente edad nombre resistencia amigos)

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
