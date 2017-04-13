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

--agregarAmigoAlCliente :: Cliente->Cliente->Cliente
--agregarAmigoAlCliente nuevo actual =((: nuevo). amigos )actual -- se puede hacer point free

--v2
agregarAmigoAlCliente:: Cliente->Cliente->Cliente
agregarAmigoAlCliente amigoNuevo (Cliente edad nombre resistencia amigos) =	Cliente edad nombre resistencia (amigoNuevo:amigos)

--v3
--agregarAmigoAlCliente:: Cliente->Cliente->Cliente
--agregarAmigoAlCliente amigoNuevo cliente =	cliente { amigos = (amigoNuevo:amigos) }