import Text.Show.Functions
{-Punto 1 
De los clientes, además del nombre, resistencia y sus amigos, se desea saber qué 
bebidas tomó.
a) Hacer las modificaciones en la abstracción cliente y considerar 
i)Que Rodri tomó un tintico 
ii)Que Marcos tomó un Klusener de guinda 
iii)Que Ana no tomó nada 
iv)Que Cristian tomó un grog XD y una jarraLoca 

b) Hacer que un cliente pueda tomar una bebida. Además del efecto que le 
causa la bebida en sí, se debe registrar esa bebida en su historial de bebidas 
tomadas.  
 
c) Desarrollar la función ​ tomarTragos​ , la cual recibe a un cliente y una lista de 
tragos y retorna al cliente luego de tomarlos todos los tragos.  
 
d) Hacer la función ​ dameOtro​ , que hace que un cliente vuelva a tomarse el 
último trago que se tomó. -}

data Cliente = Cliente {edad::Int, nombre::String, resistencia::Int, amigos::[Cliente], bebidas::[Bebida]} deriving Show

rodri = Cliente {edad= 20,amigos=[],nombre = "Rodri",resistencia = 55,bebidas=[tintico] }
marcos = Cliente {edad=30,amigos = [rodri],resistencia =40,nombre = "Marcos",bebidas=[klusener "guinda"] } 
cristian = Cliente {edad=40,amigos=[],resistencia= 2,nombre="Cristian",bebidas=[grogXD,jarraLoca]}
ana = Cliente {edad=35,amigos=[rodri,marcos],resistencia=120,nombre="Ana",bebidas=[]}

comoEsta :: Cliente->String
comoEsta cliente 
     | ((>50).resistencia) cliente = "fresco"
     | ((>1).length.amigos) cliente ="piola"
     | otherwise ="duro"

agregarAListaAmigos::Cliente->Cliente->Cliente
agregarAListaAmigos amigoNuevo cliente = Cliente (edad cliente) (nombre cliente) (resistencia cliente) (amigoNuevo:(amigos cliente)) (bebidas cliente)

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


{-Punto 2 
a) Definir la función ​ cualesPuedeTomar​ , la cual recibe a un cliente y una lista de 
tragos y nos dice cuáles de esas bebidas lo dejarían con una resistencia 
mayor a cero, en caso de tomarlas solas. 
 
b) Definir la función ​ cuantasPuedeTomar​ , que devuelva la cantidad de bebidas 
en base al punto a). -}

puedeTomar::Cliente->Bebida->Bool
puedeTomar cliente bebida = (>0).resistencia.bebida $ cliente

cualesPuedeTomar :: Cliente->[Bebida]->[Bebida]
cualesPuedeTomar cliente tragos= filter (puedeTomar cliente) tragos

cuantasPuedeTomar :: Cliente->[Bebida]->Int
cuantasPuedeTomar cliente tragos = length.cualesPuedeTomar cliente$ tragos 

{-Punto 3 
Ahora, aparecen diferentes itinerarios que pueden realizar los clientes, de los cuales 
se registran su nombre, una duración estimada  y lo más importante, el detalle de las 
acciones que componen el itinerario. 
Algunos itinerarios son los siguientes: 
● Mezcla explosiva, se recomienda para 2.5 horas y consiste en  tomarse 2 
Grog XD, 1 Klusener de Huevo y otro de Frutilla. 
● Itinerario básico, es como el del punto 7 de la primera parte, en 5 horas. 
● Salida de amigos, se recomienda para 1 hora, consiste en tomarse una soda, 
de nivel 1, un tintico, hacerse amigo de Roberto Carlos (sí, es cliente de este 
boliche 1 ) y tomarse una jarra loca. 
 
a) Modelar los itinerarios existentes, para lo cual considerar que 
i)
Roberto Carlos no tiene amigos, tiene 165 de resistencia y no tomó 
nada. 
ii)
Deben utilizar como expresiones robertoCarlos, mezclaExplosiva, 
itinerarioBasico, salidaDeAmigos. 
b) Mostrar cómo rodri hace una salida de amigos y marcos una mezcla 
explosiva.  -}



--BEBIDAS

type Bebida = Cliente->Cliente

agregarBebidaACliente::Bebida->Cliente->Cliente
agregarBebidaACliente bebida cliente =cliente{ bebidas = bebidas cliente ++ [bebida]} 

tomarBebida::Cliente->Bebida->Cliente
tomarBebida cliente bebida= bebida cliente

tomarTragos::Cliente->[Bebida]->Cliente
tomarTragos cliente tragos = foldl tomarBebida cliente tragos

dameOtro::Cliente->Cliente
dameOtro cliente = (last.bebidas) cliente $ cliente

modificarResistencia:: Int->Cliente->Cliente
modificarResistencia cantidad cliente = cliente{ resistencia = cantidad+resistencia cliente}

grogXD::Bebida
grogXD cliente = agregarBebidaACliente grogXD cliente{resistencia = 0}

listaDeResistencias::[Cliente]->[Int]
listaDeResistencias amigos = map resistencia amigos

menos10ResistenciaAmigos::Cliente->Cliente
menos10ResistenciaAmigos cliente = cliente { amigos= map (modificarResistencia (-10)) (amigos cliente)} 

jarraLoca::Bebida
jarraLoca cliente = agregarBebidaACliente jarraLoca.(modificarResistencia (-10).menos10ResistenciaAmigos) $ cliente

klusener:: String->Bebida
klusener gusto cliente = (agregarBebidaACliente $ klusener gusto).(modificarResistencia (-length gusto)) $ cliente 

tintico :: Bebida
tintico cliente = modificarResistencia ((5*).length.amigos$cliente) cliente

erupto:: Int->String
erupto fuerza = "e"++(replicate fuerza 'r')++"p"

soda:: Int->Bebida
soda fuerza cliente = agregarBebidaACliente (soda fuerza) cliente{nombre= (erupto fuerza)++(nombre cliente)}

rescatarse :: Int->Bebida
rescatarse horas cliente 
     | horas>3 = modificarResistencia 200 cliente
     | horas>0 = modificarResistencia 100 cliente
     | otherwise = cliente
