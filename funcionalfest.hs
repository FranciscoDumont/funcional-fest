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

agregarBebidaACliente::Bebida->Cliente->Cliente
agregarBebidaACliente bebida cliente =cliente{bebidas= bebida:(bebidas cliente)} 

tomarBebida::Cliente->Bebida->Cliente
tomarBebida cliente bebida= bebida (cliente)

tomarTragos::Cliente->[Bebida]->Cliente
tomarTragos cliente tragos = foldl tomarBebida cliente tragos

dameOtro::Cliente->Cliente
dameOtro cliente = (last.bebidas) cliente $ cliente

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


type Bebida = Cliente->Cliente

modificarResistencia:: Int->Cliente->Cliente
modificarResistencia cantidad (Cliente edad nombre resistencia amigos bebidas) = Cliente edad nombre (resistencia+cantidad) amigos bebidas

grogXD::Bebida
grogXD cliente = cliente{resistencia = 0}

listaDeResistencias::[Cliente]->[Int]
listaDeResistencias amigos = map resistencia amigos

menos10ResistenciaAmigos::Cliente->Cliente
menos10ResistenciaAmigos (Cliente edad nombre resistencia amigos bebidas) = Cliente edad nombre resistencia (map (modificarResistencia (-10)) amigos) bebidas 

jarraLoca::Bebida
jarraLoca cliente = (modificarResistencia (-10).menos10ResistenciaAmigos) cliente

klusener:: String->Bebida
klusener gusto cliente = modificarResistencia (-length gusto) cliente 

tintico :: Bebida
tintico cliente = modificarResistencia ( ( (5*).length.amigos) cliente) cliente

erupto:: Int->String
erupto fuerza = "e"++(replicate fuerza 'r')++"p"

soda:: Int->Bebida
soda fuerza (Cliente edad nombre resistencia amigos bebidas) = (Cliente edad ((erupto fuerza)++nombre) resistencia amigos bebidas)

rescatarse :: Int->Bebida
rescatarse horas cliente 
     | horas>3 = modificarResistencia 200 cliente
     | horas>0 = modificarResistencia 100 cliente
     | otherwise = cliente
