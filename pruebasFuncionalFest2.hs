import Test.Hspec
import Control.Exception
import FuncionalFest

main = hspec $ do
	describe "Punto 1" $ do
		it "Marcos toma una soda de nivel 3 y queda con 2 bebidas" $do
			(length.bebidas) (soda 3 marcos) `shouldBe` 2
		it "Marcos toma una soda de nivel 3 y queda con 40 de resistencia" $do
			resistencia (soda 3 marcos) `shouldBe` 40

		it "Rodri toma una soda de nivel 1 y una soda de nivel 2 y queda con nombre errperpRodri" $do
			(nombre.(soda 2).(soda 1)) rodri `shouldBe` "errperpRodri"
		
		it "Marcos toma un klusener de huevo, un tintico y una jarraLoca y queda con 30 de resistencia " $do
			(resistencia.jarraLoca.tintico.(klusener "huevo")) marcos `shouldBe` 30

		it "Marcos toma un klusener de huevo, un tintico y una jarraLoca y queda con 4 bebidas en el historial" $do
			(length.bebidas.jarraLoca.tintico.(klusener "huevo")) marcos `shouldBe` 4 

		it "Ana pide “dame otro” y debe dar error" $do
			evaluate (dameOtro ana) `shouldThrow` errorCall "Prelude.last: empty list"

		it "Marcos pide “dame otro” y tiene 2 bebidas en el historial" $do
			(length.bebidas.dameOtro) marcos `shouldBe` 2  

		it "Marcos pide “dame otro” y lo deja con 34 de resistencia" $do
			(resistencia.dameOtro) marcos `shouldBe` 34 

		it "Rodri toma una soda de nivel 1, y “dame otro” da como resultado que tiene 3 bebidas" $ do
			(length.bebidas.dameOtro.soda 1) rodri `shouldBe` 3

		it "Rodri toma una soda de nivel 1, y dameOtro da como resultado que su nombre queda “erperpRodri”" $do
			(nombre.dameOtro.soda 1) rodri `shouldBe` "erperpRodri"
	
	describe "Punto 2" $do
		it "Rodri puede tomar dos bebidas, entre un grog XD, un tintico y un klusener de frutilla" $do
			cuantasPuedeTomar rodri [grogXD,tintico,klusener "frutilla"] `shouldBe` 2

		it "Rodri puede tomar dos bebidas, entre un grog XD, un tintico y un klusener de frutilla" $do
			cuantasPuedeTomar rodri [grogXD,tintico,klusener "fruuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuutilla"] `shouldBe` 1

	describe "Punto 3" $do
		it "Rodri hace una salida de amigos y debe quedar con un amigo" $do
			(length.amigos) (hacerItinerario rodri salidaDeAmigos) `shouldBe` 1 

		it "Rodri hace una salida de amigos y se debe llamar “erpRodri” " $do
			nombre (hacerItinerario rodri salidaDeAmigos) `shouldBe` "erpRodri"

		it "Rodri hace una salida de amigos y debe quedar con 45 de resistencia" $do
			resistencia (hacerItinerario rodri salidaDeAmigos) `shouldBe` 45

		it "Rodri hace una salida de amigos y su primer y único amigo Roberto Carlos debe quedar con 155 de resistencia" $do
			(resistencia.head.amigos) (hacerItinerario rodri salidaDeAmigos) `shouldBe` 155

		it "Rodri hace una salida de amigos y debe quedar con 5 bebidas en su historial" $do
			(length.bebidas) (hacerItinerario rodri salidaDeAmigos) `shouldBe` 5

	describe "Punto 4" $do
		it "la intensidad de la mezcla explosiva es 2.0" $do
			intensidad mezclaExplosiva `shouldBe` 2.0         --mezcla explosiva tiene 4 bebidas y el tiempo es de 2.5

		it "la intensidad de la salidaDeAmigos es 4.0" $do
			intensidad salidaDeAmigos `shouldBe` 4.0

		it "la intensidad del itinerario basico es 0.8" $do
			intensidad itinerarioBasico `shouldBe` 0.8
		--listadeitinerarios= [mezclaExplosiva, itinerarioBasico, salidaDeAmigos]
		it "Entre la salida de amigos, la mezcla explosiva y el itinerario básico, el itinerario más intenso es la salida de amigos (tip: se puede reconocer por el nombre)" $do
			(nombreitinerario.maximumItinerario) listadeitinerarios `shouldBe` "Salida de Amigos"

		it "Rodri hace el itinerario más intenso entre una salida de amigos, la mezcla explosiva y el itinerario básico y queda con el nombre erpRodri" $do
			nombre (hacerItinerarioIntenso rodri listadeitinerarios) `shouldBe` "erpRodri"

		it "Rodri hace el itinerario más intenso entre una salida de amigos, la mezcla explosiva y el itinerario básico y queda con resistencia 45" $do
			resistencia (hacerItinerarioIntenso rodri listadeitinerarios) `shouldBe` 45

		it "Rodri hace el itinerario más intenso entre una salida de amigos, la mezcla explosiva y el itinerario básico y queda con un amigo: Roberto Carlos" $do
			(length.amigos) (hacerItinerarioIntenso rodri listadeitinerarios) `shouldBe` 1
			(nombre.head.amigos) (hacerItinerarioIntenso rodri listadeitinerarios) `shouldBe` "Roberto Carlos"

	describe "Punto 6" $do

		it "Roberto Carlos se hace amigo de Ana, toma una jarra popular de espirituosidad 0, sigue quedando con una sola amiga (Ana)" $do
			pendingWith "Punto 6 no es obligatorio"

		it "Roberto Carlos se hace amigo de Ana, toma una jarra popular de espirituosidad 3, queda con 3 amigos (Ana, Marcos y Rodri) " $do
			pendingWith "Punto 6 no es obligatorio"

		it "Cristian se hace amigo de Ana. Roberto Carlos se hace amigo de Cristian, toma una jarra popular de espirituosidad 4, queda con 4 amigos (Cristian, Ana, Marcos y Rodri)" $do
			pendingWith "Punto 6 no es obligatorio"
