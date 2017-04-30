import Test.Hspec
import Control.Exception
import FuncionalFest

main = hspec $ do

	describe "Punto 3" $ do
		it "Cristian debe estar duro" $do
			comoEsta cristian `shouldBe` "duro"
		it "Rodri debe estar fresco" $do
			comoEsta rodri `shouldBe` "fresco"
		it "Marcos debe estar duro" $do
			comoEsta marcos `shouldBe` "duro"
		it "Marcos se hace amigo de Ana y rodri entonces esta piola" $do
			(comoEsta.(agregarAmigoAlCliente rodri).(agregarAmigoAlCliente ana)) marcos `shouldBe` "piola"
	 
	describe "Punto 5" $ do
		it "Ana toma GrogXD. Queda con resistencia 0" $do
			(resistencia.grogXD) ana `shouldBe` 0

		it "Ana toma la Jarra Loca Queda con resistencia 110,marcos con 30 y rodri con 45" $do
			(resistencia.jarraLoca) ana `shouldBe` 110
			(resistencia.last.amigos.jarraLoca) ana `shouldBe` 30 --marcos
			(resistencia.head.amigos.jarraLoca) ana `shouldBe` 45 --rodri

		it "Ana toma un Klusener de huevo, queda con 115 de resistencia" $do
			resistencia (klusener "huevo" ana) `shouldBe` 115
		
		it "Ana toma un Klusener de chocolate, queda con 111 de resistencia" $do
			resistencia (klusener "chocolate" ana) `shouldBe` 111

		it "Cristian toma un Tintico, queda con 2 de resistencia por no tener amigos" $do
			(resistencia.tintico) cristian `shouldBe` 2

		it "Ana toma un Tintico, pasa de 120 a 130 de resistencia ,tiene 2 amigos" $do
			(resistencia.tintico) ana `shouldBe` 130

		it "Rodri toma una Soda de fuerza 2, queda con nombre errpRodri" $do
			nombre (soda 2 rodri) `shouldBe` "errpRodri"

		it "Ana toma una Soda de fuerza 10, queda con nombre errrrrrrrrrpAna" $do
			nombre (soda 10 ana) `shouldBe` "errrrrrrrrrpAna" 

		it "Ana toma una Soda de fuerza 0, queda con nombre epAna " $do
			nombre (soda 0 ana) `shouldBe` "epAna"

	describe "Punto 6" $ do
		it "Rodri se rescata 5 horas, queda con 255 de resistencia" $do
			resistencia (rescatarse 5 rodri) `shouldBe` 255

		it "Rodri se rescata 1 hora, queda con 155 de resistencia" $do
			resistencia (rescatarse 1 rodri) `shouldBe` 155

	describe "Punto 7" $ do
		it "Luego del itinerario de Ana, queda con 196 de resistencia, Marcos con 30 y Rodri con 45" $do
			let itinerario = (klusener "huevo").(rescatarse 2).(klusener "chocolate").(jarraLoca) 
			resistencia (itinerario ana) `shouldBe` 196
			(resistencia.head.amigos) (itinerario ana) `shouldBe` 45 --rodri
	  		(resistencia.last.amigos) (itinerario ana) `shouldBe` 30 -- marcos