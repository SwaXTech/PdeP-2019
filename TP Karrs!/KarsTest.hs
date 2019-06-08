import Kars
import Test.Hspec

main :: IO ()
main = hspec $ do

    describe "Kars" $ do

        describe "Entrega 2" $ do

            describe "Punto 3.0 - deReversa no tomará en cuenta la longitud de la pista, si no que aumentará su nafta en una quinta parte de la velocidad" $ do

                describe "RochaMcQueen" $ do

                    it "Nafta luego de hacer su truco favorito \"deReversa\" = 300" $ do
                        (nivelNafta . truco rochaMcQueen) rochaMcQueen `shouldBe` 300

                describe "Rodra" $ do

                    it "Nafta luego de hacer de hacer \"deReversa\" = 10" $ do
                        (nivelNafta . deReversa) rodra `shouldBe` 10
                
            describe "Punto 3.2 - Modelado de trampas" $ do
                
                describe "sacarAlPistero - el primer participante queda fuera de la competencia." $ do
                    
                    it "cantidad de participantes luego de sacarAlPistero en potreroFunes = 3" $ do
                        (length.sacarAlPistero.participantes) potreroFunes `shouldBe` 3

                    -- Comparo por el nombre ya que no puedo hacer deriving Eq porque el Auto tiene una funcion
                    it "rochaMcQueen ya no participa en potreroFunes tras sacarAlPistero" $ do
                        not (elem (nombre rochaMcQueen) (map nombre (sacarAlPistero (participantes potreroFunes)))) `shouldBe` True

                describe "pocaReserva -  los participantes con menos de 30 litros de nafta quedan fuera." $ do
            
                    it "cantidad de participantes luego de pocaReserva en potreroFunes = 3" $ do
                        (length.pocaReserva.participantes) potreroFunes `shouldBe` 3

                    -- Comparo por el nombre ya que no puedo hacer deriving Eq porque el Auto tiene una funcion
                    it "rodra ya no participa en potreroFunes tras usar pocaReserva" $ do
                        not (elem (nombre rodra) (map nombre (pocaReserva (participantes potreroFunes)))) `shouldBe` True
                
                describe "podio - solamente deja seguir compitiendo a los primeros 3 participantes de la carrera." $ do
    
                    it "cantidad de participantes luego de podio en potreroFunes = 3" $ do
                        (length.podio.participantes) potreroFunes `shouldBe` 3

                describe "lluvia - baja 10 km/h de velocidad a todos los participantes" $ do

                    it "velocidad del último participante de potreroFunes (rodra) tras usar lluvia = 40" $ do
                        (velocidad.last.lluvia.participantes) potreroFunes `shouldBe` 40

            describe "Punto 3.3 - A correr" $ do

                describe "darVuelta" $ do

                    it "nafta del primer participante (biankerr) luego de dar una vuelta en potreroFunes = 490" $ do
                        (nivelNafta.head.participantes.darVuelta) potreroFunes `shouldBe` 490

                    it "velocidad del primer participante (biankerr) luego de dar una vuelta en potreroFunes = 40" $ do
                        (velocidad.head.participantes.darVuelta) potreroFunes `shouldBe` 40

                    it "cantidad de participantes luego de dar 2 vueltas en potreroFunes = 2" $ do
                        (length.participantes.darVuelta.darVuelta) potreroFunes `shouldBe` 2

                    it "luego de dar 2 vueltas en potreroFunes, la nafta del primer participante (gushtav) = 70" $ do
                        (nivelNafta.head.participantes.darVuelta.darVuelta) potreroFunes `shouldBe` 70

                    it "rodra debe ser el unico participante luego de correr la carrera de potreroFunes" $ do
                       ((== 1).length.participantes.correrCarrera) potreroFunes &&  elem (nombre rodra) (map nombre ((participantes.correrCarrera) potreroFunes)) `shouldBe` True

            describe "Punto 3.4 - Quien gana" $ do

                describe "quienGana" $ do

                    -- utilizo el nombre porque para usar shouldBe necesita ser Eq
                    it "el ganador de potreroFunes es rodra" $ do
                        (nombre.quienGana) potreroFunes `shouldBe` "Rodra"

            describe "Punto 3.5 - elGranTruco" $ do

                describe "elGranTruco - recibe una lista de trucos y aplica todos a un auto" $ do

                    it "velocidad de rodra tras realizar elGranTruco con nitro, deReversa e impresionar = 130" $ do
                        (velocidad.elGranTruco [nitro, deReversa, impresionar]) rodra `shouldBe` 130

                    it "nafta de rodra tras realizar elGranTruco con nitro, deReversa e impresionar = 13" $ do
                        (nivelNafta.elGranTruco [nitro, deReversa, impresionar]) rodra `shouldBe` 13