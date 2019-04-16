 import Text.Show.Functions 

type Tesoro = (String, Integer)
type Pirata = (String, [(Tesoro)])

esAfortunado (nombre , tesoros) = sum (map valorTesoro tesoros) > 10000
esAfortunado :: Pirata -> Bool

valorTesoro (_, c) = c
valorTesoro :: Tesoro -> Integer

tesoros (_, listaTesoros) = listaTesoros
tesoros :: Pirata -> [Tesoro]

jack, david, anne :: Pirata
jack = ("JackSparrow", [("brujula", 10000), ("frascoArena", 0)])
david = ("DavidJones", [("cajitaMusical", 1)])
anne = ("AnneBonny", [("doblones", 100), ("frascoArena", 1)])

saquear tesoro pirata modoSaqueo 
  |modoSaqueo tesoro = agregarTesoro tesoro pirata
  |otherwise = pirata

agregarTesoro tesoro (nombre, tesoros) = (nombre,  tesoro : tesoros)

saqueoTesorosValiosos tesoro = valorTesoro tesoro > 10000 

saqueoTesorosEspecificos nombre (nTesoro, _) = nombre == nTesoro

saqueoConCorazon tesoro = False

saqueoComplejo :: [Tesoro -> Bool] -> Tesoro -> Bool
saqueoComplejo listaDeSaqueos tesoro = any (seCumplePara tesoro) listaDeSaqueos 
seCumplePara tesoro saqueo = saqueo tesoro 

perlaNegra = ("PerlaNegra", [jack, anne], saqueoTesorosValiosos)
holandesErrante = ("HolandesErrante", [david], saqueoConCorazon)

coincidenciaTesoros pirata1 pirata2 = any (loTiene pirata2) (tesoros pirata1)
loTiene pirata tesoro = any (mismoNombreYDistintoValor tesoro) (tesoros pirata)
mismoNombreYDistintoValor (n1, v1) (n2, v2) = n1 == n2 && v1 /= v2


