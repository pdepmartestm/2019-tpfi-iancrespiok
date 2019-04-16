type tesoro = (string, Int)
type pirata = (string, [(tesoro)], Int, esAfortunado)

esAfortunado (_, _, c, _) = c>10000
esAfortunado :: pirata -> Bool

(JackSparrow, [(brujula, 10000), (frascoArena, 0), 10000, True)
(DavidJones, [(cajitaMusical, 1)], 1, False)
(AnneBonny, [(doblones, 100), (frascoArena, 1)], 101, False)

tesorosPirata pirata  (_, [(t, s)], _, _) = [s]
tesorosPirata :: pirata -> [Int]

sumar [] = 0
sumar (x:xs) = x + sumar(xs)
sumar :: [Int] -> Int

totalBotin pirata = sumar.tesorosPirata 

saquear tesoro (_, [t], _, _) = (_, [t]++tesoro, _, _)
saquear :: tesoro -> pirata -> pirata
