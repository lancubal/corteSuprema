module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Ley = UnaLey {
    tema::String,
    presupuesto::Number,
    promotores::[String]
}

estaDentroDe [] _ = True
estaDentroDe _ [] = False
estaDentroDe ley1 ley2 | ley1 == take (length ley1) ley2 = True
                | otherwise = estaDentroDe ley1 (tail ley2)
algunTemaEstaContenido :: Ley -> Ley -> Bool
algunTemaEstaContenido ley1 ley2 = estaDentroDe (tema ley1) (tema ley2) || estaDentroDe (tema ley2) (tema ley1)

compatibles :: Ley -> Ley -> Bool
compatibles leya leyb = (length (intersectar (promotores leya) (promotores leyb)) > 0) && (algunTemaEstaContenido leya leyb)
 where intersectar listaB listaA  = filter (\x -> elem x listaA) listaB

agenda :: [[String]]
agenda = ["Tema_agenda 1","Tema_agenda 2"]

juez1 :: Ley -> Bool
juez1 ley = elem (tema ley) agenda

juez2 :: Ley -> Bool
juez2 ley = elem "Sector financiero" (promotores ley)

juez3 :: Ley -> Bool
juez3 ley = presupuesto ley <= 10

juez4 :: Ley -> Bool
juez4 ley = presupuesto ley <= 20

juez5 :: Ley -> Bool
juez5 ley = elem "Partido conservador" (promotores ley)

esConstitucional :: [(Ley->Bool)] -> Ley -> Bool
esConstitucional corte ley = length (filter (\x -> x ley) corte) > div (length corte) 2

juez6 :: Ley -> Bool
juez6 _ = True

suLeyFavorita = (UnaLey "Aumentar el sueldo de los jueces" 100 ["Partido conservador"])

juez7 :: Ley -> Bool
juez7 ley = compatibles ley suLeyFavorita

juez8 :: Ley -> Bool
juez8 ley = presupuesto ley <= 5

nuevasConstitucionales :: [(Ley ->Bool)] -> [(Ley ->Bool)] -> [Ley] -> [Ley]
nuevasConstitucionales corteactual cortenueva leyes = filter (esConstitucional (corteactual ++ cortenueva)) (filter (not(esConstitucional corteactual)) leyes)

borocotizar :: [(Ley ->Bool)] -> [(Ley ->Bool)]
borocotizar corte = map (\x -> not x) corte

leyesQueApoyaUnJuez:: (Ley ->Bool) -> [Ley] -> [Ley]
leyesQueApoyaUnJuez juez leyes = filter (\x -> juez x) leyes

leyesQueApoyaUnSector :: String  -> [Ley] -> [Ley]
leyesQueApoyaUnSector sector leyes = filter (\x ->elem sector (promotores x)) leyes

curioso :: (Ley ->Bool) ->[Ley] -> String -> Bool
curioso juez leyes sector = length (intersectar (leyesQueApoyaUnJuez juez leyes) (leyesQueApoyaUnSector sector leyes)) == length (leyesQueApoyaUnJuez juez leyes)
 where intersectar listaB listaA  = filter (\x -> elem x listaA) listaB