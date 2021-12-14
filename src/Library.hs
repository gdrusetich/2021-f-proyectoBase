module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero
type Calorias = Number
type Minutos = Number
type Kilos = Number
type Inclinacion = Number
type Ejercicio = Gimnasta -> Gimnasta
type Probando = Number

data Gimnasta = UnGimnasta {
    nombre::String
,   edad::Number
,   peso::Number
,   tonificacion::Number
    } 

data Rutina = UnaRutina {
    nombreRutina::String
,   duracion::Number
,   ejercicios::[Ejercicio]
} deriving (Show,Eq)
pancho = UnGimnasta "Francisco" 40 120 1
nati = UnGimnasta "Natalia" 35 102 3
andres = UnGimnasta "Andy" 20 80 6

hacerEjercicios:: Gimnasta -> Ejercicio -> Gimnasta
hacerEjercicios gimnasta ejercicio= ejercicio gimnasta

--Está Saludable
estaSaludable :: Gimnasta -> Bool
estaSaludable gimnasta = not (estaObeso gimnasta) && tonificacion gimnasta > 5 

estaObeso:: Gimnasta -> Bool
estaObeso gimnasta = not (peso gimnasta < 100)

--Quemar Calorias
quemarCalorias :: Gimnasta -> Calorias -> Gimnasta
quemarCalorias gimnasta calorias = gimnasta {
    peso = calcularCalorias gimnasta calorias
    }

calcularCalorias :: Gimnasta-> Calorias -> Number
calcularCalorias gimnasta calorias  | (estaObeso gimnasta)==True = peso gimnasta - (calorias/150)
calcularCalorias gimnasta calorias  | (edad gimnasta) > 30 && calorias > 200 = peso gimnasta - 1
                                    | otherwise = peso gimnasta - calorias/((peso gimnasta)*(edad gimnasta))


--Ejercicios
caloriasEnCinta :: Number -> Minutos -> Number
caloriasEnCinta velocidad minutos = velocidad*minutos

--Caminata En Cinta
caminataEnCinta :: Minutos -> Ejercicio
caminataEnCinta minutos gimnasta = quemarCalorias gimnasta (caloriasEnCinta 5 minutos) 

--Entrenamiento En Cinta
entrenamientoEnCinta :: Minutos -> Ejercicio
entrenamientoEnCinta minutos gimnasta = quemarCalorias gimnasta (((6 + 6 + (div minutos 5) )/2)*40)


--Pesas
pesas:: Kilos -> Minutos -> Ejercicio
pesas kilos minutos gimnasta | minutos > 10 = gimnasta {
    tonificacion = tonificacion gimnasta + (kilos * 0.1)
    }
                    |otherwise = gimnasta

--Colina
colina :: Inclinacion -> Minutos -> Ejercicio
colina inclinacion minutos gimnasta = quemarCalorias gimnasta (2*minutos*inclinacion)

montania :: Inclinacion -> Minutos -> Ejercicio
montania inclinacion minutos gimnasta = foldl (hacerEjercicios)  gimnasta [colina inclinacion (minutos/2),colina (inclinacion+3) (minutos/2),aumentarTonificacion 1]

aumentarTonificacion :: Number -> Ejercicio
aumentarTonificacion numero gimnasta = gimnasta {tonificacion = tonificacion gimnasta + numero}

--Rutinas
rutina1 = UnaRutina "Las del ejercicio 3" 40 [caminataEnCinta 10, entrenamientoEnCinta 5, pesas 10 15, colina 2 5, montania 2 5]
rutina2 = UnaRutina "Solo Cinta" 90 [caminataEnCinta 5, entrenamientoEnCinta 5, caminataEnCinta 5, entrenamientoEnCinta 10, caminataEnCinta 5, entrenamientoEnCinta 15, caminataEnCinta 5, entrenamientoEnCinta 20, caminataEnCinta 5, entrenamientoEnCinta 25]
rutina3 = UnaRutina "Solo Pesas" 90 [pesas 5 30 , pesas 10 25, pesas 15 20, pesas 10 15]
rutina4 = UnaRutina "Solo Escalinatas" 100 [colina 1 5, montania 2 5, colina 1 5, montania 2 10, colina 1 5, montania 2 15, colina 1 5, montania 2 20, colina 1 5, montania 3 10, colina 1 5, montania 3 15]


hacerRutina :: Rutina -> Gimnasta -> Gimnasta
hacerRutina rutina gimnasta = foldl (hacerEjercicios) gimnasta (ejercicios rutina)

--Resumen de Rutina
resumenDeRutina::Rutina -> Gimnasta ->  (String,Kilos,Number)
resumenDeRutina rutina gimnasta = (nombreRutina rutina ,peso gimnasta - peso (hacerRutina rutina gimnasta),tonificacion (hacerRutina rutina gimnasta)-tonificacion gimnasta)


--nombreDeRutinasUtiles :: [Rutina]->[Rutina]
listaDeRutinas :: [Rutina]
listaDeRutinas = [rutina1, rutina2, rutina3, rutina4]

rutinasUtiles :: [Rutina] -> Gimnasta -> [Rutina]
rutinasUtiles rutinas gimnasta = filter (flip esRutinaUtil gimnasta) rutinas



esRutinaUtil :: Rutina -> Gimnasta -> Bool
esRutinaUtil rutina gimnasta = estaSaludable (hacerRutina rutina gimnasta)
