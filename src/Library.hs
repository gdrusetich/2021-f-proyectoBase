module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero
<<<<<<< HEAD
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
=======
type Hierba = Raton -> Raton
type Medicamento = [Hierba]

type Comunidad = [Raton]

data Raton = UnRaton {
    nombre::String
,    edad:: Number
,    peso:: Number
,    enfermedades:: [String]

} deriving (Show,Eq)

cerebro = UnRaton {
    nombre = "Cerebro"
,   edad=9
,   peso=0.2
,   enfermedades=["breucelosis","sarampion", "tuberculosis"]
}

orejita = UnRaton {
    nombre = "Orejita"
,   edad=24
,   peso=0.325
,   enfermedades =["Estetococos", "Condetodo", "Esterlaisis", "Pulmonia","Covid","Malaria", "Anemia","Uretritis","Ceguera"]
}

bicenterrata = UnRaton {
    nombre = "Bicenterrata"
,   edad=256
,   peso=0.2
,   enfermedades=[]
}

huesudo = UnRaton {
    nombre = "Huesudo"
,   edad=4
,   peso=10
,   enfermedades = ["alta obesidad","sinusitis"]
}
modificarPeso :: Number -> Raton -> Raton
modificarPeso otroPeso raton = raton {peso = otroPeso}
--Hierba Buena
hierbaBuena :: Hierba
hierbaBuena raton = raton { edad = sqrt (edad raton)}

--Hierba Verde
hierbaVerde :: String -> Hierba
hierbaVerde terminacion raton = raton {enfermedades = eliminarEnfermedadHV terminacion (enfermedades raton)}

eliminarEnfermedadHV :: String -> [String]->[String]
eliminarEnfermedadHV terminacion lista = filter (coincidirEnfermedad terminacion) lista

coincidirEnfermedad :: String -> String -> Bool
coincidirEnfermedad terminacion enfermedad = take (length terminacion) (reverse enfermedad) /= reverse terminacion

--Alcachofa

alcachofa :: Hierba
alcachofa raton |peso raton > 2 = modificarPeso (peso raton*0.9) raton
                |otherwise = modificarPeso (peso raton*0.95) raton

--Hierba Zort
hierbaZort :: Hierba
hierbaZort raton = raton {
    nombre = "Pinky"
,   edad = 0
,   peso = peso raton
,   enfermedades = []
}

--Hierba Diablo

hierbaDiablo :: Hierba
hierbaDiablo = bajarPesoDelRaton.eliminarEnfermedadHD

bajarPesoDelRaton :: Hierba
bajarPesoDelRaton raton | (peso raton > 0.1) = modificarPeso (peso raton - 0.1) raton
                        |otherwise = modificarPeso 0 raton
eliminarEnfermedadHD :: Hierba
eliminarEnfermedadHD raton = raton {enfermedades = filter esEnfermedadLarga (enfermedades raton)}

esEnfermedadLarga :: String -> Bool
esEnfermedadLarga enfermedad = length enfermedad > 10

--Medicamentos
administrarMedicamento :: Medicamento -> Raton -> Raton
administrarMedicamento medicamento raton  = foldl (flip tomarHierba) raton medicamento

tomarHierba :: Hierba -> Raton -> Raton
tomarHierba hierba raton = hierba raton

--Ponds Anti Age
pondsAntiAge :: Medicamento
pondsAntiAge = [hierbaBuena, hierbaBuena, hierbaBuena, alcachofa]

--Reduce Fat Fast
{-
Hacer el reduceFatFast, (que viene en distintas potencias) y es un medicamento compuesto por una hierbaVerde de “obesidad” y tantas alcachofas como indique su potencia.
Por ejemplo administrándole a Huesudo un reduceFatFast de potencia 1 hace que huesudo pase a pesar 9 kg y sólo quede con sinusitis. Si en lugar de la 1 le administramos un reduceFatFast de potencia 2, pasa a pesar 8.1 kg y queda también solo con sinusitis.

reduceFatFast :: Number -> Medicamento -> Medicamento
reduceFatFast potencia medicamento = (agregarAlcachofa medicamento)^3

agregarAlcachofa :: Medicamento -> Medicamento
agregarAlcachofa medicamento = alcachofa : medicamento
-}

--PdeP Cilina
pdepCilina :: Raton -> Raton
pdepCilina raton = eliminarEnfermedadPdepCilina sufijosInfecciosas raton

eliminarEnfermedadPdepCilina :: [String]-> Raton -> Raton
eliminarEnfermedadPdepCilina sufijosInfecciosas raton = foldl (flip hierbaVerde) raton sufijosInfecciosas

sufijosInfecciosas :: [String]
sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

--Experimento


{-
Experimento: Los laboratorios antes de publicar un medicamento, lo prueban con distintos ratones para evaluar los resultados:
Hacer la función que encuentra la cantidadIdeal. Recibe una condición y dice cuál es el primer número natural que la cumple.
> cantidadIdeal even           > cantidadIdeal (>5)
2                              6
Saber si un medicamento lograEstabilizar una comunidad de ratones. Esto sucede cuando, luego de aplicarle el medicamento a todos los ratones de la comunidad, se elimina el sobrepeso y todos tienen menos de 3 enfermedades. Un ratón tiene sobrepeso si pesa más de 1kg.
Diseñar el siguiente experimento: dado una comunidad de ratones, encontrar la potencia ideal del reduceFatFast necesaria para estabilizar la comunidad.

-}


lograEstabilizar :: Medicamento -> Bool
lograEstabilizar medicamento = foldl (&&) True (lograEstabilizar' medicamento) 
lograEstabilizar' :: Medicamento -> [Bool] 
lograEstabilizar' medicamento = map (ratonAprobado medicamento) comunidad

comunidad :: [Raton]
comunidad = [cerebro, bicenterrata, huesudo]

tienePocasEnfermedades :: Raton -> Bool
tienePocasEnfermedades raton = length (enfermedades raton) < 3

tieneSobrepeso:: Raton -> Bool
tieneSobrepeso raton = peso raton > 1

ratonAprobado :: Medicamento -> Raton -> Bool
ratonAprobado medicamento raton = tienePocasEnfermedades (administrarMedicamento medicamento raton) && tieneSobrepeso (administrarMedicamento medicamento raton)
>>>>>>> 1410b7e92e0bc1866c1ebc401567ef0193ea04dc
