module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero
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
agregarAlcachofa medicamento = alcachofa:medicamento
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
