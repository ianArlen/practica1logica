--1 Ejercicio
--Variables representadas como enteros
type VarP = Int

--2 Ejercicio
--Definicion de valuaciones como listas
type Lista = [VarP]


--3 Ejercicio
--Implementa una funcion que reciba una formula y regrese el conjunto o lista, de
--las variables usadas en la formula.




--conjForm (Neg (Var "p"))

--4 Ejercicio
--Una funcion que devuelva el numero de apariciones de conjunciones en la
--formula

type Indice = Int

data PL = Top | Bot  
			  | Var Indice
              | Neg PL 
              | And PL PL 
              | Or PL PL 
              | Imp PL PL deriving (Eq, Show)



numConj :: PL -> Int
numConj phi = case phi of
                Top -> 0
                Bot -> 0
                Var _ -> 0
                Neg alpha -> numConj alpha
                And alpha beta -> 1 + numConj alpha + numConj beta
                Or alpha beta -> numConj alpha + numConj beta
                Imp alpha beta -> numConj alpha + numConj beta


--Main>numConj Or (And (Var 1) Neg $ Var 2) (And Bot (Var 3))


--5 Ejercicio
--1. Implementa el algoritmo quitaImp, que recibe una formula de Logica
--Proposicional y regresa una formula de Logica proposicional sin apari-
--ciones del operador implicacion.

quitaImp :: PL -> PL
quitaImp phi = case phi of
                 Top -> Top
                 Bot -> Bot
                 Var x -> Var x
                 Neg x -> Neg (quitaImp x)
                 And x y -> And (quitaImp x) (quitaImp y)
                 Or x y -> Or (quitaImp x) (quitaImp y)
                 Imp x y -> Or (quitaImp (Neg  x)) (quitaImp y)


--Main>quitaImp (Imp (Var 1) (Var 2))


--6 Ejercicio
--1. Implementa el algoritmo lNand que recibe una formula de L ́ogica proposi-
--cional y regresa una formula en donde solo aparece el operador nand




--7 Ejercicio
--Define la funci ́on mSatisface :: Modelo-> PL -> Bool, que reciba un mod-
--elo y una formula de PL y regrese si el modelo satisface o no a la formula.

type Modelo = [Indice]
type Valuacion = Indice -> Bool

satMod :: Modelo -> PL -> Bool
satMod m phi = case phi of
                 Top -> True
                 Bot -> False
                 Var n -> elem n m
                 Neg alpha -> not (satMod m alpha)
                 And alpha beta -> (satMod m alpha) && (satMod m beta)
                 Or alpha beta -> (satMod m alpha) || (satMod m beta)
                 Imp alpha beta -> not (satMod m alpha) || (satMod m beta)

--Main>mSatisface [] (Imp (Var 1) (Var 2))

