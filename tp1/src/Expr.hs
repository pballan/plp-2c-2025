module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar,
  )
where

import qualified Data.Bifunctor
import Data.List.NonEmpty (cons)
import Generador
import Histograma

-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

-- recrExpr :: Esquema de recursión primitiva sobre expresiones
recrExpr ::
  (Float -> b) ->
  (Float -> Float -> b) ->
  (Expr -> Expr -> b -> b -> b) ->
  (Expr -> Expr -> b -> b -> b) ->
  (Expr -> Expr -> b -> b -> b) ->
  (Expr -> Expr -> b -> b -> b) ->
  Expr ->
  b
recrExpr cConst cRango cSuma cResta cMult cDiv e = case e of
  Const f -> cConst f
  Rango f1 f2 -> cRango f1 f2
  Suma e1 e2 -> cSuma e1 e2 (rec e1) (rec e2)
  Resta e1 e2 -> cResta e1 e2 (rec e1) (rec e2)
  Mult e1 e2 -> cMult e1 e2 (rec e1) (rec e2)
  Div e1 e2 -> cDiv e1 e2 (rec e1) (rec e2)
  where
    rec = recrExpr cConst cRango cSuma cResta cMult cDiv

-- foldExpr :: Esquema de recursión estructural sobre expresiones
foldExpr ::
  (Float -> b) ->
  (Float -> Float -> b) ->
  (b -> b -> b) ->
  (b -> b -> b) ->
  (b -> b -> b) ->
  (b -> b -> b) ->
  Expr ->
  b
foldExpr cConst cRango cSuma cResta cMult cDiv e = case e of
  Const f -> cConst f
  Rango f1 f2 -> cRango f1 f2
  Suma e1 e2 -> cSuma (rec e1) (rec e2)
  Resta e1 e2 -> cResta (rec e1) (rec e2)
  Mult e1 e2 -> cMult (rec e1) (rec e2)
  Div e1 e2 -> cDiv (rec e1) (rec e2)
  where
    rec = foldExpr cConst cRango cSuma cResta cMult cDiv

-- | Evaluar expresiones dado un generador de números aleatorios
eval :: Expr -> G Float
eval =
  foldExpr
    (,)
    (\l u gs -> dameUno (l, u) gs)
    (aux (+))
    (aux (-))
    (aux (*))
    (aux (/))
  where
    -- equivalente a (op (fst (e1 gs)) (fst (e2 (snd (e1 gs)))), snd (e2 (snd (e1 gs))))
    aux op e1 e2 gs = Data.Bifunctor.first (op (fst (e1 gs))) (e2 (snd (e1 gs)))

-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f gin = (histograma m r xl, gout)
  where
    (xl, gout) = muestra f n gin
    r = rango95 xl

-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = armarHistograma m n (eval expr)

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar =
  recrExpr
    show
    (\l u -> show l ++ "~" ++ show u)
    (aux " + " esResMulDiv)
    (aux " - " noEsConsRan)
    (aux " * " esSumResDiv)
    (aux " / " noEsConsRan)
  where
    aux op b e1 e2 s1 s2 = maybeParen (b e1) s1 ++ op ++ maybeParen (b e2) s2

esResMulDiv :: Expr -> Bool
esResMulDiv ex = constructor ex `elem` [CEResta, CEMult, CEDiv]

esSumResDiv :: Expr -> Bool
esSumResDiv ex = constructor ex `elem` [CESuma, CEResta, CEDiv]

noEsConsRan :: Expr -> Bool
noEsConsRan ex = constructor ex `notElem` [CEConst, CERango]

data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)

-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv

-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s
