module Main (main) where

import App
import Expr
import Expr.Parser
import GHC.Stack (HasCallStack)
import Generador
import Histograma
import Test.HUnit
import Util

infix 1 @?/=, ~?/=

-- definimos @?/= y ~?/= para facilitar los tests
(@?/=) :: (Eq a, Show a) => a -> a -> Assertion
actual @?/= expected = assertBool "" (expected /= actual)

(~?/=) :: (Eq a, Show a) => a -> a -> Test
actual ~?/= expected = TestCase (actual @?/= expected)

main :: IO ()
main = runTestTTAndExit allTests

-- | Función auxiliar para marcar tests como pendientes a completar
completar :: (HasCallStack) => Test
completar = TestCase (assertFailure "COMPLETAR")

allTests :: Test
allTests =
  test
    [ "Ej 1 - Util.alinearDerecha" ~: testsAlinearDerecha,
      "Ej 2 - Util.actualizarElem" ~: testsActualizarElem,
      "Ej 3 - Histograma.vacio" ~: testsVacio,
      "Ej 4 - Histograma.agregar" ~: testsAgregar,
      "Ej 5 - Histograma.histograma" ~: testsHistograma,
      "Ej 6 - Histograma.casilleros" ~: testsCasilleros,
      "Ej 7 - Expr.recrExpr" ~: testsRecr,
      "Ej 7 - Expr.foldExpr" ~: testsFold,
      "Ej 8 - Expr.eval" ~: testsEval,
      "Ej 9 - Expr.armarHistograma" ~: testsArmarHistograma,
      "Ej 10 - Expr.evalHistograma" ~: testsEvalHistograma,
      "Ej 11 - Expr.mostrar" ~: testsMostrar,
      "Expr.Parser.parse" ~: testsParse,
      "App.mostrarFloat" ~: testsMostrarFloat,
      "App.mostrarHistograma" ~: testsMostrarHistograma
    ]

testsAlinearDerecha :: Test
testsAlinearDerecha =
  test
    [ alinearDerecha 6 "hola" ~?= "  hola",
      alinearDerecha 10 "incierticalc" ~?= "incierticalc",
      alinearDerecha 2 "" ~?= "  ",
      alinearDerecha 3 " " ~?= "   ",
      alinearDerecha 4 "hola" ~?= "hola"
    ]

testsActualizarElem :: Test
testsActualizarElem =
  test
    [ actualizarElem 0 (+ 10) [1, 2, 3] ~?= [11, 2, 3],
      actualizarElem 1 (+ 10) [1, 2, 3] ~?= [1, 12, 3],
      actualizarElem (-1) (+ 10) [1, 2, 3] ~?= [1, 2, 3],
      actualizarElem 3 (* 2) [1, 2, 3, 4] ~?= [1, 2, 3, 8],
      actualizarElem 3 (* 2) [] ~?= [],
      actualizarElem 2 (++ "a") ["a", "a", "b"] ~?= ["a", "a", "ba"]
    ]

testsVacio :: Test
testsVacio =
  test
    [ casilleros (vacio 1 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 2 0 0,
              Casillero 2 4 0 0,
              Casillero 4 6 0 0,
              Casillero 6 infinitoPositivo 0 0
            ],
      casilleros (vacio 5 (1, 31))
        ~?= [ Casillero infinitoNegativo 1.0 0 0.0,
              Casillero 1.0 7.0 0 0.0,
              Casillero 7.0 13.0 0 0.0,
              Casillero 13.0 19.0 0 0.0,
              Casillero 19.0 25.0 0 0.0,
              Casillero 25.0 31.0 0 0.0,
              Casillero 31.0 infinitoPositivo 0 0.0
            ]
    ]

testsAgregar :: Test
testsAgregar =
  let h0 = vacio 3 (0, 6)
   in test
        [ casilleros (agregar 0 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 100, -- El 100% de los valores están acá
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 2 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 1 100, -- El 100% de los valores están acá
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar (-1) h0)
            ~?= [ Casillero infinitoNegativo 0 1 100, -- El 100% de los valores están acá
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 10 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 1 100
                ],
          casilleros (agregar (-1) (agregar 10 h0))
            ~?= [ Casillero infinitoNegativo 0 1 50,
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 1 50
                ],
          casilleros (agregar (-1) (agregar 10 (agregar 10 (agregar 10 h0))))
            ~?= [ Casillero infinitoNegativo 0 1 25,
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 3 75
                ],
          casilleros (agregar (-1) (agregar 10 (agregar 10 (agregar 10 (agregar 3 h0)))))
            ~?= [ Casillero infinitoNegativo 0 1 20,
                  Casillero 0 2 0 0,
                  Casillero 2 4 1 20,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 3 60.000004
                ]
        ]

testsHistograma :: Test
testsHistograma =
  test
    [ histograma 4 (1, 5) [1, 2, 3] ~?= agregar 3 (agregar 2 (agregar 1 (vacio 4 (1, 5)))),
      histograma 4 (4, 6) [] ~?= vacio 4 (4, 6),
      histograma 3 (4, 6) [5, 5, 5] ~?= agregar 5 (agregar 5 (agregar 5 (vacio 3 (4, 6))))
    ]

testsCasilleros :: Test
testsCasilleros =
  test
    [ casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casilleros (agregar 2 (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 1 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ]
    ]

testsRecr :: Test
testsRecr =
  test
    [ recrExpr
        (\c -> Const (c + 1))
        (\l u -> Rango (l + 1) (u + 1))
        (\e1 e2 r1 r2 -> Suma e1 (Suma r1 r2))
        (\e1 e2 r1 r2 -> Resta e1 (Resta r1 r2))
        (\e1 e2 r1 r2 -> Mult (Mult r1 r2) e2)
        (\e1 e2 r1 r2 -> Div (Div r1 r2) e2)
        (Suma (Rango 1 5) (Const 1))
        ~?= Suma (Rango 1.0 5.0) (Suma (Rango 2.0 6.0) (Const 2.0))
    ]

testsFold :: Test
testsFold =
  test
    [ foldExpr
        (\c -> Const (c + 1))
        (\l u -> Rango (l + 1) (u + 1))
        Suma
        Resta
        Mult
        Div
        (Suma (Rango 1 5) (Const 1))
        ~?= Suma (Rango 2 6) (Const 2)
    ]

testsEval :: Test
testsEval =
  let e1 = Suma (Rango 1 5) (Const 1)
   in test
        [ fst (eval e1 genFijo) ~?= 4.0,
          fst (eval e1 (genNormalConSemilla 0)) ~?= 3.7980492,
          -- el primer rango evalua a 2.7980492 y el segundo a 3.1250308
          fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 5.92308,
          -- el primer rango evalua a 2.7980492, el segundo a 3.1250308, y el tercero a 5.464013
          fst (eval (Suma (Rango 1 5) (Suma (Rango 1 5) (Rango 1 5))) (genNormalConSemilla 0)) ~?= 11.387093,
          -- devuelve el mismo resultado con genFijo
          fst (eval e1 genFijo) ~?= fst (eval e1 (snd (eval e1 genFijo))),
          -- devuelve resultados distintos con genNormalConSemilla
          fst (eval e1 (genNormalConSemilla 0)) ~?/= fst (eval e1 (snd (eval e1 (genNormalConSemilla 0))))
        ]

testsArmarHistograma :: Test
testsArmarHistograma =
  let h1 = armarHistograma 1 1 (dameUno (1, 5)) (genNormalConSemilla 0)
      h2 = armarHistograma 10 5 (dameUno (1, 5)) (genNormalConSemilla 0)
   in test
        [ length (casilleros (fst h1)) ~?= 3,
          length (casilleros (fst h2)) ~?= 12,
          fst h1 ~?= histograma 1 (1.7980492, 3.7980492) [fst (dameUno (1, 5) (genNormalConSemilla 0))],
          fst h2 ~?= histograma 10 (0.77653575, 5.79564075) [2.7980492, 3.1250308, 5.464013, 3.526857, 1.5164927]
        ]

testsEvalHistograma :: Test
testsEvalHistograma =
  let e1 = evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105))
   in test
        [ length (casilleros (fst (e1 genFijo))) ~?= 13,
          length (casilleros (fst (e1 (genNormalConSemilla 0)))) ~?= 13,
          fst (e1 genFijo) ~?= histograma 11 (104.5, 106.5) [105.5, 105.5, 105.5, 105.5, 105.5, 105.5, 105.5, 105.5, 105.5, 105.5],
          fst (e1 (genNormalConSemilla 0)) ~?= histograma 11 (102.005486, 109.4118278) [105.45434, 108.62258, 104.74236, 108.77485, 101.97703, 105.34323, 106.31043, 104.52736, 106.369606, 104.96473],
          -- devuelve el mismo resultado con genFijo
          fst (e1 genFijo) ~?= fst (e1 (snd (e1 genFijo))),
          -- devuelve resultados distintos con genNormalConSemilla
          fst (e1 (genNormalConSemilla 0)) ~?/= fst (e1 (snd (e1 (genNormalConSemilla 0))))
        ]

testsParse :: Test
testsParse =
  test
    [ parse "1" ~?= Const 1.0,
      parse "-1.7 ~ -0.5" ~?= Rango (-1.7) (-0.5),
      parse "1+2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2 * 3" ~?= Suma (Const 1.0) (Mult (Const 2.0) (Const 3.0)),
      parse "1 + 2 + 3" ~?= Suma (Suma (Const 1.0) (Const 2.0)) (Const 3.0),
      parse "1 + (2 + 3)" ~?= Suma (Const 1.0) (Suma (Const 2.0) (Const 3.0)),
      parse "1 + 2 ~ 3 + 4" ~?= Suma (Suma (Const 1.0) (Rango 2.0 3.0)) (Const 4.0),
      parse "1 - 2 - 3 - 4" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "(((1 - 2) - 3) - 4)" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "1 " ~?= Const 1.0,
      parse "   1    " ~?= Const 1.0
    ]

testsMostrar :: Test
testsMostrar =
  test
    [ mostrar (Div (Suma (Rango 1 5) (Mult (Const 3) (Rango 100 105))) (Const 2))
        ~?= "(1.0~5.0 + (3.0 * 100.0~105.0)) / 2.0",
      mostrar (Suma (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Const 1) (Suma (Const 2) (Suma (Const 3) (Const 4))))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Suma (Const 1) (Const 2)) (Suma (Const 3) (Const 4)))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Mult (Mult (Mult (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Const 1) (Mult (Const 2) (Mult (Const 3) (Const 4))))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Mult (Const 1) (Const 2)) (Mult (Const 3) (Const 4)))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Resta (Resta (Const 1) (Const 2)) (Resta (Const 3) (Const 4)))
        ~?= "(1.0 - 2.0) - (3.0 - 4.0)",
      mostrar (Resta (Resta (Resta (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 - 2.0) - 3.0) - 4.0",
      mostrar (Suma (Mult (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 + 2.0) * 3.0) + 4.0",
      mostrar (Mult (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "(1.0 + 2.0 + 3.0) * 4.0",
      mostrar (Const 0) ~?= "0.0",
      mostrar (Rango 1 5) ~?= "1.0~5.0"
    ]

testsMostrarFloat :: Test
testsMostrarFloat =
  test
    [ mostrarFloat 0.0 ~?= "0.00",
      mostrarFloat 1.0 ~?= "1.00",
      mostrarFloat (-1.0) ~?= "-1.00",
      -- Redondeo
      mostrarFloat 3.14159 ~?= "3.14",
      mostrarFloat 2.71828 ~?= "2.72",
      mostrarFloat 0.000001 ~?= "1.00e-6",
      mostrarFloat 100000 ~?= "100000.00",
      -- Infinitos
      mostrarFloat infinitoPositivo ~?= "+inf",
      mostrarFloat infinitoNegativo ~?= "-inf"
    ]

testsMostrarHistograma :: Test
testsMostrarHistograma =
  let h0 = vacio 3 (0, 6)
      h123 = agregar 1 (agregar 2 (agregar 3 h0))
   in test
        [ lines (mostrarHistograma h123)
            ~?= [ "6.00 - +inf |",
                  "4.00 - 6.00 |",
                  "2.00 - 4.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 66.67%",
                  "0.00 - 2.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒",
                  "-inf - 0.00 |"
                ],
          lines (mostrarHistograma (agregar 1 (vacio 3 (0, 1000))))
            ~?= [ "  1000.00 - +inf |",
                  "666.67 - 1000.00 |",
                  " 333.33 - 666.67 |",
                  "   0.00 - 333.33 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 100.00%",
                  "     -inf - 0.00 |"
                ]
        ]
