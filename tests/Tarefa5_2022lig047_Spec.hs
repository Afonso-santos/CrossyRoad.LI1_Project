module Tarefa5_2022li1g047_Spec where

import LI12223
import Tarefa5_2022li1g047
import Test.HUnit


testsT5 :: Test
testsT5 = test  [
        "Testes Tarefa 5 t1" ~: Jogo (Jogador (2,1)) (Mapa 5 [(Estrada (-1),[Nenhum,Nenhum,Carro,Carro,Carro]), (Rio 1,[Tronco,Tronco,Nenhum,Nenhum,Tronco])]) ~=? deslizaJogo 68 teste1,
        "Testes Tarefa 5 t2" ~: Jogo (Jogador (2,1)) (Mapa 5 [(Relva,[Nenhum,Arvore,Arvore,Arvore,Arvore]),(Rio 1,[Tronco,Tronco,Nenhum,Nenhum,Tronco])]) ~=? deslizaJogo 43 teste1
                ]



teste1= (Jogo (Jogador (2,0))
                             (Mapa 5 [(Rio 1,[Tronco,Tronco,Nenhum,Nenhum,Tronco]),
                                      (Estrada 2,[Carro, Carro, Nenhum,Nenhum, Carro]) ] ))

s