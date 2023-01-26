module Tarefa4_2022li1g047_Spec where

import LI12223
import Tarefa4_2022li1g047
import Test.HUnit

testsT4 :: Test
testsT4 = test [
            "Testes Tarefa 4 m1"  ~:False ~=? jogoTerminou j1 ,
            "Testes Tarefa 4 m2"  ~:True ~=? jogoTerminou j2 ,
            "Testes Tarefa 4 m3"  ~:True ~=? jogoTerminou j3 ,
            "Testes Tarefa 4 m4"  ~: False ~=? jogoTerminou j4

              ]

--Vivo
j1=(Jogo (Jogador (1,2)) (Mapa 5 [(Rio 2,[Tronco,Nenhum,Nenhum,Tronco,Tronco]),
                                  (Estrada 3,[Nenhum,Carro,Carro,Carro,Nenhum]),
                                  (Relva,[Arvore,Nenhum,Nenhum,Nenhum,Arvore])]))

--morto no carro
j2= (Jogo (Jogador (2,1)) (Mapa 5 [(Rio 2,[Tronco,Nenhum,Nenhum,Nenhum,Nenhum]),
                                           (Estrada 3,[Nenhum,Carro,Carro,Nenhum,Nenhum]),
                                           (Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhum])]))
--morto afogado              
j3= (Jogo (Jogador (4,0)) (Mapa 5 [(Rio 2,[Tronco,Nenhum,Nenhum,Nenhum,Nenhum]),
                                           (Estrada 3,[Nenhum,Carro,Carro,Nenhum,Nenhum]),
                                           (Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhum])]))
--Vivo em cima do tronco
j4= (Jogo (Jogador (0,0)) (Mapa 5 [(Rio 2,[Tronco,Nenhum,Nenhum,Nenhum,Nenhum]),
                                           (Estrada 3,[Nenhum,Carro,Carro,Nenhum,Nenhum]),
                                           (Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhum])]))