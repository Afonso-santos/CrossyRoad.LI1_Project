module Tarefa3_2022li1g047_Spec where

import LI12223
import Tarefa3_2022li1g047
import Test.HUnit








testsT3 :: Test
testsT3 = test  [
        "Testes Tarefa 3 t1" ~: r1 ~=? animaJogo (Jogo (Jogador (1,2))  m1) (Move Cima) ,
       
        "Testes Tarefa 3 t2" ~: r2 ~=? animaJogo  (Jogo (Jogador (0,1))  m2 )(Move Cima),
       
        "Testes Tarefa 3 t3" ~:r3 ~=? animaJogo (Jogo (Jogador (1,0))  m3)  (Move Cima),
        
        "Testes Tarefa 3 t4" ~: r4 ~=? animaJogo (Jogo (Jogador (2,2))  m4) (Move Esquerda),
       
        "Testes Tarefa 3 t5" ~:r5   ~=? animaJogo (Jogo (Jogador (1,2))  m5) (Move Direita),
       
       
        "Testes Tarefa 3 t6" ~: r6 ~=? animaJogo (Jogo (Jogador (1,0))  m6 ) (Move Baixo) ,

        "Testes Tarefa 3 t7" ~: r7 ~=? animaJogo (Jogo (Jogador (2,1))  m7 ) Parado ,

        "Testes Tarefa 3 t8" ~:r8 ~=? animaJogo (Jogo (Jogador (1,2)) m8 ) (Parado),
        
        "Testes Tarefa 3 t9" ~:r9 ~=? animaJogo (Jogo (Jogador (1,0)) m9 ) (Parado)
        

                ]






r1 = (Jogo (Jogador (1,1)) (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),
                                   ( Estrada (-1), [Nenhum, Nenhum, Carro, Carro,Nenhum]),
                                   (Relva, [Arvore, Nenhum, Nenhum,Arvore, Arvore])]))





r2= (Jogo (Jogador (0,1)) (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]), 
                                  (Estrada (-1), [Nenhum, Nenhum, Carro, Carro,Nenhum]),
                                  (Relva, [Arvore, Nenhum, Nenhum,Arvore, Arvore])]))





r3= (Jogo (Jogador (1,0) )(Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]), 
                                  (Estrada (-1), [Nenhum, Nenhum, Carro, Carro,Nenhum]),
                                  (Relva, [Arvore, Nenhum, Nenhum,Arvore, Arvore])]))



r4=(Jogo (Jogador (1,2)) (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),
                                  (Estrada (-1), [Nenhum, Nenhum, Carro, Carro,Nenhum]),
                                  (Relva, [Arvore, Nenhum, Nenhum,Arvore, Arvore])]))



r5=(Jogo (Jogador (2,2)) (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),
                                  (Estrada (-1), [Nenhum, Nenhum, Carro, Carro,Nenhum]),
                                  (Relva, [Arvore, Nenhum, Nenhum,Arvore, Arvore])]))




r6 = (Jogo (Jogador (1,1) )(Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),
                                  (Estrada (-1), [Nenhum, Nenhum, Carro, Carro,Nenhum]),
                                  (Relva, [Arvore, Nenhum, Nenhum,Arvore, Arvore])]))



r7 =  (Jogo (Jogador (2,1)) (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),
                                  (Estrada 0, [Nenhum, Nenhum, Carro, Carro,Nenhum]),
                                  (Relva, [Arvore, Nenhum, Nenhum,Arvore, Arvore])]))




r8 = (Jogo (Jogador (3,2) ) (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]), 
                                  (Estrada (-1), [Nenhum, Nenhum, Carro, Carro,Nenhum]),
                                  (Rio 2,[Nenhum,Nenhum,Tronco,Tronco,Nenhum]),
                                  (Relva, [Arvore, Nenhum, Nenhum,Arvore, Arvore])]))

r9 = (Jogo (Jogador (1,0) )(Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]), 
                                  (Estrada (-2), [Nenhum, Carro, Carro, Nenhum,Nenhum]),
                                  (Rio 2,[Nenhum,Nenhum,Tronco,Tronco,Nenhum]),
                                  (Relva, [Arvore, Nenhum, Nenhum,Arvore, Arvore])]))


m1= (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),
            (Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),
            (Relva, [Arvore, Nenhum, Nenhum,Arvore, Arvore])])

m2 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]), -- andar contra arvore
            (Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),
            (Relva, [Arvore, Nenhum, Nenhum,Arvore, Arvore])])

m3 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),-- andar tentar sair no topo
            (Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),
            (Relva, [Arvore, Nenhum, Nenhum,Arvore, Arvore])])

m4= (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]), -- anda para para a esquerda 
            (Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),
            (Relva, [Arvore, Nenhum, Nenhum,Arvore, Arvore])])

m5= (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]), -- anda para direita
            (Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),
            (Relva, [Arvore, Nenhum, Nenhum,Arvore, Arvore])])

m6 =(Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]), -- anda para Baixo
            (Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),
            (Relva, [Arvore, Nenhum, Nenhum,Arvore, Arvore])])


m7= (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]), -- Parado
            (Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),
            (Relva, [Arvore, Nenhum, Nenhum,Arvore, Arvore])])

m8 =  (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]), -- parado em cima do tronco
            (Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),
            (Rio 2,[Tronco,Tronco,Nenhum,Nenhum,Nenhum]),
            (Relva, [Arvore, Nenhum, Nenhum,Arvore, Arvore])])

m9 =  (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]), -- parado em cima do tronco
            (Estrada (-2), [Nenhum, Nenhum, Nenhum, Carro, Carro]),
            (Rio 2,[Tronco,Tronco,Nenhum,Nenhum,Nenhum]),
            (Relva, [Arvore, Nenhum, Nenhum,Arvore, Arvore])])