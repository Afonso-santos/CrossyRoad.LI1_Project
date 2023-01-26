{- |
Module      : Tarefa4_2022li1g047
Description : Determinar se o jogo terminou
Copyright   : Afonso Dionísio Santos <a104276@alunos.uminho.pt>
              Luis Enrique Diaz de Freitas <a104000@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g047 where

import LI12223
import Tarefa1_2022li1g047
{-|A funçao jogoTerminou verifica os casos em que o jogador tem mesma posiçao do que um carro ou se o jogador cai no Rio

@
jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo (Jogador (x,y)) (Mapa l tos))
    | tipoTerreno (fst (tos !! y)) == "Rio" && (snd (tos !! y)) !! x == Nenhum = True
    | tipoTerreno (fst (tos !! y)) == "Estrada" && (snd (tos !! y)) !! x == Carro = True
    | otherwise = False
@

==Exemplo
>>> jogoTerminou (Jogo (Jogador (1,2)) (Mapa 5 [(Rio 2,[Tronco,Nenhum,Nenhum,Tronco,Tronco]),
                                                (Estrada 3,[Nenhum,Carro,Carro,Carro,Nenhum]),
                                                (Relva,[Arvore,Nenhum,Nenhum,Nenhum,Arvore])]))

False

>>>jogoTerminou (Jogo (Jogador (4,0)) (Mapa 5 [(Rio 2,[Tronco,Nenhum,Nenhum,Nenhum,Nenhum]),
                                              (Estrada 3,[Nenhum,Carro,Carro,Nenhum,Nenhum]),
                                              (Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhhum])]))

True
-}




-- determina se o jogo acabou, se o jogador esta nas coordenasdaas de um carro/agua(nenhum no terrenodo rio)
jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo (Jogador (x,y)) (Mapa l tos))
    | tipoTerreno (fst (tos !! y)) == "Rio" && (snd (tos !! y)) !! x == Nenhum = True
    | tipoTerreno (fst (tos !! y)) == "Estrada" && (snd (tos !! y)) !! x == Carro = True
    | otherwise = False