{- |
Module      : Tarefa5_2022li1g047
Description : Geração contínua de um mapa e o efito de deslizar do mesmo 
Copyright   : Afonso Dionísio Santos <a104276@alunos.uminho.pt>
              Luis Enrique Diaz de Freitas <a104000@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g047 where
import Tarefa2_2022li1g047
import LI12223 
import Tarefa1_2022li1g047
import GHC.Base 
import Data.List.NonEmpty (group, group1)
import Tarefa1_2022li1g047 (tamanhoCheck)
import LI12223 (Obstaculo, Velocidade, Jogador (Jogador))
import Data.Bool (Bool (True, False))
import Data.Foldable (Foldable(length))
import Data.List

{-| A função deslizaJogo , provoca o movimento do mapa ao longo do tempo .

@
deslizaJogo:: Int->Jogo->Jogo
deslizaJogo n  (Jogo c (Mapa l t )) = (Jogo c (estendeMapa (Mapa l  init t) n) )
@

==Exemplo
>>>deslizaJogo 68 (Jogo (2,2) (Mapa 5 [(Rio 1,[Tronco,Tronco,Nenhum,Nenhum,Tronco]),
                                      (Estrada 2 ,[Carro, Carro, Nenhum,Nenhum, Carro]),
                                      (Relva, [Nenhum,Arvore,Nenhum,Nenhum,Nenhum])]) )

Jogo (Jogador (2,1)) (Mapa 5 [(Estrada (-1),[Nenhum,Nenhum,Carro,Carro,Carro]),
                            (Rio 1,[Tronco,Tronco,Nenhum,Nenhum,Tronco])])
-}









deslizaJogo:: Int->Jogo->Jogo
deslizaJogo  n (Jogo (Jogador (x,y)) (Mapa l t )) = (Jogo (Jogador(x,y+1)) (estendeMapa (Mapa l (init t)) n) )




