{- |
Module      : Tarefa3_2022li1g047
Description : Movimentação do personagem e obstáculos
Copyright   : Afonso Dionísio Santos <a104276@alunos.uminho.pt>
              Luis Enrique Diaz de Freitas <a104000@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g047 where

import LI12223
import Tarefa1_2022li1g047
import GHC.Arr (accum)
{-| A funçao animaJogo que movimenta os obstáculos (de acordo com a velocidade) do terreno em que se encontram), e o personagem, de acordo com a jogada dada.
Para a animação do jogo é necesário animar diversas coisas como por exemplo:
 * Movimento do jogador 
 * Movimentação dos obstáculos.Com particularidade de que se o carro atropelar o jogador o carro fica parado
 * Movimento dos Jogador em cima de obstáculos
 
@

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo actJogador (Mapa l tos)) novaJogada =
    let auxJogador = ajustarJogador (Mapa l tos) (actJogador)
        novoJogador = posicaoJogador (Mapa l tos) novaJogada auxJogador
        novomapa = (movImento (Jogo actJogador (Mapa l tos)))
    in Jogo novoJogador novomapa


@

==Exemplo
>>> animaJogo (Jogo (Jogador (0,1)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),
                                        (Estrada 1,[Carro,Carro,Nenhum]),
                                        (Relva,[Arvore,Nenhum,Nenhum])])) Parado

Jogo (Jogador (0,1)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),
                              (Estrada 1, [Nenhum,Carro,Carro])
                              (Relva, [Arvore,Nenhum,Nenhum])])

>>> animaJogo (Jogo (Jogador (2,2)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),
                                            (Estrada 1,[Carro,Nenhum,Nenhum]),
                                            (Relva,[Arvore,Nenhum,Nenhum])])) (Move Cima)

Jogo (Jogador (2,1)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum]),
                                     (Estrada 1,[Carro,Nenhum,Nenhum]),
                                     (Relva,[Arvore,Nenhum,Nenhum])])

>>> animaJogo (Jogo(Jogador (2,2,)) (Mapa 3 [(Rio 1 ,[Nenhum, Tronco,Nenhum]),
                                          (Relva, [Arvore ,Nenhum,Arvore]),
                                          (Relva, [Arvore , Nenhum, Nenhum])])) (Move Cima)

Jogo ( Jogador (2,2)) (Mapa 3 [(Rio 1 ,[Nenhum, Tronco,Nenhum]),
                               (Relva, [Arvore ,Nenhum,Arvore]),
                               (Relva, [Arvore , Nenhum, Nenhum])])


Funções que promovem a habilidade de movimentação do jogador para as 4 direções :Cima,Baixo,Direita e Esquerda. A mesma bloqueia a passagem do jogador atravez de árvores. 

@
posicaoJogador :: Mapa -> Jogada -> Jogador -> Jogador
posicaoJogador (Mapa l ((t,o):ts)) d j = Jogador (novaPosicao d (movImento (Jogo j (Mapa l ((t,o):ts)))) (coordenadas j))

coordenadas :: Jogador -> Coordenadas
coordenadas (Jogador (x,y)) = (x,y)

novaPosicao :: Jogada -> Mapa -> Coordenadas -> Coordenadas  
novaPosicao Parado _ (x, y) = (x, y)
novaPosicao (Move Cima) (Mapa _ tos) (x, y)
    | y == 0 = (x,y)
    | (snd(tos !! (y - 1))) !! x == Arvore = (x,y)
    | otherwise = (x, y - 1)
novaPosicao (Move Direita) (Mapa l tos) (x, y)
    | x == l - 1 = (x, y)
    | (snd(tos !! y)) !! (x + 1) == Arvore = (x, y)
    | otherwise = (x + 1, y)
novaPosicao (Move Baixo) (Mapa _ tos) (x, y)
    | y == length tos - 1 = (x, y)
    | (snd(tos !! (y + 1))) !! x == Arvore = (x, y)
    | otherwise = (x, y + 1)
novaPosicao (Move Esquerda) (Mapa _ tos) (x, y)
    | x == 0 = (x, y)
    | (snd(tos !! y)) !! (x - 1) == Arvore = (x, y)
    | otherwise = (x - 1, y)

@

Ao longo do mapa há movimentações de obstáculos, carros e troncos. Se a velocidade for positiva ,desloca se da esquerda para a direito e se for negativo, desloca-se da direita para esquerda. A função tem em consideração as coordenadas do jogador para verifica se o carro coicide com as coordenadas do jogador 

@
movImento :: Jogo -> Mapa --jogo
movImento (Jogo(Jogador(x,y))(Mapa l list )) = (Mapa l (movimentoAuc list (velOcidade list) (x,y) 0)) --(Jogo(Jogador (x,y)) (Mapa l (movimentoAuc list (velOcidade list) (x,y) 0)))

velOcidade :: [(Terreno,[Obstaculo])]->[Velocidade]
velOcidade []    = []
velOcidade (h:t) = terrenoDir (fst h ) :velOcidade t

movimentoAuc:: [(Terreno,[Obstaculo])]->[Velocidade]->(Int,Int)->Int-> [(Terreno,[Obstaculo])] 
movimentoAuc [] [] (_,_) _ = []
movimentoAuc ((Estrada v, l):t) (ve:ta) (x,y) acc 
  |ve>0 && y == acc && (((last l):(init l)) !! x) == Carro = (Estrada 0,(last l):(init l)):movimentoAuc t ta (x,y) (acc+1)
  |ve>0 && y == acc && (((last l):(init l)) !! x) /= Carro = movimentoAuc ((Estrada v,(last l):(init l)):t)  ((ve-1):ta) (x,y) acc
  |ve>0 && y /= acc                      = movimentoAuc ((Estrada v,(last l):(init l)):t)  ((ve-1):ta) (x,y) acc
  |ve<0 && y == acc && (((tail l)++[head l]) !! x) == Carro = (Estrada 0,(tail l)++[head l]):movimentoAuc t ta (x,y) (acc+1)
  |ve<0 && y == acc && (((tail l)++[head l]) !! x) /= Carro = movimentoAuc ((Estrada v,(tail l)++[head l]):t) ((ve+1):ta) (x,y) acc
  |ve<0 && y /= acc                      = movimentoAuc ((Estrada v,(tail l)++[head l]):t) ((ve+1):ta) (x,y) acc
  |otherwise = (Estrada v,l) : movimentoAuc t ta (x,y) (acc+1)
movimentoAuc ((Rio v,l):t) (ve:ta) (x,y) acc
  |ve>0 = movimentoAuc ((Rio v,((last l):(init l))):t)  ((ve-1):ta) (x,y) acc
  |ve<0 = movimentoAuc ((Rio v,((tail l)++[head l])):t) ((ve+1):ta) (x,y) acc
  |otherwise = (Rio v,l) : movimentoAuc t ta (x,y) (acc+1)
movimentoAuc ((Relva ,l):t) (ve:ta) (x,y) acc= (Relva,l):movimentoAuc t ta (x,y ) (acc+1)  

@

O jogador consegue se movimentar em cima do tronco. Mas o tronco tambem tem a sua movimentação. Por isso temos de ter esse cuidaed.

@
ajustarJogador :: Mapa -> Jogador -> Jogador
ajustarJogador (Mapa l tos) (Jogador (x,y))
  | (snd(tos !! y)) !! x /= Tronco = Jogador (x, y)
  | x + v < 0 = Jogador (0,y)
  | l - 1 < x + v = Jogador (l - 1, y)
  | otherwise = Jogador (x + v, y)
  where v = terrenoDir (fst (tos !! y))
@
-}
 

movImento :: Jogo -> Mapa --jogo
movImento (Jogo(Jogador(x,y))(Mapa l list )) = (Mapa l (movimentoAuc list (velOcidade list) (x,y) 0)) --(Jogo(Jogador (x,y)) (Mapa l (movimentoAuc list (velOcidade list) (x,y) 0)))

velOcidade :: [(Terreno,[Obstaculo])]->[Velocidade]
velOcidade []    = []
velOcidade (h:t) = terrenoDir (fst h ) :velOcidade t

movimentoAuc:: [(Terreno,[Obstaculo])]->[Velocidade]->(Int,Int)->Int-> [(Terreno,[Obstaculo])] 
movimentoAuc [] [] (_,_) _ = []
movimentoAuc ((Estrada v, l):t) (ve:ta) (x,y) acc 
  |ve>0 && y == acc && (((last l):(init l)) !! x) == Carro = (Estrada 0,(last l):(init l)):movimentoAuc t ta (x,y) (acc+1)
  |ve>0 && y == acc && (((last l):(init l)) !! x) /= Carro = movimentoAuc ((Estrada v,(last l):(init l)):t)  ((ve-1):ta) (x,y) acc
  |ve>0 && y /= acc                      = movimentoAuc ((Estrada v,(last l):(init l)):t)  ((ve-1):ta) (x,y) acc
  |ve<0 && y == acc && (((tail l)++[head l]) !! x) == Carro = (Estrada 0,(tail l)++[head l]):movimentoAuc t ta (x,y) (acc+1)
  |ve<0 && y == acc && (((tail l)++[head l]) !! x) /= Carro = movimentoAuc ((Estrada v,(tail l)++[head l]):t) ((ve+1):ta) (x,y) acc
  |ve<0 && y /= acc                      = movimentoAuc ((Estrada v,(tail l)++[head l]):t) ((ve+1):ta) (x,y) acc
  |otherwise = (Estrada v,l) : movimentoAuc t ta (x,y) (acc+1)
movimentoAuc ((Rio v,l):t) (ve:ta) (x,y) acc
  |ve>0 = movimentoAuc ((Rio v,((last l):(init l))):t)  ((ve-1):ta) (x,y) acc
  |ve<0 = movimentoAuc ((Rio v,((tail l)++[head l])):t) ((ve+1):ta) (x,y) acc
  |otherwise = (Rio v,l) : movimentoAuc t ta (x,y) (acc+1)
movimentoAuc ((Relva ,l):t) (ve:ta) (x,y) acc= (Relva,l):movimentoAuc t ta (x,y ) (acc+1)  

--funcao auxiliar de "posicaoJogador"
coordenadas :: Jogador -> Coordenadas
coordenadas (Jogador (x,y)) = (x,y)

--funcao auxiliar de "posicaoJogador"
novaPosicao :: Jogada -> Mapa -> Coordenadas -> Coordenadas  
novaPosicao Parado _ (x, y) = (x, y)
novaPosicao (Move Cima) (Mapa _ tos) (x, y)
    | y == 0 = (x,y)
    | (snd(tos !! (y - 1))) !! x == Arvore = (x,y)
    | otherwise = (x, y - 1)
novaPosicao (Move Direita) (Mapa l tos) (x, y)
    | x == l - 1 = (x, y)
    | (snd(tos !! y)) !! (x + 1) == Arvore = (x, y)
    | otherwise = (x + 1, y)
novaPosicao (Move Baixo) (Mapa _ tos) (x, y)
    | y == length tos - 1 = (x, y)
    | (snd(tos !! (y + 1))) !! x == Arvore = (x, y)
    | otherwise = (x, y + 1)
novaPosicao (Move Esquerda) (Mapa _ tos) (x, y)
    | x == 0 = (x, y)
    | (snd(tos !! y)) !! (x - 1) == Arvore = (x, y)
    | otherwise = (x - 1, y)

--posicao do jogador logo de fazer a jogada
posicaoJogador :: Mapa -> Jogada -> Jogador -> Jogador
posicaoJogador (Mapa l ((t,o):ts)) d j = Jogador (novaPosicao d (movImento (Jogo j (Mapa l ((t,o):ts)))) (coordenadas j))

--o movimento do jogador acima do tronco
ajustarJogador :: Mapa -> Jogador -> Jogador
ajustarJogador (Mapa l tos) (Jogador (x,y))
  | (snd(tos !! y)) !! x /= Tronco = Jogador (x, y)
  | x + v < 0 = Jogador (0,y)
  | l - 1 < x + v = Jogador (l - 1, y)
  | otherwise = Jogador (x + v, y)
  where v = terrenoDir (fst (tos !! y))

-- anima o jogo, oseja, devolve o novo map com os obstaculos na nova posicao e o jogador com as suas novas coordenadas logo de fazer uma jogada e se estiver acima de um tronco
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo actJogador (Mapa l tos)) novaJogada =
    let auxJogador = ajustarJogador (Mapa l tos) (actJogador)
        novoJogador = posicaoJogador (Mapa l tos) novaJogada auxJogador
        novomapa = (movImento (Jogo actJogador (Mapa l tos)))
    in Jogo novoJogador novomapa

