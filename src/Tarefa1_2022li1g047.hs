{- |
Module      : Tarefa1_2022li1g047
Description : Validação de um mapa
Copyright   : Afonso Dionísio Santos <a104276@alunos.uminho.pt>
              Luis Enrique Diaz de Freitas <a104000@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g047 where

import LI12223
import Data.List (group)
import Control.Exception (BlockedIndefinitelyOnMVar)
{-|--A função mapaValido,constituido por funções auxiliares ,que verifica  se o mapa corresponde as seguintes condicionantes:
--1. Não existem obstáculos em terrenos impróprios, exemplo: troncos em estradas ou relvas, árvores em rios ou estradas, etc.(terrenoValido),
--2. Se dois rios são seguidos têm dirececões opostas.(riosOpostos),
--3. Carros e troncos não podem ultrupassar um certo comprimento, 3 unidades e 5 unidades , respetivamente.(obstaculosValidos),
--4. Ou seja, uma linha não pode ser composta exclusivamente por obstáculos, precisando de haver pelo menos um espaço livre.(nenhumValidos),
--5. O comprimento da lista de obstáculos de cada linha corresponde exatamente a largura do mapa.(linhasValidas),
--6. Contiguamente, não devem existir mais do que 4 rios, nem 5 estradas ou relvas.s

@
mapaValido :: Mapa -> Bool
mapaValido (Mapa _ []) = True
mapaValido (Mapa l t)
    | terrenoValido t == False = False
    | riosOpostos t == False = False
    | obstaculosValidos t == False = False
    | nenhumValidos t == False = False
    | linhaValidas (Mapa l t) == False = False
    | 0 < sum[1 | ("Rio", x) <-list,maxRios < x] = False
    | 0 < sum[1 | ("Estrada", x) <-list,maxEstrada < x] = False
    | 0 < sum[1 | ("Relva", x) <-list,maxRelva < x] = False
    | otherwise = True 
    where
        list = listLinha t


@

==Exemplos
>>>mapaValido 5 [(Relva,[Arvore,Arvore,Nenhum,Nenhum,Arvore]),
                (Estrada 2,[Carro,Carro,Nenhum,Nenhum,Carro]),
                (Rio 2,[Nenhum,Nenhum,Tronco,Tronco, Nenhum]),
                (Rio (-2),[Nenhum,Tronco,Tronco,Nenhum,Nenhum])]
True

>>>mapaValido 5[(Rio (-2),[Nenhum,Tronco,Tronco,Nenhum,Nenhum]),
                (Relva,[Arvore,Arvore,Nenhum,Nenhum,Arvore]),
                (Estrada 2,[Carro,Carro,Nenhum,Nenhum,Carro]),
                (Rio 2,[Nenhum,Nenhum,Tronco,Tronco, Nenhum]),
                (Relva ,[Nenhum,Arvore,Arvore,Nenhum,Nenhum])]
True

>>>mapaValido 4 [(Relva,[Arvore,Arvore,Nenhum,Nenhum,Arvore]),
                (Estrada 2,[Carro,Carro,Nenhum,Nenhum,Carro]),
                (Rio 2,[Nenhum,Nenhum,Tronco,Tronco, Nenhum]),
                (Rio (-2),[Nenhum,Tronco,Tronco,Nenhum,Nenhum])] 
False

>>>mapaValido 5 [(Relva,[Arvore,Arvore,Nenhum,Nenhum,Arvore]),
                (Estrada 2,[Carro,Carro,Carro,Nenhum,Carro]),
                (Rio 2,[Nenhum,Nenhum,Tronco,Tronco, Nenhum]),
                (Rio 2,[Nenhum,Tronco,Tronco,Nenhum,Nenhum])]
False 


1. terrenoValido ,função auxiliar, é a função que verifica se os obstáculos no terrenos respetivos,
(Carros nas estradas,Arvore na relva ,troncos no rio)

@
terrenoValido :: [(Terreno, [Obstaculo])] -> Bool
terrenoValido [] = True
terrenoValido ((t, o):ts)
    | tipoTerreno t == "Rio"     && 0 < sum[1 | Carro <-o]  + sum[1 | Arvore <-o] = False
    | tipoTerreno t == "Estrada" && 0 < sum[1 | Tronco <-o] + sum[1 | Arvore <-o] = False
    | tipoTerreno t == "Relva"   && 0 < sum[1 | Carro <-o]  + sum[1 | Tronco <-o] = False
    | otherwise = terrenoValido ts


@

==Exemplos
>>>terrenoValido [(Relva,[Arvore,Arvore,Nenhum,Nenhum,Arvore]),
                (Estrada 2,[Carro,Carro,Nenhum,Nenhum,Carro]),
                (Rio 2,[Nenhum,Nenhum,Tronco,Tronco, Nenhum]),
                (Rio (-2),[Nenhum,Tronco,Tronco,Nenhum,Nenhum])]
True

>>>terrenoValido [(Relva,[Arvore,Arvore,Nenhum,Nenhum,Arvore]),
                (Estrada 2,[Carro,Carro,Nenhum,Nenhum,Carro]),
                (Rio 2,[Carro,Nenhum,Tronco,Tronco, Nenhum]),
                (Rio (-2),[Nenhum,Tronco,Tronco,Nenhum,Nenhum])]
False



2. riosOpostos verifica se os rios consecutivos se tem direções oposto, ou seja se a veocidade for positiva ,desloca se da esquerda para a direita,o próximo obrigatoriamente negativo, desloca-se da direito para a esquerda.

@

riosOpostos :: [(Terreno, [Obstaculo])] -> Bool
riosOpostos [] = True
riosOpostos ((t, o):ts)
    | 0 < length ts && tipoTerreno t == "Rio" && tipoTerreno (fst (head ts)) == "Rio" && (terrenoDir t)*(terrenoDir (fst (head ts))) > 0 = False 
    | otherwise = riosOpostos ts

@

3. obstaculosValidos analisa os obstáculos de cada terreno, neste caso os carros e os troncos.Se respeitam o tamanho suposto .

@                     
obstaculosValidos :: [(Terreno, [Obstaculo])] -> Bool
obstaculosValidos [] = True
obstaculosValidos ((t, o):ts)
    | tipoTerreno t == "Rio" && (listObts o) = False
    | tipoTerreno t == "Estrada" && (listObts o)= False
    | tipoTerreno t =="Relva" = True
    | otherwise = obstaculosValidos ts
@
>>>obstaculosValidos [(Estrada 2,[Carro,Carro,Nenhum,Nenhum,Carro])
                     (Rio 2,[Nenhum,Nenhum,Tronco,Tronco, Nenhum]),
                     (Estrada 2,[Carro,Carro,Nenhum,Nenhum,Carro]),
                     (Rio 2,[Nenhum,Nenhum,Tronco,Tronco, Nenhum])] 
True
                     

>>>obstaculosValidos [(Rio (-2),[Tronco,Tronco,Tronco,Tronco,Tronco])
                     (Relva,[Arvore,Arvore,Nenhum,Nenhum,Arvore]),
                     (Estrada 2,[Carro,Carro,Nenhum,Carro,Carro]),
                     (Rio 2,[Nenhum,Nenhum,Tronco,Tronco, Nenhum])]
False 


3.1 A função  obstaculosValidos é constituida por 4 auxiliares.As auxiliares descrevem por si mesmo

@
listObts :: [Obstaculo]->Bool
listObts []=True
listObts l= listObts2 (group l) && checkO  (group l)
@
@
listObts2::[[Obstaculo]]->Bool
listObts2 []= True
listObts2 (h:t)
  |(head h==Tronco && length h >5 )||(head h ==Carro && (length h) >3) =False
  |head h == Arvore || head h == Nenhum = True
  |otherwise = listObts2 t
@
@
checkO::[[Obstaculo]]->Bool
checkO (h:t)
  |head h ==last (last t) && (tamanhoCheck (h) (last t))==False  =False
  |otherwise= True 

tamanhoCheck::[Obstaculo]->[Obstaculo]->Bool
tamanhoCheck ( Tronco :t) l2 
  | (length (Tronco:t)) +(length l2)>5 =False 
  | otherwise = True 
tamanhoCheck (Carro:t) l2 
  |(length (Carro:t))+ (length l2)>3   =False
  |otherwise =True 


@

4.A funçao nenhumValidos verifica se há pelo menos um obstaculo «Nenhum»


@
nenhumValidos:: [(Terreno, [Obstaculo])] -> Bool
nenhumValidos [] = True
nenhumValidos ((t, o):ts)
    | sum[1 | Nenhum <-o] == 0 = False
    | otherwise = nenhumValidos ts

@

5.As linhasValidas, o nome da funçao é auto explicativo. Verifica se a largura do mapa corresponde ao tamanho da lista de objetos

@
linhaValidas :: Mapa -> Bool
linhaValidas (Mapa l []) = True
linhaValidas (Mapa l ((t, o):ts))
    | l /= length o = False
    | otherwise = linhaValidas (Mapa l ts)
@ 

6.A funçao listLinha , conta quantos terrenos continuos há , que depois na função mapa valido verifica se o numero é valido .

@
listLinha :: [(Terreno, a)] -> [(String, Int)]
listLinha [] = []
listLinha ((Rio _, _):ts)
    | length (ts) > 0 && fst (head l) == "Rio" = ("Rio", snd (head l) + 1):(tail l)
    | otherwise = ("Rio", 1):l
    where
        l = listLinha ts
listLinha ((Estrada _, _):ts)
    | length (ts) > 0 && fst (head l) == "Estrada" = ("Estrada", snd (head l) + 1):(tail l)
    | otherwise = ("Estrada", 1):l
    where
        l = listLinha ts
listLinha ((Relva, _):ts)
    | length (ts) > 0 && fst (head l) == "Relva" = ("Relva", snd (head l) + 1):(tail l)
    | otherwise = ("Relva", 1):l
    where
        l = listLinha ts
@
-}




maxTroncos = 5
maxCarros = 3
maxRios = 4
maxEstradas = 5
maxRelvas = 5

tipoTerreno :: Terreno -> String
tipoTerreno (Rio _) = "Rio"
tipoTerreno (Estrada _) = "Estrada"
tipoTerreno Relva = "Relva"

terrenoDir :: Terreno -> Int
terrenoDir (Rio v) = v
terrenoDir (Estrada v) = v
terrenoDir _ = 0

terrenoValido :: [(Terreno, [Obstaculo])] -> Bool
terrenoValido [] = True
terrenoValido ((t, o):ts)
    | tipoTerreno t == "Rio"     && 0 < sum[1 | Carro <-o]  + sum[1 | Arvore <-o] = False
    | tipoTerreno t == "Estrada" && 0 < sum[1 | Tronco <-o] + sum[1 | Arvore <-o] = False
    | tipoTerreno t == "Relva"   && 0 < sum[1 | Carro <-o]  + sum[1 | Tronco <-o] = False
    | otherwise = terrenoValido ts



obstaculosValidos :: [(Terreno, [Obstaculo])] -> Bool
obstaculosValidos [] = True
obstaculosValidos ((t, o):ts)
    | tipoTerreno t == "Rio" && (listObts o) = False
    | tipoTerreno t == "Estrada" && (listObts o)= False
    | tipoTerreno t =="Relva" = True
    | otherwise = obstaculosValidos ts

listObts :: [Obstaculo]->Bool
listObts []=True
listObts l= listObts2 (group l) && checkO  (group l)

listObts2::[[Obstaculo]]->Bool
listObts2 []= True
listObts2 (h:t)
  |(head h==Tronco && length h >5 )||(head h ==Carro && (length h) >3) =False
  |head h == Arvore || head h == Nenhum = True
  |otherwise = listObts2 t


checkO::[[Obstaculo]]->Bool
checkO (h:t)
  |head h ==last (last t) && (tamanhoCheck (h) (last t))==False  =False
  |otherwise= True 

tamanhoCheck::[Obstaculo]->[Obstaculo]->Bool
tamanhoCheck ( Tronco :t) l2 
  | (length (Tronco:t)) +(length l2)>5 =False 
  | otherwise = True 
tamanhoCheck (Carro:t) l2 
  |(length (Carro:t))+ (length l2)>3   =False
  |otherwise =True 

riosOpostos :: [(Terreno, [Obstaculo])] -> Bool
riosOpostos [] = True
riosOpostos ((t, o):ts)
    | 0 < length ts && tipoTerreno t == "Rio" && tipoTerreno (fst (head ts)) == "Rio" && (terrenoDir t)*(terrenoDir (fst (head ts))) > 0 = False 
    | otherwise = riosOpostos ts

nenhumValidos :: [(Terreno, [Obstaculo])] -> Bool
nenhumValidos [] = True
nenhumValidos ((t, o):ts)
    | sum[1 | Nenhum <-o] == 0 = False
    | otherwise = nenhumValidos ts

linhaValidas :: Mapa -> Bool
linhaValidas (Mapa l []) = True
linhaValidas (Mapa l ((t, o):ts))
    | l /= length o = False
    | otherwise = linhaValidas (Mapa l ts)

listLinha :: [(Terreno, a)] -> [(String, Int)]
listLinha [] = []
listLinha ((Rio _, _):ts)
    | length (ts) > 0 && fst (head l) == "Rio" = ("Rio", snd (head l) + 1):(tail l)
    | otherwise = ("Rio", 1):l
    where
        l = listLinha ts
listLinha ((Estrada _, _):ts)
    | length (ts) > 0 && fst (head l) == "Estrada" = ("Estrada", snd (head l) + 1):(tail l)
    | otherwise = ("Estrada", 1):l
    where
        l = listLinha ts
listLinha ((Relva, _):ts)
    | length (ts) > 0 && fst (head l) == "Relva" = ("Relva", snd (head l) + 1):(tail l)
    | otherwise = ("Relva", 1):l
    where
        l = listLinha ts

mapaValido :: Mapa -> Bool
mapaValido (Mapa _ []) = True
mapaValido (Mapa l t)
    | terrenoValido t == False = False
    | riosOpostos t == False = False
    | obstaculosValidos t == False = False
    | nenhumValidos t == False = False
    | linhaValidas (Mapa l t) == False = False
    | 0 < sum[1 | ("Rio", x) <-list,maxRios < x] = False
    | 0 < sum[1 | ("Estrada", x) <-list,maxEstradas < x] = False
    | 0 < sum[1 | ("Relva", x) <-list,maxRelvas < x] = False
    | otherwise = True 
    where
        list = listLinha t




