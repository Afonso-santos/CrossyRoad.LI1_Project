{- |
Module      : Tarefa2_2022li1g047
Description : Geração contínua de um mapa
Copyright   : Afonso Dionísio Santos <a104276@alunos.uminho.pt>
              Luis Enrique Diaz de Freitas <a104000@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g047 where

import LI12223 
import Tarefa1_2022li1g047
import System.Random
import GHC.Base 
import Data.List.NonEmpty (group, group1)
import Tarefa1_2022li1g047 (tamanhoCheck)
import LI12223 (Obstaculo, Velocidade)
import Data.Bool (Bool (True, False))
import Data.Foldable (Foldable(length))
import Data.List
import GHC.Base (absentErr)

{-|1.A função estendeMapa, definida por funções auxiliares, que irá defenir aleatoriamente o terreno da nova linhas. A mesma adiciona uma velocidades aos devidos terrenos.A função estendeMapa só é possivel devido à função estendeLinha que nesta função irá defenir a lista de obstáculos aleatória.

@
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa l ((terreno,o):t)) n = Mapa l ((estendeLinha l speed ( newTerreno ,[]) ):(terreno,o):t) 
  where listaTerreno= proximosTerrenosValidos (Mapa l ((terreno,o):t))
        newTerreno= listaTerreno !! randomInt (length listaTerreno) n
        speed = case terreno of 
          Rio v ->  v*(-1)
          Estrada v -> (randomInt n 40 )
          Relva -> (randomInt n 33 )
@

==Exemplo 
>>> estendeMapa (Mapa 5 [(Rio 1,[Tronco,Tronco,Nenhum,Nenhum,Tronco]),
                        (Relva, [Nenhum,Arvore,Nenhum,Nenhum,Nenhum])]) 12


Mapa 5 [(Estrada (-1),[Nenhum,Nenhum,Carro,Carro,Carro]),
                      (Rio 1,[Tronco,Tronco,Nenhum,Nenhum,Tronco]),
                      (Relva,[Nenhum,Arvore,Nenhum,Nenhum,Nenhum])]                  

3. A função "proximosTerrenosValidos" e "prosimosObstaculosValidos" são funções na qual indicam o quais são os casos possiveis dependendo da situação.

@
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ [])= [Rio 0 ,Estrada 0 ,Relva]
proximosTerrenosValidos (Mapa _ (h:(Rio _,_):(Rio _,_):(Rio _,_):(Rio _,_):t))= [Estrada 0,Relva]
proximosTerrenosValidos (Mapa _(h:(Estrada _,_):(Estrada _,_):(Estrada _,_):(Estrada _,_):(Estrada _,_):t)) =[ Relva,Rio 0]
proximosTerrenosValidos (Mapa _(h:(Relva,_):(Relva,_):(Relva,_):(Relva,_):(Relva,_):t))= [Rio 0,Estrada 0]
proximosTerrenosValidos (Mapa _ (h:t))=[Rio 0, Relva, Estrada 0]
@

@
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos x (Relva,[])= [Nenhum,Arvore]
proximosObstaculosValidos x (Estrada _,[])= [Nenhum,Carro]
proximosObstaculosValidos x (Rio _ , [])= [Nenhum , Tronco] 
proximosObstaculosValidos x (Rio _ , o)
  |x == length o = []
  |(x - length o ) == 1 && elem Nenhum o && liObts o==False  =[Nenhum]
  |(x - length o ) == 1 && elem Nenhum o == False && liObts o ==True = [Tronco]
  |(x-length o) > 1 && elem Nenhum o   && liObts o == True = [ Tronco, Nenhum]
  |(x-length o) > 1 && elem Nenhum o   && liObts o == False = [Nenhum]
  |(x-length o) >=1 && elem Nenhum o && liObts o  ==True = [Nenhum, Tronco]
  |(x-length o) >=1 && elem Nenhum o && liObts o == False =[Nenhum]
  |otherwise = [Nenhum]
proximosObstaculosValidos x (Estrada _ , o)
  |x == length o = []
  |(x - length o ) == 1 && elem Nenhum o && liObts (o)==False  =[Nenhum]
  |(x-length o) > 1 && elem Nenhum o == False  && liObts o == True = [Nenhum, Carro]
  |(x-length o) > 1 && elem Nenhum o == False  && liObts o == False = [Nenhum]
  |(x-length o) >=1 && elem Nenhum o && liObts o  ==True = [Carro, Nenhum]
  |(x-length o) >=1 && elem Nenhum o && liObts o == False = [Nenhum]
  |otherwise = [Nenhum]
proximosObstaculosValidos x (Relva, o)
  |x == length o = []
  |(x - length o ) == 1 && elem Nenhum o  =[Nenhum]
  |(x-length o) > 1 && elem Nenhum o == False  = [Nenhum,Arvore]
  |(x-length o) >=1 && elem Nenhum o  = [Nenhum,Arvore]
  |otherwise = [Nenhum]
@

4. A razão para aleatoridade das funções referidas anteriormente são:

@
randomList :: (Random a) => Int --^ Um inteiro que será a seed das crições dos "randoms" 
          -> [a]  --^A lista de inteiros dos "randoms" 
randomList seed = randoms (mkStdGen seed)

randomInt:: Int->Int->Int
randomInt  y x = mod ((randomList x)!! x) y
@

-}





estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa l (m@(terreno,o):t)) n = Mapa l (((estendeValido l (m) (estendeLinha l speed ( newTerreno ,[]) ))):((terreno,o):t)) 

  where listaTerreno= proximosTerrenosValidos (Mapa l ((terreno,o):t))
        newTerreno= listaTerreno !! randomInt (length listaTerreno) n
        estendeValido ::Int-> (Terreno,[Obstaculo])-> (Terreno,[Obstaculo]) -> (Terreno,[Obstaculo])
        estendeValido la (Relva,l) (Relva,l1)= if (elem (Nenhum,Nenhum) (zip l l1) ) {- == True -}then (Relva,l1) else estendeLinha la  speed (newTerreno,[])
        estendeValido  la _  (t,l) = (t,l)
        speed = case terreno of 
          Rio v ->  v*(-1)
          Estrada v -> (randomInt n 40)
          Relva -> (randomInt n 33 )







randomList :: (Random a) => Int -> [a]
randomList seed = randoms (mkStdGen seed)

randomInt:: Int->Int->Int
randomInt  y x = mod ((randomList x)!! x) y
          

estendeLinha ::Int ->Velocidade-> (Terreno, [Obstaculo]) -> (Terreno, [Obstaculo])
estendeLinha l s (terreno,o) 
  | l== length o = (terreno,o)  
estendeLinha l s (Relva,o)= estendeLinha l s (Relva, obsataculo:o )

   where listaobstaculos= proximosObstaculosValidos l (Relva,o) 
         obsataculo = listaobstaculos !! randomInt (length listaobstaculos) l

estendeLinha l s (Rio v ,o)=estendeLinha  l s (Rio s , obsataculo:o)  
   
   where listaobstaculos= proximosObstaculosValidos l (Rio v ,o)
         obsataculo = listaobstaculos !! randomInt (length listaobstaculos) l
  

estendeLinha l s (Estrada v,o)=estendeLinha l s (Estrada s , obsataculo: o) 
                            
   where listaobstaculos= proximosObstaculosValidos l (Estrada v ,o)
         obsataculo = listaobstaculos !! randomInt (length listaobstaculos) l
         



proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ [])= [Rio 0 ,Estrada 0 ,Relva]
proximosTerrenosValidos (Mapa _ (h:(Rio _,_):(Rio _,_):(Rio _,_):(Rio _,_):t))= [Estrada 0,Relva]
proximosTerrenosValidos (Mapa _(h:(Estrada _,_):(Estrada _,_):(Estrada _,_):(Estrada _,_):(Estrada _,_):t)) =[ Relva,Rio 0]
proximosTerrenosValidos (Mapa _(h:(Relva,_):(Relva,_):(Relva,_):(Relva,_):(Relva,_):t))= [Rio 0,Estrada 0]
proximosTerrenosValidos (Mapa _ (h:t))=[Rio 0, Relva, Estrada 0]


proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos x (Relva,[])= [Nenhum,Arvore]
proximosObstaculosValidos x (Estrada _,[])= [Nenhum,Carro]
proximosObstaculosValidos x (Rio _ , [])= [Tronco] 
proximosObstaculosValidos x (Rio _ , o)
  |x == length o = []
  |(x - length o ) == 1 && elem Nenhum o == False  && liObts o==False  =[Nenhum]
  |(x-length o) > 1 && elem Nenhum o ==False  && liObts o == True = [ Tronco, Nenhum]
  |(x-length o) > 1 && elem Nenhum o == False  && liObts o == False = [Nenhum]
  |otherwise = [Nenhum]
proximosObstaculosValidos x (Estrada _ , o)
  |x == length o = []
  |(x - length o ) == 1 && elem Nenhum o == False  && liObts o ==False  =[Nenhum]
  |(x-length o) > 1 && elem Nenhum o == False  && liObts o == True = [Nenhum, Carro]
  |(x-length o) > 1 && elem Nenhum o == False  && liObts o == False = [Nenhum]
  |otherwise = [Nenhum]
proximosObstaculosValidos x (Relva, o)
  |x == length o = []
  |(x - length o ) == 1 && elem Nenhum o == False =[Nenhum]
  |(x-length o) > 1 && elem Nenhum o == False  = [Nenhum,Arvore]
  |otherwise = [Nenhum]


liObts :: [Obstaculo]->Bool
liObts []=True
liObts l= tamanhoObts (groupa l)

groupa :: Eq a => [a] -> [[a]]
groupa [] = []
groupa (h:t) = (h:takeWhile (== h) t) : groupa (dropWhile (== h) t)

tamanhoObts::[[Obstaculo]]->Bool
tamanhoObts [[]]=True
tamanhoObts l
  | last (last l) == Tronco && (length (last l) >=5) =False
  | last (last l) == Carro && (length (last l) >=3) =False
  |otherwise = True 



