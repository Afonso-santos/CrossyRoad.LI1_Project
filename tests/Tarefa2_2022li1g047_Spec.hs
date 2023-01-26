module Tarefa2_2022li1g047_Spec where
import Tarefa1_2022li1g047
import LI12223
import Tarefa2_2022li1g047
import Test.HUnit


-- Verifica se o mapa criado é valido
t1 = Mapa 12 m1 -- pode ser qualquer terreno 
t2 = Mapa 12 m2 -- 5 estradas seguidas logoo proximo tem de ser rio ou relva , para ser valido
t3 = Mapa 12 m3 --5 relvas seguidas logo proximo tem de ser estrada ou rio ,para ser valido 
t4 = Mapa 12 m4  -- 4 rios seguidos logo prixmo tem de ser estrda ou relva ,para ser valido 


testsT2 :: Test
testsT2 = test [
        "Testes Tarefa 2 t1" ~:True ~=? mapaValido (estendeMapa t1 44 ),
        "Testes Tarefa 2 t2" ~:False ~=? mapaValido (estendeMapa t2 23 ),
        "Testes Tarefa 2 t3" ~:False~=? mapaValido (estendeMapa t3 89),
        "Testes Tarefa 2 t4" ~:True ~=? mapaValido (estendeMapa t4 9 )        
                ]



m1=[(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore]),
    (Estrada 2,[Carro,Carro,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro]),
    (Estrada 4,[Nenhum,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum]),
    (Rio 2 ,[Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco,Nenhum]),
    (Rio (-3),[Tronco,Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Nenhum])]

m2=[(Estrada 2,[Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum]),
    (Estrada 3,[Carro,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum,Carro,Carro,Nenhum,Carro,Carro]),
    (Estrada (-1),[Nenhum,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum,Carro,Carro]),
    (Estrada (-4),[Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum]),
    (Estrada 1 ,[Nenhum,Carro,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum,Carro,Nenhum])]
        
m3=[(Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum]),
    (Relva,[Arvore,Nenhum,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore]),
    (Relva,[Arvore,Arvore,Nenhum,Arvore,Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore]),
    (Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum]),
    (Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore]),
    (Estrada 2 ,[Carro,Carro,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro])]

m4=[(Rio 2, [Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco,Nenhum]),
    (Rio (-3),[Tronco,Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Nenhum]),
    (Rio 2, [Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco,Nenhum]),
    (Rio (-3),[Tronco,Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Nenhum]),
    (Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore])]