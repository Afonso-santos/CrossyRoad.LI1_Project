{- |
Module      : Tarefa6_2022li1g047
Description : Movimentação do personagem e obstáculos
Copyright   : Afonso Dionísio Santos <a104276@alunos.uminho.pt>
              Luis Enrique Diaz de Freitas <a104000@alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/23.
-}
--import Language.Haskell.TH (javaScript)
import Data.Char
import Control.Monad
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
--import Graphics.Gloss.Interface.Pure.Game
-- Handle, hIsEOF, openFile, ReadMode, WriteMode, hGetLine, hPutLine, hClose
import System.IO
-- doesFileExist
import System.Directory
-- exitSuccess
import System.Exit
import LI12223

{-|A função animaJogo é responsável por interligar todas as funcões e fazer o jogo funcionar 

@
playJogo :: JogoX -> IO ()
playJogo jogox = do
    playIO
        (creaDisplay initDisplay) 
        fundo  
        fps
        jogox
        pintaJogo
        reageEvento
        actJogo

main = do
    img <- carregarImg
    let jogox = insertaImg initJogoXI img
    playJogo jogox        
@

-|Para a fluidez do movimento as funções de o reageEvento e o  actJogo são constituidas pelas funções adptadastar das tarefas anteriores. 
-|A Função actJogo  atualizaa cotaçao dos pontos que está defenida em todos os casos possiveis onde os pontos aumentam através do tempo de jogo ,(p+dt).
@
 if jogoTerminouX (Jogo (Jogador (xj', yj)) (Mapa l tos'), (xPx', yPx'), v, dtva, m, dta', dya', p + dt, e, img) then
                                Menu Repetir
@
-|O jogo tem uma opção de guardar o jogo que guarda o estado do jogo e guarda e o recorde do jogador  
@
salvarJogoX :: JogoX -> IO JogoX
salvarJogoX (jogo, c, v, dtva, m, dta, dya, p, e, img) = do
    saved <- openFile "cr-saved.txt" WriteMode
    hPutStrLn saved (show (jogo, c, v, dtva, m, dta, dya, p, e))
    hClose saved
    trocaEstadoIO Salvado (jogo, c, v, dtva, m, dta, dya, p, e, img)

salveRecord :: Float -> IO ()
salveRecord r = do
    config <- openFile "cr-config.txt" WriteMode
    hPutStrLn config (show r)
    hClose config
    return ()
@      
-|Quando o jogador quiser recomeçar o jogo salvado estas são as funções utilizadas

@
lerFloat :: String -> IO Float
lerFloat = readIO

lerInt :: String -> IO Int
lerInt = readIO

lerCoordenadas :: String -> IO Coordenadas
lerCoordenadas = readIO


lerCoordPx :: String -> IO CoordPx
lerCoordPx = readIO


lerJogada :: String -> IO Jogada
lerJogada = readIO


lerTOb :: String -> IO (Terreno, [Obstaculo])
lerTOb = readIO

lerJogoXI :: String -> IO JogoXI
lerJogoXI = readIO

cargarJogoX :: IO JogoX
cargarJogoX = do
    saved <- openFile "cr-saved.txt" ReadMode
    temp <- hGetLine saved
    jogoxi <- lerJogoXI temp
    hClose saved
    img <- carregarImg    
    trocaEstadoIO (Menu Continuar) (insertaImg jogoxi img)
@
-|A função CargarTObs ,primeiro verifica se o arquivo foi finalizado se não tiver baixa o texto da linha, que representa uma linha do jogo, e depois adiciona essa linha ao final da lista de mapas e processamos a linha e devolvemos um mapa
@
cargarTObs :: TObs -> Handle -> IO TObs
cargarTObs tos s = do
     noTOb <- hIsEOF s
    if not noTOb then do
        temp <- hGetLine s
        to <- lerTOb temp
        cargarTObs (tos ++ [to]) s
    else
        return (tos)
@
-| Criamos um manipulador de arquivos ,que lê e converte os dados do record guardando o e devolve-nos o record 
@
recordIO :: IO Float
recordIO = do
    config <- openFile "cr-config.txt" ReadMode
    temp <- hGetLine config
    recordF <- lerFloat temp
    hClose config
    return recordF

-- | Função que lê um valor String em um arquivo de texto e o converte em um tipo JogoX, sem imagens.
@
lerJogoXI :: String -> IO JogoXI
lerJogoXI = readIO

cargarJogoX :: IO JogoX
cargarJogoX = do
    saved <- openFile "cr-saved.txt" ReadMode
    temp <- hGetLine saved
    jogoxi <- lerJogoXI temp
    hClose saved
    img <- carregarImg    
    trocaEstadoIO (Menu Continuar) (insertaImg jogoxi img)
@

-}

























----------------------------------------------------------------------------------------------------------
-- DEFINIÇÕES
----------------------------------------------------------------------------------------------------------

-- | Velocidade com que o jogador se move horizontalmente ao interagir com um tronco.
type VelXJogador = Float

-- | Tempo de movimento horizontal ao interagir com um tronco.
type TempoVelX = Float

-- | Tipo de variável onde é armazenada a diferencial de tempo acumulado (tick) para calcular a posição do obstáculo móvel.
type ObsdtAcum = Float

type Deslizamento  = Float

-- | Tipo de variável onde é armazenada a pontuação alcançada no jogo.
type Pontos = Float

-- | Opções nos menus.
data Opcao = Jogar | Carregar | Saida | Repetir | Volta | Continuar | Salvar | Cancelar deriving (Show, Read, Eq)

-- | Estados do jogo.
data Estado = Menu Opcao | Ativado | Finalizado | Salvado deriving (Show, Read, Eq)

-- | Lista das imagens.
type Imagens = [[Picture]]

-- | Tipo de objeto que representa o mundo do jogo.
type JogoX = 
    (Jogo,       -- ^ Jogo = Jogo Jogador Mapa, definição original contida no arquivo1
    CoordPx,     -- ^ CoordPx: (xPx, yPx), Coordenadas atuais em pixels do jogador na janela
    VelXJogador, -- ^ VelXJogador: Velocidad sobre tronco
    TempoVelX,   -- ^ TempoVelX: Acelere o tempo de aplicação em X no tronco
    Jogada,      -- ^ Jogada: Jogada em andamento
    ObsdtAcum,   -- ^ ObsdtAcum: Diferencial de tempo (ticks) que, multiplicado pela velocidade,define o deslocamento de um obstáculo em pixels    Ex: posição inicial + ObsdtAcum * velocidade = localização final
    Deslizamento,-- ^ Deslizamento: Diferencial acumulado de pixels para o cálculo do deslizamento do mapa para baixo
    Pontos,      -- ^ Pontos: Pontuação acumulada durante o jogo, resultado do tempo acumulado (ticks) com o jogador vivo
    Estado,      -- ^ Estado: Indica a interface ativa do jogo
    Imagens)     -- ^ Imagens: Lista das imagens


-- | Tipo de objeto que representa o mundo do jogo, sem imagens.
type JogoXI = (Jogo, CoordPx, VelXJogador, TempoVelX, Jogada, ObsdtAcum, Deslizamento, Pontos, Estado)

-- | Modos visuais disponíveis para a janela do jogo.
data ModoDisplay = Window | Full deriving (Eq)

-- | Coordenadas em pixels dos objetos do jogo.
type CoordPx = (Float, Float)

-- | Objeto contendo um conjunto de terrenos e seus obstáculos.
type TObs = [(Terreno, [Obstaculo])]

----------------------------------------------------------------------------------------------------------
-- VALORES INICIAIS
----------------------------------------------------------------------------------------------------------

maxTroncos = 5
maxCarros = 3
maxRios = 4
maxEstradas = 5
maxRelvas = 5
mapaAlta = 11
maxTerrenos = 11

-- Comprimento horizontal e vertical em pixels de cada caixa do mapa
undObs = 60.0

altoJanelaPx = 720 :: Float

-- Índice del último terreno del juego
maxIndiceTerrenos = round (altoJanelaPx / undObs) - 1 -- 12

maxIndiceObs = 16 :: Int

comJanelaPx = fromIntegral (maxIndiceObs + 1) * undObs -- 1020

initJogador = Jogador (div maxIndiceObs 2, maxIndiceTerrenos)

origXPx :: Float
origXPx = - 1 * comJanelaPx / 2 -- - 510

origYPx :: Float
origYPx = - 1 * altoJanelaPx / 2 -- - 360

-- Função que converte as coordenadas do jogo para as coordenadas da janela
transfPosPx :: Coordenadas -> Deslizamento -> CoordPx
transfPosPx (xj, yj) dya = (xPx, yPx)
    where 
        xPx = origXPx + (fromIntegral xj) * undObs + undObs / 2
        yPx = origYPx + undObs / 2 + fromIntegral (maxIndiceTerrenos - yj) * altoTerrenoPx - dya + 1

-- Função que converte as coordenadas X em pixels da tela em coordenadas X do jogo
transfxPxXJ :: Float -> Int
transfxPxXJ xPx = floor ((xPx - origXPx) / undObs)

--initJogoXI :: JogoXI
initJogoXI = (Jogo initJogador (Mapa maxIndiceObs [(Relva, replicate (maxIndiceObs + 1) Nenhum)]), transfPosPx (extraeCoord initJogador) 0, 0, 0, Parado, 0, 0, 0, Menu Jogar)

fps :: Int
fps = 10

-- Módulo de velocidade do jogador, Px / Tick
mvj :: Float
mvj = 20 -- 2 * undObs / fromIntegral fps

-- Pixels que um obstáculo de velocidade 1 move por tick
fo :: Float
fo =  1 -- mvj / 2 

-- Ticks de tempo limite entre os deslizamentos no mapa
ticksDes = 50.0

altoTerrenoPx = undObs

extraeComMapa :: Jogo -> Int
extraeComMapa (Jogo _ (Mapa l _)) = l

initDisplay :: ModoDisplay
initDisplay = Full --Window

fundo :: Color
fundo = greyN 0.50

brown = makeColorI 102 51 0 255

marco = pictures [  translate (origXPx - undObs / 2) 0 marcosV, 
                    translate (origXPx + comJanelaPx + undObs / 2) 0 marcosV, 
                    translate 0 (origYPx + altoJanelaPx + undObs / 2) marcosH, 
                    translate 0 (origYPx - undObs / 2) marcosH]
    where
        marcosV = color fundo (rectangleSolid undObs (altoJanelaPx + 2* undObs))
        marcosH = color fundo (rectangleSolid comJanelaPx undObs)

fundoMenu = color (makeColorI 255 255 255 200) (rectangleSolid comJanelaPx altoJanelaPx)

idImgJogador = 0
idImgTerrenos = 1
idImgObstaculos = 2
idImgMenus = 3
idImgMensagems = 4

espacoMenu = -90

----------------------------------------------------------------------------------------------------------
-- LENDO DE ARQUIVOS
----------------------------------------------------------------------------------------------------------

-- | Função que lê um valor String em um arquivo de texto e o converte em um tipo Float.
lerFloat :: String -> IO Float
lerFloat = readIO

-- | Função que lê um valor String em um arquivo de texto e o converte em um tipo Int.
lerInt :: String -> IO Int
lerInt = readIO

-- | Função que lê um valor String em um arquivo de texto e o converte em um tipo Coordenadas.
lerCoordenadas :: String -> IO Coordenadas
lerCoordenadas = readIO

-- | Função que lê um valor String em um arquivo de texto e o converte em um tipo CoordPx.
lerCoordPx :: String -> IO CoordPx
lerCoordPx = readIO

-- | Função que lê um valor String em um arquivo de texto e o converte em um tipo Jogada.
lerJogada :: String -> IO Jogada
lerJogada = readIO

-- | Função que lê um valor String em um arquivo de texto e o converte em um tipo (Terreno, [Obstaculo]).
lerTOb :: String -> IO (Terreno, [Obstaculo])
lerTOb = readIO

cargarTObs :: TObs -> Handle -> IO TObs
cargarTObs tos s = do
    -- Verificamos se o arquivo foi finalizado
    noTOb <- hIsEOF s
    -- Caso não seja feito, baixamos o texto da linha, que representa um tipo (Terreno, [Obstaculo])
    if not noTOb then do
        temp <- hGetLine s
        to <- lerTOb temp
        -- Adicionamos o tipo de dados (Terrain, [Obstaculo]) ao final da lista de mapas e processamos a seguinte linha do arquivo
        cargarTObs (tos ++ [to]) s
    else
        -- Devolvemos a lista completa de terrenos e obstáculos
        return (tos)

recordIO :: IO Float
recordIO = do
    -- Criamos o manipulador de arquivos
    config <- openFile "cr-config.txt" ReadMode
    -- Lemos e convertemos os dados do record
    temp <- hGetLine config
    recordF <- lerFloat temp
    -- Fechamos o arquivo
    hClose config
    -- Devolvemos o record
    return recordF

-- | Função que lê um valor String em um arquivo de texto e o converte em um tipo JogoX, sem imagens.
lerJogoXI :: String -> IO JogoXI
lerJogoXI = readIO

cargarJogoX :: IO JogoX
cargarJogoX = do
    saved <- openFile "cr-saved.txt" ReadMode
    temp <- hGetLine saved
    jogoxi <- lerJogoXI temp
    hClose saved
    img <- carregarImg    
    trocaEstadoIO (Menu Continuar) (insertaImg jogoxi img)

carregarImgPer :: IO [Picture]
carregarImgPer = do 
    i1 <- loadBMP "andaru.bmp"
    i2 <- loadBMP "andard.bmp"
    i3 <- loadBMP "andarr.bmp"
    i4 <- loadBMP "andarl.bmp"
    return ([i1,i2,i3,i4])

carregarImgTer :: IO [Picture]
carregarImgTer = do
    t1 <- loadBMP "Rio60x60.bmp"
    t2 <- loadBMP "Estrada60x60.bmp"
    t3 <- loadBMP "Relva60x60.bmp"
    return ([t1,t2,t3])

carregarImgObs :: IO [Picture]
carregarImgObs = do 
    o1 <- loadBMP "Tronco60x60.bmp"
    o2 <- loadBMP "Carro60x60E.bmp"
    o3 <- loadBMP "Carro60x60D.bmp"
    o4 <- loadBMP "Arvore60x60.bmp"
    return ([o1,o2,o3,o4])

carregarImgMenu :: IO [Picture]
carregarImgMenu = do 
    i00 <- loadBMP "MJogar.bmp"
    i01 <- loadBMP "MCarregar.bmp"
    i02 <- loadBMP "MSaida.bmp"
    i03 <- loadBMP "MContinuar.bmp"
    i04 <- loadBMP "MSalvar.bmp"
    i05 <- loadBMP "MCancelar.bmp"
    i06 <- loadBMP "MRepetir.bmp"
    i07 <- loadBMP "MVolta.bmp"
    i08 <- loadBMP "MJogarA.bmp"
    i09 <- loadBMP "MCarregarA.bmp"
    i10 <- loadBMP "MSaidaA.bmp"
    i11 <- loadBMP "MContinuarA.bmp"
    i12 <- loadBMP "MSalvarA.bmp"
    i13 <- loadBMP "MCancelarA.bmp"
    i14 <- loadBMP "MRepetirA.bmp"
    i15 <- loadBMP "MVoltaA.bmp"
    return ([i00,i01,i02,i03,i04,i05,i06,i07,i08,i09,i10,i11,i12,i13,i14,i15])
    
carregarImgMensagem :: IO [Picture]
carregarImgMensagem = do 
    i1 <- loadBMP "Mensagem01.bmp"
    i2 <- loadBMP "Mensagem02.bmp"
    i3 <- loadBMP "Mensagem03.bmp"
    i4 <- loadBMP "Mensagem04.bmp"
    i5 <- loadBMP "Mensagem05.bmp"
    i6 <- loadBMP "Mensagem06.bmp"
    return ([i1,i2,i3,i4,i5,i6])

carregarImg :: IO Imagens
carregarImg = do
    imgP <- carregarImgPer
    imgT <- carregarImgTer
    imgO <- carregarImgObs
    imgMN <- carregarImgMenu
    imgMG <- carregarImgMensagem
    return ([imgP, imgT, imgO, imgMN, imgMG])

----------------------------------------------------------------------------------------------------------
-- ESCRITA DE ARQUIVOS
----------------------------------------------------------------------------------------------------------

salvarJogoX :: JogoX -> IO JogoX
salvarJogoX (jogo, c, v, dtva, m, dta, dya, p, e, img) = do
    saved <- openFile "cr-saved.txt" WriteMode
    hPutStrLn saved (show (jogo, c, v, dtva, m, dta, dya, p, e))
    hClose saved
    trocaEstadoIO Salvado (jogo, c, v, dtva, m, dta, dya, p, e, img)

salveRecord :: Float -> IO ()
salveRecord r = do
    config <- openFile "cr-config.txt" WriteMode
    hPutStrLn config (show r)
    hClose config
    return ()
        
----------------------------------------------------------------------------------------------------------
-- FUNÇÕES DE EXTRAÇÃO E CONVERSÃO
----------------------------------------------------------------------------------------------------------

extraeTObs :: JogoX -> TObs
extraeTObs (Jogo _ (Mapa _ tos),_,_,_,_,_,_,_,_,_) = tos

tipoTerreno :: Terreno -> String
tipoTerreno (Rio _) = "Rio"
tipoTerreno (Estrada _) = "Estrada"
tipoTerreno Relva = "Relva"

terrenoVel :: Terreno -> Int
terrenoVel (Rio v) = v
terrenoVel (Estrada v) = v
terrenoVel _ = 0

extraeTerreno :: (Terreno, [Obstaculo]) -> Terreno
extraeTerreno to = fst to

extraeTerrenos :: JogoX -> [(Terreno, [Obstaculo])]
extraeTerrenos ((Jogo _ (Mapa _ tos)),_,_,_,_,_,_,_,_,_) = tos

tipoTerrenoYS :: Int -> JogoX -> String
tipoTerrenoYS y ((Jogo _ (Mapa _ tos)),_,_,_,_,_,_,_,_,_) = tipoTerreno (fst (tos !! y))

extraeVelF :: Terreno -> Float
extraeVelF (Estrada v) = fromIntegral v
extraeVelF (Rio v) = fromIntegral v
extraeVelF (Relva) = 0

extraeVelYF :: Int -> JogoX -> Float
extraeVelYF y ((Jogo _ (Mapa _ tos)),_,_,_,_,_,_,_,_,_) = extraeVelF (fst (tos !! y))

extraeObstaculoXY :: (Int, Int) -> Jogo -> Obstaculo
extraeObstaculoXY (x,y) (Jogo _ (Mapa _ tos)) = (snd (tos !! y)) !! x

-- Função que converte coordenadas na matriz do mapa em coordenadas em pixels na janela
extraeCoord :: Jogador -> Coordenadas
extraeCoord (Jogador (x,y)) = (x,y)

-- | Função para modificar a velocidade de um terreno.
modVel :: Terreno -> Velocidade -> Terreno
modVel (Estrada _) v = Estrada v
modVel (Rio _) v = Rio v
modVel Relva _ = Relva

insertaImg :: JogoXI -> Imagens -> JogoX
insertaImg (j, co, v, dtva, m, dta, dya, p, e) img = (j, co, v, dtva, m, dta, dya, p, e, img)

----------------------------------------------------------------------------------------------------------
-- FUNÇÕES DE MODIFICAÇÃO DO MAPA
----------------------------------------------------------------------------------------------------------

-- | Função de inteiros aleatórios entre zero e n
nRand :: Int -> IO Int
nRand i = do
    n <- randomRIO(0,i::Int)
    return n

listLinha :: [(Terreno, a)] -> [(String, Int)]
listLinha [] = []
listLinha ((Rio _, _):tos)
    | length (tos) > 0 && fst (head l) == "Rio" = ("Rio", snd (head l) + 1):(tail l)
    | otherwise = ("Rio", 1):l
    where
        l = listLinha tos
listLinha ((Estrada _, _):tos)
    | length (tos) > 0 && fst (head l) == "Estrada" = ("Estrada", snd (head l) + 1):(tail l)
    | otherwise = ("Estrada", 1):l
    where
        l = listLinha tos
listLinha ((Relva, _):tos)
    | length (tos) > 0 && fst (head l) == "Relva" = ("Relva", snd (head l) + 1):(tail l)
    | otherwise = ("Relva", 1):l
    where
        l = listLinha tos

-- | Função que cria uma lista de terrenos válidos que podem ser adicionados ao início da lista de terrenos do mapa
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ tos)
    | head list == ("Rio", maxRios) = [Estrada 0, Relva]
    | head list == ("Estrada", maxEstradas) = [Rio 0, Relva]
    | head list == ("Relva", maxRelvas) = [Estrada 0, Rio 0]
    | otherwise = [Rio 0, Estrada 0, Relva]
    where list = listLinha tos

-- groupObst [Tronco,Tronco,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Tronco] = [[Tronco,Tronco,Tronco],[],[Tronco,Tronco],[],[],[Tronco]]
-- | Função que cria uma lista de listas de obstáculos contíguos em um terreno.
groupObst :: [Obstaculo] -> [[Obstaculo]]
groupObst [Nenhum] = [[]]
groupObst (h:t)
    | 1 < length l && h == Nenhum = []:g
    | 1 < length l && head g == [] = [h]:g
    | 1 < length l && h == head (head g) = [h : head g] ++ tail g
    | otherwise = [[h]]
    where 
        l = h:t
        g = groupObst t

-- compObst [Tronco,Tronco,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Tronco] = [3,0,2,0,0,1]
-- | Função que cria uma lista de comprimentos de conjuntos de obstáculos em um terreno.
compObst :: [Obstaculo] -> [Int]
compObst os = [length comp | comp <- groupObst os]

-- | Função que cria uma lista de obstáculos válidos que podem ser adicionados a um TObs incompleto.
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos _ (Rio _, []) = [Nenhum, Tronco]
proximosObstaculosValidos _ (Estrada _, []) = [Nenhum, Carro]
proximosObstaculosValidos _ (Relva, []) = [Nenhum, Arvore]
proximosObstaculosValidos l (Rio _, os)
    | l == length os = []
    | last list == maxTroncos = [Nenhum]
    | l - length os == 1 && not (elem Nenhum os) = [Nenhum]
    | l - length os == 1 && maxTroncos < head list + last list + 1 = [Nenhum]
    | otherwise = [Nenhum,Tronco]
    where 
        list = compObst os

proximosObstaculosValidos l (Estrada _, os)
    | l == length os = []
    | last list == maxCarros = [Nenhum]
    | l - length os == 1 && not (elem Nenhum os) = [Nenhum]
    | l - length os == 1 && maxCarros < head list + last list + 1 = [Nenhum]
    | otherwise = [Nenhum,Carro]
    where 
        list = compObst os

proximosObstaculosValidos l (Relva, o)
    | l == length o = []
    | l - length o == 1 && not (elem Nenhum o) = [Nenhum]
    | otherwise = [Nenhum, Arvore]

-- | Função que adiciona obstáculos válidos a um terreno até que ele seja concluído.
insertaObs :: Int -> (Terreno, [Obstaculo]) -> IO (Terreno, [Obstaculo])
insertaObs l (t, os) = do
    if length os < l then do 
        let po1 = proximosObstaculosValidos l (t,os)
            po2 =   if t == Relva then
                po1 ++ [Nenhum]
            else
                po1
        n <- nRand (length po2 -1)
        insertaObs l (t, os ++ [po2 !! n])
    else
        return (t, os)

-- | Função que adiciona um novo terreno válido com obstáculos ao início da lista de mapa.
estendeMapa :: JogoX -> IO JogoX
estendeMapa (Jogo j mapa, c, v, dtva, m, dta, dya, p, e, img) = do
    let jogox = (Jogo j mapa, c, v, dtva, m, dta, dya, p, e, img) 
        tValidos = proximosTerrenosValidos mapa
    nt <- nRand (length tValidos - 1)
    nv1 <- nRand (7 - fst (properFraction(p / 1000))) -- caso quiera cambiModoar la velocidad altero el 7 por totaldevelocidad - 1
    -- Selecionamos o novo terreno
    let nv =    if fst (properFraction(p / 1000)) < 8 then
                    nv1 + fst (properFraction(p / 1000))
                else
                    7 -- totaldevelocidad - 1 
        t = tValidos !! nt
        --ns = 0      
    ns <- nRand 1
    -- Selecionamos o sinal da velocidade
    let s = case t of
                Rio 0     -> case tipoTerrenoYS 0 jogox of
                                "Rio"     -> (-1 * round (signum (extraeVelYF 0 jogox)))
                                otherwise -> [(-1), 1] !! ns
                Estrada 0 -> [(-1), 1] !! ns
                Relva     -> 0
        -- Selecionamos a velocidade
        vo :: Int
        vo = case t of
                Estrada 0 -> ([1, 2, 3, 4, 5, 6, 10 ,12] !! nv) * s -- todas las velocidades psibles que sena divisores de 60 ( xq es el largo deun obstaculo)
                Relva     -> 0
                Rio 0     -> ([1, 2, 3, 4, 5, 6, 10 ,12] !! nv) * s -- todas las velocidades psibles que sena divisores de 60 ( xq es el largo deun obstaculo)
    -- Selecionamos todos os obstáculos do novo terreno
    to <- insertaObs (maxIndiceObs + 1) (modVel t vo, [])
    -- Devolvemos o mundo com o terreno adicionado ao início
    return (Jogo j (Mapa (maxIndiceObs + 1) ([to] ++ (extraeTObs jogox))), c, v, dtva, m, dta, dya, p, e, img)

-- | Criar um novo JogoX (mundo).
creaJogoX :: JogoX -> IO JogoX
creaJogoX jogox = do
    jogox2 <- estendeMapa jogox
    -- Verificamos se o mundo tem toda a terrenos que deveria. Se faltar um terreno, adicionamos mais um. Se estiver completo, devolvemos o mundo
    if length (extraeTerrenos jogox2) < maxIndiceTerrenos + 1 then
        creaJogoX jogox2
    else
        return jogox2

----------------------------------------------------------------------------------------------------------
-- FUNÇÕES DE CONTROLE
----------------------------------------------------------------------------------------------------------

-- | Função que realoca um obstáculo na matriz do mapa assim que o obstáculo terminar de se mover de um bloco para outro no mapa.
actObs :: Float -> TObs -> TObs
actObs _ [] = []    
actObs dta (to:tos)
    | act && v < 0 = [(t, (drop 1 os) ++ [head os])] ++ (actObs dta tos)
    | act && v > 0 = [(t, (last os) : (init os))] ++ (actObs dta tos)
    | otherwise = to : (actObs dta tos)
    where
        t = fst to
        v = extraeVelF t
        os = snd to
        --ORIGINALact = snd (properFraction (dta * v / undObs)) == 0
        --act = snd (properFraction ((dta * v + signum(v) * undObs / 2) / undObs)) == 0
        --act = fromIntegral (fst (properFraction (2 * (dta - 1) / undObs * v))) /= fromIntegral (fst (properFraction (2 * dta / undObs * v)))
        act = fst (properFraction ((v * (dta - 1) + signum (v) * undObs / 2) / undObs)) /= fst (properFraction ((v * dta + signum (v) * undObs / 2) / undObs))

-- | Função que determina se o jogador morreu e o jogo acabou, con JogoX.
jogoTerminouX :: JogoX -> Bool
jogoTerminouX (Jogo (Jogador (xj, yj)) (Mapa l tos),_, v, dtva,_,_,_,_, e,_)
    | dtva == 0 && e == Finalizado = True
    | tipoTerreno (fst (tos !! yj)) == "Estrada" && (snd (tos !! yj)) !! xj == Carro = True
    | xj < 0 || l <= xj || maxIndiceTerrenos < yj = True
    | otherwise = False

----------------------------------------------------------------------------------------------------------
-- FUNÇÕES DE PINTURA
----------------------------------------------------------------------------------------------------------

-- | Funcao que dado um terreno devolve a imagem correspondente ao terreno
imgTerreno :: Terreno -> [Picture] -> Picture
imgTerreno t img =
    let segmento =  case t of
                        (Rio _)     -> img !! 0
                        (Estrada _) -> img !! 1
                        Relva       -> img !! 2
    in pictures [translate (origXPx + undObs / 2 + fromIntegral i * undObs) 0 segmento | i <- [0..maxIndiceObs]]

-- | Função que cria a lista de imagens dos terrenos vazios.
listPTerrenos :: Int -> Float -> TObs -> [Picture] -> [Picture]
listPTerrenos _ _ [] _ = []
listPTerrenos n dya tos img = (listPTerrenos (n + 1) dya (init tos) img) ++ [translate xt yt (imgTerreno t img)] 
    where   
        t = fst (last tos)
        xt = origXPx + comJanelaPx / 2
        yt = origYPx + altoTerrenoPx / 2 + fromIntegral n * undObs - dya

-- | Função que cria a imagem do conjunto de terrenos vazios.
pintaTerrenos :: Float -> TObs -> Imagens -> Picture
pintaTerrenos dya tos img = pictures (listPTerrenos 0 dya tos (img !! idImgTerrenos))

-- | Função que cria a imagem de um obstáculo.
pintaObs :: Coordenadas -> ObsdtAcum -> Deslizamento -> (Terreno, [Obstaculo]) -> Imagens -> Picture
pintaObs (xj, yj) dta dya to img
    | obs == Nenhum = pictures []
    -- Pintura de obstáculos que saem pela esquerda do mapa e continuam pela direita
    | xPx' - undObs / 2 < origXPx = pictures [translate xPx' yPx' p, translate (xPx' + comJanelaPx) yPx' p]
    -- Pintura de obstáculos que saem pela direita do mapa e continuam pela esquerda
    | origXPx + comJanelaPx < xPx' + undObs / 2 = pictures [translate (xPx' - comJanelaPx) yPx' p, translate xPx' yPx' p]
    -- Pintura de obstáculos que estão completamente dentro do mapa
    | otherwise = pictures [translate xPx' yPx' p]
    where
        obs = (snd to) !! xj
        v = if obs /= Nenhum then
                extraeVelF (fst to)
            else
                0
        (xPx1, yPx) =   if obs /= Nenhum then
                            transfPosPx (xj, yj) dya
                        else
                            (0,0)
        yPx' =  if obs /= Nenhum then
                    if dya == 0 then
                        yPx
                    else
                        yPx + undObs
                else
                    0
        -- Ajustamos a coordenada X em pixels para velocidade e subtraímos o erro para atualizar a posição na matriz
        xPx' =  if obs /= Nenhum then
                    xPx1 + v * dta - fromIntegral (fst (properFraction ((v * dta + signum(v) * undObs / 2) / undObs))) * undObs
                else
                    0
        p = case obs of
                Tronco      ->  ((img !! idImgObstaculos) !! 0)
                Carro       ->  if extraeVelF (fst to) <0 then
                                    ((img !! idImgObstaculos) !! 1)
                                else
                                    ((img !! idImgObstaculos) !! 2)
                Arvore      ->  ((img !! idImgObstaculos) !! 3)
                otherwise   ->  pictures []

-- | Função que cria a imagem do conjunto de obstáculos.
pintaObss :: ObsdtAcum -> Deslizamento -> TObs -> Imagens -> Picture
pintaObss _ _ [] _ = pictures []
pintaObss dta dya tos img
    | 1 < length (snd to) = pictures ([pintaObss dta dya (init tos ++ [(extraeTerreno to, init (snd to))]) img] ++ [pintaObs (xj, yj) dta dya to img])
    | 1 == length (snd to) = pictures ([pintaObss dta dya (init tos) img] ++ [pintaObs (xj, yj) dta dya to img])
    where 
        to = last tos
        (xj, yj) = (length (snd (to)) - 1, length tos - 1)

-- | Função que cria a imagem do texto com escala, coordenadas e cor determinada.
pintaTexto :: Float -> CoordPx -> Color -> String -> Picture
pintaTexto s (xPx, yPx) c t = translate xPx yPx $ scale s s $ color c $ text t

-- | Função que cria a imagem os pontos enquanto joga.
pintaPontosF :: Float -> Picture
pintaPontosF p =
    let x = origXPx + comJanelaPx
        y = origYPx + altoJanelaPx
        p1 = translate (x + undObs * 1.5 + 20) (y - undObs) (color white (rectangleSolid (undObs * 3 - 2) (undObs * 2 - 2)))
        p2 = pintaTexto 0.2 (x + 40, y - 30) green "Pontos:"
        p3 = pintaTexto 0.2 (x + 40, y - 100) green (show (round(p)))
        p4 = translate (x + undObs * 1.5 + 20) (y - undObs) (rectangleWire (undObs * 3) (undObs * 2))
    in pictures [p1, p2, p3, p4]


-- | Função que cria a imagem os pontos enquanto joga.
pintaNivel :: Float -> Picture
pintaNivel p =
    let x = origXPx + comJanelaPx
        y = origYPx + altoJanelaPx
        p1 = translate (x + undObs * 1.5 + 20) (y - undObs * 5) (color white (rectangleSolid (undObs * 3 - 2) (undObs * 2 - 2)))
        p2 = pintaTexto 0.2 (x + 40, y - 30 - undObs * 4) green "Nivel:"
        p3 = pintaTexto 0.2 (x + 40, y - 100 - undObs * 4) green (show (ceiling(p / 1000)))
        p4 = translate (x + undObs * 1.5 + 20) (y - undObs * 5) (rectangleWire (undObs * 3) (undObs * 2))
    in pictures [p1, p2, p3, p4]

-- | Função que cria a imagem do jogador.
pintaJogador :: CoordPx -> Deslizamento -> Estado -> Jogada -> Imagens -> Picture
pintaJogador (xPx, yPx) dya e m img
    | e == Menu Repetir || e == Menu Volta = translate xPx yPx $ scale 0.075 0.075 $ i
    | otherwise = translate xPx yPx $ scale 0.075 0.075 $ i
    where
        yPx' =  if dya == 0 then
                    yPx
                else
                    yPx + undObs
        i = case m of
                Move Cima       -> (img !! idImgJogador) !! 0
                Move Baixo      -> (img !! idImgJogador) !! 1
                Move Direita    -> (img !! idImgJogador) !! 2
                Move Esquerda   -> (img !! idImgJogador) !! 3
                Parado          -> (img !! idImgJogador) !! 0

-- | Função que pinta apenas as partes do jogo que devem deslizar com o mapa.
pintaFundoJogoDeslizante :: JogoX -> IO Picture
pintaFundoJogoDeslizante ((Jogo (Jogador (xj, yj)) (Mapa l tos)), c, v, dtva, m, dta, dya, p, e, img) = do
    let p1 = pintaTerrenos dya tos img
        p2 = pintaObss dta dya tos img
        p3 = pintaJogador c dya e m img
        plist = [p1, p2, p3]
    return (pictures plist)

-- Função que pinta todas as partes do jogo quando ativo.
pintaFundoJogoCompleto :: JogoX -> IO Picture
pintaFundoJogoCompleto ((Jogo j (Mapa l tos)), c, v, dtva, m, dta, dya, p, e, img) = do
    p1 <- pintaFundoJogoDeslizante ((Jogo j (Mapa l tos)), c, v, dtva, m, dta, dya, p, e, img)
    let p2 = pintaPontosF p
        p3 = pintaNivel p
        plist = [p1, marco, p2, p3]
    return (pictures plist)

-- | Função que cria as imagens das interfaces dos diferentes menus.
pintaMenu :: Estado -> Imagens -> IO Picture
pintaMenu e img = do
    logo <- loadBMP "Logo.bmp"
    let m01 = [translate 0 200 $ scale 1 1 $ logo]
        m02 = []
        colorAtivo = blue
        colorDesativo = black 
        menu = case e of
            Menu Jogar      ->  let m1 = m01 ++ [translate 0 (espacoMenu * 0)  ((img !! idImgMenus) !! (0 + 8))]
                                    m2 = m1 ++  [translate 0 (espacoMenu * 1)  ((img !! idImgMenus) !! (1 + 0))]
                                    m3 = m2 ++  [translate 0 (espacoMenu * 2)  ((img !! idImgMenus) !! (2 + 0))]
                                in m3    
            Menu Carregar   ->  let m1 = m01 ++ [translate 0 (espacoMenu * 0)  ((img !! idImgMenus) !! (0 + 0))]
                                    m2 = m1 ++  [translate 0 (espacoMenu * 1)  ((img !! idImgMenus) !! (1 + 8))]
                                    m3 = m2 ++  [translate 0 (espacoMenu * 2)  ((img !! idImgMenus) !! (2 + 0))]
                                in m3    
            Menu Saida      ->  let m1 = m01 ++ [translate 0 (espacoMenu * 0)  ((img !! idImgMenus) !! (0 + 0))]
                                    m2 = m1 ++  [translate 0 (espacoMenu * 1)  ((img !! idImgMenus) !! (1 + 0))]
                                    m3 = m2 ++  [translate 0 (espacoMenu * 2)  ((img !! idImgMenus) !! (2 + 8))]
                                in m3    

            Menu Continuar  ->  let m1 = m02 ++ [translate 0 (espacoMenu * 0)  ((img !! idImgMenus) !! (3 + 8))]
                                    m2 = m1 ++  [translate 0 (espacoMenu * 1)  ((img !! idImgMenus) !! (4 + 0))]
                                    m3 = m2 ++  [translate 0 (espacoMenu * 2)  ((img !! idImgMenus) !! (5 + 0))]
                                in m3    
            Menu Salvar     ->  let m1 = m02 ++ [translate 0 (espacoMenu * 0)  ((img !! idImgMenus) !! (3 + 0))]
                                    m2 = m1 ++  [translate 0 (espacoMenu * 1)  ((img !! idImgMenus) !! (4 + 8))]
                                    m3 = m2 ++  [translate 0 (espacoMenu * 2)  ((img !! idImgMenus) !! (5 + 0))]
                                in m3    
            Salvado         ->  let m1 = m02 ++ [translate 0 (espacoMenu * 0)  ((img !! idImgMenus) !! (3 + 0))]
                                    m2 = m1 ++  [translate 0 (espacoMenu * 1)  ((img !! idImgMenus) !! (4 + 8))]
                                    m3 = m2 ++  [translate 0 (espacoMenu * 2)  ((img !! idImgMenus) !! (5 + 0))]
                                    m4 = m3 ++  [translate 0 (espacoMenu * (-1.25)) ((img !! idImgMensagems) !! 5)]
                                in m4    
            Menu Cancelar   ->  let m1 = m02 ++ [translate 0 (espacoMenu * 0)  ((img !! idImgMenus) !! (3 + 0))]
                                    m2 = m1 ++  [translate 0 (espacoMenu * 1)  ((img !! idImgMenus) !! (4 + 0))]
                                    m3 = m2 ++  [translate 0 (espacoMenu * 2)  ((img !! idImgMenus) !! (5 + 8))]
                                in m3    

            Menu Repetir    ->  let m1 = m02 ++ [translate 0 (espacoMenu * 0)  ((img !! idImgMenus) !! (6 + 8))]
                                    m2 = m1 ++  [translate 0 (espacoMenu * 1)  ((img !! idImgMenus) !! (7 + 0))]
                                    m3 = m2
                                in m3    
            Menu Volta      ->  let m1 = m02 ++ [translate 0 (espacoMenu * 0)  ((img !! idImgMenus) !! (6 + 0))]
                                    m2 = m1 ++  [translate 0 (espacoMenu * 1)  ((img !! idImgMenus) !! (7 + 8))]
                                    m3 = m2
                                in m3    
    return (pictures menu)

-- | Função que pinta as interfaces de jogo a pedido da função Gloss.Play
pintaJogo :: JogoX -> IO Picture
pintaJogo (jogo, c, v, dtva, m, dta, dya, p, Menu Jogar, img) = do
    menu <- pintaMenu (Menu Jogar) img
    return menu

pintaJogo (jogo, c, v, dtva, m, dta, dya, p, Menu Carregar, img) = do
    menu <- pintaMenu (Menu Carregar) img
    return menu

pintaJogo (jogo, c, v, dtva, m, dta, dya, p, Menu Saida, img) = do
    menu <- pintaMenu (Menu Saida) img
    return menu

pintaJogo ((Jogo (Jogador (xj, yj)) mapa), c, v, dtva, m, dta, dya, p, Menu Repetir, img) = do
    p1 <- pintaFundoJogoCompleto ((Jogo (Jogador (xj, yj)) mapa), c, v, dtva, m, dta, dya, p, Menu Repetir, img)
    menu <- pintaMenu (Menu Repetir) img
    arquivoExiste <- doesFileExist "cr-config.txt"
    recordF <- recordIO
    if recordF < p then do
        salveRecord p
    else
        return ()
    let jogox = ((Jogo (Jogador (xj, yj)) mapa), c, v, dtva, m, dta, dya, p, Menu Repetir, img)
        p2 =    if xj < 0 || maxIndiceObs< xj then
                    translate 0 (espacoMenu * (-2)) $ scale 0.6 0.6 $ ((img !! idImgMensagems) !! 1)
                else
                    if maxIndiceTerrenos < yj then
                        translate 0 (espacoMenu * (-2)) $ scale 0.6 0.6 $ ((img !! idImgMensagems) !! 2)
                    else
                        case tipoTerrenoYS yj jogox of
                            "Estrada"   -> translate 0 (espacoMenu * (-2)) $ scale 0.6 0.6 $ ((img !! idImgMensagems) !! 3)
                            "Rio"       -> translate 0 (espacoMenu * (-2)) $ scale 0.6 0.6 $ ((img !! idImgMensagems) !! 4)
        p3 =    if recordF < p then do
                    translate 0 (espacoMenu * (-1.25)) ((img !! idImgMensagems) !! 0)
                else
                    pictures []
    return (pictures [p1, fundoMenu, p2, p3, menu])

pintaJogo ((Jogo (Jogador (xj, yj)) mapa), c, v, dtva, m, dta, dya, p, Menu Volta, img) = do
    p1 <- pintaFundoJogoCompleto ((Jogo (Jogador (xj, yj)) mapa), c, v, dtva, m, dta, dya, p, Menu Volta, img)
    menu <- pintaMenu (Menu Volta) img
    let jogox = ((Jogo (Jogador (xj, yj)) mapa), c, v, dtva, m, dta, dya, p, Menu Volta, img)
        p2 =    if xj < 0 || maxIndiceObs< xj then
                    translate 0 (espacoMenu * (-2)) $ scale 0.6 0.6 $ ((img !! idImgMensagems) !! 1)
                else
                    if maxIndiceTerrenos < yj then
                        translate 0 (espacoMenu * (-2)) $ scale 0.6 0.6 $ ((img !! idImgMensagems) !! 2)
                    else
                        case tipoTerrenoYS yj jogox of
                            "Estrada"   -> translate 0 (espacoMenu * (-2)) $ scale 0.6 0.6 $ ((img !! idImgMensagems) !! 3)
                            "Rio"       -> translate 0 (espacoMenu * (-2)) $ scale 0.6 0.6 $ ((img !! idImgMensagems) !! 4)
    return (pictures [p1, fundoMenu, p2, menu])

pintaJogo (jogo, c, v, dtva, m, dta, dya, p, Menu Continuar, img) = do
    p1 <- pintaFundoJogoCompleto (jogo, c, v, dtva, m, dta, dya, p, Menu Continuar, img)
    menu <- pintaMenu (Menu Continuar) img
    return (pictures [p1, fundoMenu, menu])

pintaJogo (jogo, c, v, dtva, m, dta, dya, p, Salvado, img) = do
    p1 <- pintaFundoJogoCompleto (jogo, c, v, dtva, m, dta, dya, p, Salvado, img)
    menu <- pintaMenu (Salvado) img
    return (pictures [p1, fundoMenu, menu])

pintaJogo (jogo, c, v, dtva, m, dta, dya, p, Menu Salvar, img) = do
    p1 <- pintaFundoJogoCompleto (jogo, c, v, dtva, m, dta, dya, p, Menu Salvar, img)
    menu <- pintaMenu (Menu Salvar) img
    return (pictures [p1, fundoMenu, menu])

pintaJogo (jogo, c, v, dtva, m, dta, dya, p, Menu Cancelar, img) = do
    p1 <- pintaFundoJogoCompleto (jogo, c, v, dtva, m, dta, dya, p, Menu Cancelar, img)
    menu <- pintaMenu (Menu Cancelar) img
    return (pictures [p1, fundoMenu, menu])

pintaJogo jogox = do
    pintaFundoJogoCompleto jogox

----------------------------------------------------------------------------------------------------------
-- FUNÇÕES DE PLAY
----------------------------------------------------------------------------------------------------------

creaDisplay :: ModoDisplay -> Display
creaDisplay modo
    | modo == Window = InWindow "Crossy Road" (round comJanelaPx, round altoJanelaPx) (0,0)
    | otherwise = FullScreen

-- GESTÃO DE EVENTOS DO MENUS-----------------------------------------------------------------------------
extraeEstado :: JogoX -> Estado
extraeEstado (jogo, co, v, dtva, m, dta, dya, p, e, img) = e

-- | Função para modificar o atributo Estado o jogo.
trocaEstadoIO :: Estado -> JogoX -> IO JogoX
trocaEstadoIO e ((Jogo (Jogador (x, y)) mapa), co, v, dtva, m, dta, dya, t, _, img) = return ((Jogo (Jogador (x, y)) mapa), co, v, dtva, m, dta, dya, t, e, img)

-- | Função secundária que gerencia os eventos do jogo para cada quadro de animação em menus, versão para Gloss.PlayIO.
reageEventoMenu :: Event -> JogoX -> IO JogoX
reageEventoMenu (EventKey (Char 'w') Down _ _) jogox = do
    case extraeEstado jogox of
        Menu Jogar      -> trocaEstadoIO (Menu Saida) jogox
        Menu Carregar   -> trocaEstadoIO (Menu Jogar) jogox
        Menu Saida      -> trocaEstadoIO (Menu Carregar) jogox

        Menu Repetir    -> trocaEstadoIO (Menu Volta) jogox
        Menu Volta      -> trocaEstadoIO (Menu Repetir) jogox

        Menu Continuar  -> trocaEstadoIO (Menu Cancelar) jogox
        Salvado         -> trocaEstadoIO (Menu Continuar) jogox
        Menu Salvar     -> trocaEstadoIO (Menu Continuar) jogox
        Menu Cancelar   -> trocaEstadoIO (Menu Salvar) jogox

reageEventoMenu (EventKey (Char 's') Down _ _) jogox = do
    case extraeEstado jogox of
        Menu Jogar      -> trocaEstadoIO (Menu Carregar) jogox
        Menu Carregar   -> trocaEstadoIO (Menu Saida) jogox
        Menu Saida      -> trocaEstadoIO (Menu Jogar) jogox

        Menu Repetir    -> trocaEstadoIO (Menu Volta) jogox
        Menu Volta      -> trocaEstadoIO (Menu Repetir) jogox

        Menu Continuar  -> trocaEstadoIO (Menu Salvar) jogox
        Salvado         -> trocaEstadoIO (Menu Cancelar) jogox
        Menu Salvar     -> trocaEstadoIO (Menu Cancelar) jogox
        Menu Cancelar   -> trocaEstadoIO (Menu Continuar) jogox

reageEventoMenu (EventKey (SpecialKey KeySpace) Down _ _) jogox = do
    case extraeEstado jogox of
        Menu Jogar      ->  do
                                -- Começamos um novo jogo
                                img <- carregarImg
                                initjogox <- creaJogoX (insertaImg initJogoXI img)
                                trocaEstadoIO Ativado initjogox
        Menu Carregar   ->  do  
                                arquivoExiste <- doesFileExist "cr-saved.txt"
                                if arquivoExiste then do
                                    -- Carregamos o jogo salvo e vamos para a interface do jogo pausado
                                    initjogox <- cargarJogoX
                                    trocaEstadoIO (Menu Continuar) initjogox
                                else
                                    -- Resta avisar que não tem jogo salvo e voltará ao jogo anterior
                                    return jogox
        Menu Saida      ->  do
                                exitSuccess

        Menu Repetir    ->  do
                                -- Começamos um novo jogo
                                img <- carregarImg
                                initjogox <- creaJogoX (insertaImg initJogoXI img)
                                trocaEstadoIO Ativado initjogox
        Menu Volta      ->  trocaEstadoIO (Menu Jogar) jogox

        Menu Continuar  ->  trocaEstadoIO Ativado jogox
        Menu Salvar     ->  salvarJogoX jogox
        Menu Cancelar   ->  trocaEstadoIO (Menu Jogar) jogox

-- GESTÃO DE EVENTOS DO JOGADOR --------------------------------------------------------------------------
-- | Função que gerencia os eventos do jogo para cada quadro de animação, versão para Gloss.PlayIO.
reageEvento :: Event -> JogoX -> IO JogoX
reageEvento (EventKey (Char 'w') Down a b) ((Jogo (Jogador (x, y)) mapa), co, v, dtva, m, dta, dya, p, e, img)
    | e /= Ativado = reageEventoMenu (EventKey (Char 'w') Down a b) (jogo, co, v, dtva, m, dta, dya, p, e, img)
    | 0 < y && extraeObstaculoXY (x, y - 1) jogo /= Arvore && m == Parado = return ((Jogo (Jogador (x, y - 1)) mapa), co, v, dtva, Move Cima, dta, dya, p, e, img)
    | otherwise = return (jogo, co, v, dtva, m, dta, dya, p, e, img)
    where
        jogo = Jogo (Jogador (x, y)) mapa

reageEvento (EventKey (Char 's') Down a b) ((Jogo (Jogador (x, y)) mapa), co, v, dtva, m, dta, dya, p, e, img)
    | e /= Ativado = reageEventoMenu (EventKey (Char 's') Down a b) ((Jogo (Jogador (x, y)) mapa), co, v, dtva, m, dta, dya, p, e, img)
    | y < maxIndiceTerrenos && extraeObstaculoXY (x, y + 1) jogo /= Arvore && m == Parado = return ((Jogo (Jogador (x, y + 1)) mapa), co, v, dtva, Move Baixo, dta, dya, p, e, img)
    | otherwise = return (jogo, co, v, dtva, m, dta, dya, p, e, img)
    where
        jogo = Jogo (Jogador (x, y)) mapa

reageEvento (EventKey (Char 'a') Down _ _) ((Jogo (Jogador (x, y)) mapa), co, v, dtva, m, dta, dya, p, e, img)
    | 0 < x && extraeObstaculoXY (x - 1, y) jogo /= Arvore && m == Parado = return ((Jogo (Jogador (x, y)) mapa), co, v, dtva, Move Esquerda, dta, dya, p, e, img)
    | otherwise = return (jogo, co, v, dtva, m, dta, dya, p, e, img)
    where
        jogo = Jogo (Jogador (x, y)) mapa

reageEvento (EventKey (Char 'd') Down _ _) ((Jogo (Jogador (x, y)) mapa), co, v, dtva, m, dta, dya, p, e, img)
    | x < maxIndiceObs && extraeObstaculoXY (x + 1, y) jogo /= Arvore && m == Parado = return ((Jogo (Jogador (x, y)) mapa), co, v, dtva, Move Direita, dta, dya, p, e, img)
    | otherwise = return (jogo, co, v, dtva, m, dta, dya, p, e, img)
    where
        jogo = Jogo (Jogador (x, y)) mapa

reageEvento (EventKey (SpecialKey KeySpace) Down a b) (jogo, co, v, dtva, m, dta, dya, p, e, img)
    | e /= Ativado = reageEventoMenu (EventKey (SpecialKey KeySpace) Down a b) (jogo, co, v, dtva, m, dta, dya, p, e, img)
    | otherwise = return (jogo, co, v, dtva, m, dta, dya, p, Menu Continuar, img)

reageEvento (EventKey (SpecialKey KeyEsc) Down a b) (jogo, co, v, dtva, m, dta, dya, p, e, img)
    | e /= Ativado = reageEventoMenu (EventKey (SpecialKey KeySpace) Down a b) (jogo, co, v, dtva, m, dta, dya, p, e, img)
    | otherwise = return (jogo, co, v, dtva, m, dta, dya, p, Menu Continuar, img)

reageEvento (EventKey (SpecialKey KeyF4) Down alt _) (jogo, co, v, dtva, m, dta, dya, p, e, img) = do exitSuccess

reageEvento _ jogox = return jogox

-- ATUALIZAÇÃO DO JOGO -------------------------------------------------------------------------------
-- | Função que atualiza o jogo a cada quadro de animação, versão para Gloss.PlayIO.
actJogo :: Float -> JogoX -> IO JogoX
actJogo _ (Jogo (Jogador (xj, yj)) (Mapa l tos), (xPx, yPx), v, dtva, m, dta, dya, p, e, img) = do
    if e == Ativado || e == Finalizado then do
        let jogo = Jogo (Jogador (xj, yj)) (Mapa l tos)
            jogox = (jogo, (xPx, yPx), v, dtva, m, dta, dya, p, e, img)
            -- Usamos o fator de deslocamento do obstáculos como duração do tick
            dt = fo
            -- Obtemos o que aparentemente serão as coordenadas finais do jogador
            (xPxf1, yPxf) =  transfPosPx (xj, yj) dya
            -- Recalculamos o diferencial de tempo acumulado para ajustar o deslocamento dos obstáculos
            dta' =  case undObs <= dta + dt of
                        True ->  0
                        False -> dta + dt
            -- Atualizada a lista de terreno e posições de obstáculos com base o deslizamento do mapa
            tos' =  if dya == undObs then
                -- Terminado o slide, removemos o último terreno do mapa
                init (actObs dta' tos)
            else
                -- Atualizamos posições de obstáculos
                actObs dta' tos
            jogo' = Jogo (Jogador (xj, yj)) (Mapa l tos')
            -- Quando snd(properFraction((p + dt) / ticksDes)) == 0, inicia deslizamento do mapa
            -- Quando dy /= 0, o processo de deslizamento do mapa está em execução
            -- Quando dya == undObs, o processo de deslizamento do mapa terminou

            -- Recalculamos o diferencial de coordenadas em pixels em um tick para o deslizamento do mapa
            dy =    if snd (properFraction ((p + dt) / ticksDes)) == 0 || (0 < dya && dya < undObs) then
                        mvj -- * 1
                    else
                        0
            dya' =  if dy == 0 then
                        0
                    else
                        dya + dy
            -- Calculamos coordenadas y em pixels do jogador, incluindo efeito de deslizamento do mapa
            yPx' =  case m of
                        Move Cima   -> yPx + mvj - dy
                        Move Baixo  -> yPx - mvj - dy
                        otherwise   -> yPx - dy
            -- Calculo do tempo disponivel para fazer os movimentos do jogador 
            dtva0 = undObs / mvj
    
     {-  Existem 9 situações de movimento para avaliar e responder:
            Parado
                Rio (S1)
                Outras (S2)
            Movimento vertical
                Terra -> Rio
                    -> Tronco (S3)
                    -> Nenhum (S4)
                Rio -> Rio
                    -> Tronco (S5)
                    -> Nenhum (S6)
                Todos -> Terra (S7)
            Movimento horizontal
                Rio <-> Rio (S8)
                Terra <-> Terra (S9)
        
        Em cada situação, há 2 etapas de ação:
            1) Etapa 1: Determinação do terreno e obstáculo alvo, cálculo da velocidade
                        em X chegar e primeiro movimento (dtva1 == dtva0) (E1)
            2) Etapa 2: Execução dos deslocamentos restantes e cálculo da velocidade final em X do
                        jogador (dtva1 /= dtva0) (E2)
    -}
        if m == Parado then do
            if tipoTerrenoYS yj jogox == "Rio" then do -- (E1) (E2) (S1)
                let xPx' = xPx + v -- * 1
                    xj' =   transfxPxXJ xPx'
                let e' =    if jogoTerminouX (Jogo (Jogador (xj', yj)) (Mapa l tos'), (xPx', yPx'), v, dtva, m, dta', dya', p + dt, e, img) then
                                Menu Repetir
                            else
                                e
                -- Retornamos o mapa atual ou, se for necessário iniciar o processo de deslizamento, o mapa estendido
                if snd (properFraction ((p + dt) / ticksDes)) == 0 then do
                    estendeMapa (Jogo (Jogador (xj', yj + 1)) (Mapa l tos'), (xPx', yPx'), v, dtva, m, dta', dya', p + dt, e', img)
                else do
                    return (Jogo (Jogador (xj', yj)) (Mapa l tos'), (xPx', yPx'), v, dtva, m, dta', dya', p + dt, e', img)
            else do -- (E1) (E2) (S2)
                -- Retornamos o mapa atual ou, se for necessário iniciar o processo de deslizamento, o mapa estendido
                let e' =    if jogoTerminouX (Jogo (Jogador (xj, yj)) (Mapa l tos'), (xPx, yPx'), v, dtva, m, dta', dya', p + dt, e, img) then
                                Menu Repetir
                            else
                                e
                if snd (properFraction ((p + dt) / ticksDes)) == 0 then do
                    estendeMapa (Jogo (Jogador (xj, yj + 1)) (Mapa l tos'), (xPx, yPx'), v, dtva, m, dta', dya', p + dt, e', img)
                else do
                    return (Jogo (Jogador (xj, yj)) (Mapa l tos'), (xPx, yPx'), v, dtva, m, dta', dya', p + dt, e', img)
        else do 
            let dtva1 = if dtva == 0 then
                            dtva0
                        else
                            dtva
            if m == Move Cima || m == Move Baixo then do -- (E1) (S3) (S4) (S5) (S6) (S7)
                if dtva1 == dtva0 && tipoTerrenoYS yj jogox == "Rio" && v == 0 then do  -- (E1) (S3) (S4)
                    let v' =    if extraeObstaculoXY (xj, yj) jogo == Tronco then -- (E1) (S3)
                                    let velObs = extraeVelYF yj jogox
                                        xPxf2 = xPxf1 + velObs * (dta + dtva1) - fromIntegral (fst (properFraction ((velObs * dta + signum (velObs) * undObs / 2) / undObs))) * undObs
                                    in (xPxf2 - xPx) / dtva0
                                else -- (E1) (S4)
                                    0
                        dtva' = dtva1 - 1
                        xPx' = xPx + v' -- * 1
                        xj' = transfxPxXJ xPx'
                        e' =    if extraeObstaculoXY (xj, yj) jogo == Tronco then
                                    Ativado
                                else
                                    Finalizado                    
                    -- Retornamos o mapa atual ou, se for necessário iniciar o processo de deslizamento, o mapa estendido
                    if snd (properFraction ((p + dt) / ticksDes)) == 0 then do
                        estendeMapa (Jogo (Jogador (xj', yj + 1)) (Mapa l tos'), (xPx', yPx'), v', dtva', m, dta', dya', p + dt, e', img)
                    else do
                        return (Jogo (Jogador (xj', yj)) (Mapa l tos'), (xPx', yPx'), v', dtva', m, dta', dya', p + dt, e', img)
                else do
                    if dtva1 == dtva0 && tipoTerrenoYS yj jogox == "Rio" && v /= 0 then do  -- (E1) (S5) (S6)
                        let velObs = extraeVelYF yj jogox
                            -- Posicion final de posible tronco en la misma columna del jugador 
                            xPxf2 = xPxf1 + velObs * (dta + dtva1) - fromIntegral (fst (properFraction ((velObs * dta + signum(velObs) * undObs / 2) / undObs))) * undObs
                            (xPxfE, _) = transfPosPx (xj - 1, yj) dya'
                            -- Posicion de posible tronco a la izquierda de la columna del jugador (posicion inicial / 2 -> posicion cuando llegue)
                            xPxE = xPxfE + velObs * dta - fromIntegral (fst (properFraction ((velObs * dta + signum(velObs) * undObs / 2) / undObs))) * undObs
                            xPxfE2 = xPxfE + velObs * (dta + dtva1) - fromIntegral (fst (properFraction ((velObs * dta + signum(velObs) * undObs / 2) / undObs))) * undObs
                            (xPxfD, _) = transfPosPx (xj + 1, yj) dya'
                            -- Posicion de posible tronco a la derecha de la columna del jugador (posicion inicial / 2 -> posicion cuando llegue)
                            xPxD = xPxfD + velObs * dta - fromIntegral (fst (properFraction ((velObs * dta + signum(velObs) * undObs / 2) / undObs))) * undObs
                            xPxfD2 = xPxfD + velObs * (dta + dtva1) - fromIntegral (fst (properFraction ((velObs * dta + signum(velObs) * undObs / 2) / undObs))) * undObs
                            v' =    if extraeObstaculoXY (xj, yj) jogo == Tronco then -- (E1) (S5) Verificamos tronco en la misma vertical
                                        (xPxf2 - xPx) / dtva0
                                    else
                                        if extraeObstaculoXY (xj - 1, yj) jogo == Tronco && abs(xPxE - xPx) <= undObs / 2 then -- (E1) (S5) Verificamos tronco a la izquierda
                                            (xPxfE2 - xPx) / dtva0
                                        else
                                            if extraeObstaculoXY (xj + 1, yj) jogo == Tronco  && abs(xPxD - xPx) <= undObs / 2 then -- (E1) (S5) Verificamos tronco a la derecha
                                                (xPxfD2 - xPx) / dtva0
                                            else -- (E1) (S6)
                                                0
                        let dtva' = dtva1 - 1
                            xPx' = xPx + v' -- * 1
                            xj' = transfxPxXJ xPx'
                            e' =    if extraeObstaculoXY (xj, yj) jogo == Tronco then do -- (E1) (S5) -- Verificamos tronco na mesma vertical
                                        Ativado
                                    else
                                        if (extraeObstaculoXY (xj - 1, yj) jogo == Tronco && abs(xPxE - xPx) <= undObs / 2) || (extraeObstaculoXY (xj + 1, yj) jogo == Tronco && abs(xPxD - xPx) <= undObs / 2) then
                                            Ativado
                                        else
                                            Finalizado
                        -- Retornamos o mapa atual ou, se for necessário iniciar o processo de deslizamento, o mapa estendido
                        if snd (properFraction ((p + dt) / ticksDes)) == 0 then do
                            estendeMapa (Jogo (Jogador (xj', yj + 1)) (Mapa l tos'), (xPx', yPx'), v', dtva', m, dta', dya', p + dt, e', img)
                        else do
                            return (Jogo (Jogador (xj', yj)) (Mapa l tos'), (xPx', yPx'), v', dtva', m, dta', dya', p + dt, e', img)
                    else do
                        if dtva1 == dtva0 then do -- (E1) (S7)
                            let v' = (xPxf1 - xPx) / dtva0
                                dtva' = dtva1 - 1
                                xPx' = xPx + v' -- * 1
                                xj' =   transfxPxXJ xPx' --v'
                           -- Retornamos o mapa atual ou, se for necessário iniciar o processo de deslizamento, o mapa estendido
                            if snd (properFraction ((p + dt) / ticksDes)) == 0 then do
                                estendeMapa (Jogo (Jogador (xj', yj + 1)) (Mapa l tos'), (xPx', yPx'), v', dtva', m, dta', dya', p + dt, e, img)
                            else do
                                return (Jogo (Jogador (xj', yj)) (Mapa l tos'), (xPx', yPx'), v', dtva', m, dta', dya', p + dt, e, img)
                        else do -- (E2) (S3) (S4) (S5) (S6) (S7)
                            let dtva' = dtva1 - 1
                                xPx' = xPx + v -- * 1
                                velObs = extraeVelYF yj jogox
                                xj' = transfxPxXJ xPx'
                                v' =    if dtva' == 0 then
                                            if extraeObstaculoXY (xj', yj) jogo' == Tronco && e /= Finalizado then
                                                extraeVelYF yj jogox
                                            else
                                                0
                                        else
                                            v
                                m' =    if dtva' == 0 then
                                            Parado
                                        else
                                            m
                                e' =    if jogoTerminouX (Jogo (Jogador (xj', yj)) (Mapa l tos'), (xPx', yPx'), v', dtva', m', dta', dya', p + dt, e, img) then
                                            Menu Repetir
                                        else
                                            e
                            -- Retornamos o mapa atual ou, se for necessário iniciar o processo de deslizamento, o mapa estendido
                            if snd (properFraction ((p + dt) / ticksDes)) == 0 then do
                                estendeMapa (Jogo (Jogador (xj', yj + 1)) (Mapa l tos'), (xPx', yPx'), v', dtva', m', dta', dya', p + dt, e', img)
                            else do
                                return (Jogo (Jogador (xj', yj)) (Mapa l tos'), (xPx', yPx'), v', dtva', m', dta', dya', p + dt, e', img)
            else do -- (E1) (E2) (S8) (S9)
                if dtva1 == dtva0 && tipoTerrenoYS yj jogox == "Rio" then do -- (E1) (S8)
                    let velObs = extraeVelYF yj jogox
                        e' =    case m of
                                    Move Esquerda   ->  if extraeObstaculoXY (xj - 1, yj) jogo /= Tronco then
                                                            Finalizado
                                                        else
                                                            e
                                    Move Direita    ->  if extraeObstaculoXY (xj + 1, yj) jogo /= Tronco then
                                                            Finalizado
                                                        else
                                                            e
                        v' =    case m of
                                    Move Esquerda   ->  if e' == Finalizado then
                                                            let (xPxf2, _) =  transfPosPx (xj - 1, yj) dya
                                                            in (xPxf2 - xPx) / dtva0
                                                        else
                                                            -1 * mvj + velObs
                                    Move Direita    ->  if e' == Finalizado then
                                                            let (xPxf2, _) =  transfPosPx (xj + 1, yj) dya
                                                            in (xPxf2 - xPx) / dtva0
                                                        else
                                                            mvj + velObs
                        dtva' = dtva1 - 1
                        xPx' = xPx + v' -- * 1
                        xj' = transfxPxXJ xPx' --v'
                        xPxf2 = case m of
                                    Move Esquerda   ->  xPx - undObs
                                    Move Direita    ->  xPx + undObs
                    -- Retornamos o mapa atual ou, se for necessário iniciar o processo de deslizamento, o mapa estendido
                    if snd (properFraction ((p + dt) / ticksDes)) == 0 then do
                        estendeMapa (Jogo (Jogador (xj', yj + 1)) (Mapa l tos'), (xPx', yPx'), v', dtva', m, dta', dya', p + dt, e', img)
                    else do
                        return (Jogo (Jogador (xj', yj)) (Mapa l tos'), (xPx', yPx'), v', dtva', m, dta', dya', p + dt, e', img)
                else do
                    if dtva1 == dtva0 && tipoTerrenoYS yj jogox /= "Rio" then do -- (E1) (S9)
                        let v' =    case m of
                                        Move Esquerda   -> -1 * mvj
                                        Move Direita    -> mvj
                            dtva' = dtva1 - 1
                            xPx' = xPx + v' -- * 1
                            xj' = transfxPxXJ xPx'
                        -- Retornamos o mapa atual ou, se for necessário iniciar o processo de deslizamento, o mapa estendido
                        if snd (properFraction ((p + dt) / ticksDes)) == 0 then do
                            estendeMapa (Jogo (Jogador (xj', yj + 1)) (Mapa l tos'), (xPx', yPx'), v', dtva', m, dta', dya', p + dt, e, img)
                        else do
                            return (Jogo (Jogador (xj', yj)) (Mapa l tos'), (xPx', yPx'), v', dtva', m, dta', dya', p + dt, e, img)
                    else do -- (E2) (S8) (S9)
                        let dtva' = dtva1 - 1
                            xPx' = xPx + v
                            xj' =   transfxPxXJ xPx'
                            m' =    if dtva' == 0 then
                                        Parado
                                    else
                                        m
                            e' =    if jogoTerminouX (Jogo (Jogador (xj', yj)) (Mapa l tos'), (xPx', yPx'), v', dtva', m', dta', dya', p + dt, e, img) then
                                        Menu Repetir
                                    else
                                        e
                            v' =    if dtva' == 0 then
                                        if e' == Finalizado then
                                            0
                                        else
                                            extraeVelYF yj jogox
                                    else
                                        v
                        -- Retornamos o mapa atual ou, se for necessário iniciar o processo de deslizamento, o mapa estendido
                        if snd (properFraction ((p + dt) / ticksDes)) == 0 then do
                            estendeMapa (Jogo (Jogador (xj', yj + 1)) (Mapa l tos'), (xPx', yPx'), v', dtva', m', dta', dya', p + dt, e', img)
                        else do
                            return (Jogo (Jogador (xj', yj)) (Mapa l tos'), (xPx', yPx'), v', dtva', m', dta', dya', p + dt, e', img)
    else do
        return (Jogo (Jogador (xj, yj)) (Mapa l tos), (xPx, yPx), v, dtva, m, dta, dya, p, e, img)

-- | Função playJogo, versão Gloss.PlayIO.
playJogo :: JogoX -> IO ()
playJogo jogox = do
    playIO
        (creaDisplay initDisplay) -- ^ Função que queria a janela
        fundo                     -- ^ Cor de fundo
        fps                       -- ^ Os frames por segundo 
        jogox                     -- ^ O mundo inicial 
        pintaJogo                 -- ^ Desenha o jogoX
        reageEvento               -- ^ Função que reage aos evento 
        actJogo                   -- ^ Aletra a o desenha pelo o tempo 

main = do                         -- ^ Faz o download das imagens
    img <- carregarImg
    let jogox = insertaImg initJogoXI img
    playJogo jogox