------------------------------------------
-- PRÁCTICA DE HASKELL                  --
-- Cuatro en raya                       --
-- Nombre: Oliver Urones García         --
-- Asignatura: Lenguajes de Programación--
-- Curso: 2º de G.I.I.S.I               --
------------------------------------------

import System.Environment

-- Función para convertir un String a un Int
atoi::String->Int
atoi = read


-- Función que crea el tablero inicial
-- @parámetros: m n -> dimensiones del tablero
-- @devuelve: Una lista de listas que será el tablero
crea_tablero :: Int -> Int -> [[Int]]
crea_tablero m n =  replicate m (replicate n 0)

-- Función que realiza una jugada
-- Se le pasa el tablero, el turno del jugador la fila y la columna.
-- @parámetros: tablero turno fila columna ->
-- @devuleve: Una lista de listas que será el tablero con la nueva jugada hecha
jugada :: [[Int]] -> Int ->  Int -> Int -> [[Int]]
jugada tablero turno fila columna = lAnterior++lActual++lPosterior
    where maxX = busca_fila tablero fila columna
          lAnterior = take (maxX-1) tablero
          lActual = [pon_ficha (last (take (maxX) tablero)) columna turno 0]
          lPosterior = drop (maxX) tablero

-- Función que realiza una jugada para el ordenador
-- Se le pasa las dimensiones del tablero (n,m), el tablero el turno y la columna
-- @parámetros: n m tablero turno columna
-- @devuelve: Una lista de listas con el tablero con la jugada hecha por el ordenador
jugada_ordenador :: Int -> Int -> [[Int]] -> Int -> Int -> [[Int]]
jugada_ordenador n m tablero turno columna = if hay_cuatro_en_raya t1 n m turno x0 columna then t1
                                             else if length coordenadas >=2 || length coordenadas == 0 then t1 else t2
    where x0 = busca_fila tablero 0 columna
          t1 = jugada tablero turno 0 columna
          coordenadas = comprueba_posiciones n m tablero
          t2 = jugada tablero turno (fst (head coordenadas)) (snd (head coordenadas))

-- Función que genera una lista de pares [(1,-1),(2,-2),...,((n `div` 2),-(n `div` 2))]
-- Esta lista servirá para que el ordenador eliga una columna más al centro usando los valores de la lista como índices para las columnas
-- @parámetros n -> Anchura del tablero
-- @devuelve: Una lista de pares
genera_direcciones :: Int -> [(Int, Int)]
genera_direcciones n = zip [1..c] (reverse [-c..(-1)])
                where c = n `div` 2

-- Función que convierte la lista de pares devuelta por la función genera_direcciones en una lista
-- [(1,-1),(2,-2),...,((n `div` 2),-(n `div` 2))] --> [1,-1,2,-2,...,(n `div` 2,-(n `div` 2))]
-- @parámetros: (x:xs) -> Lista de pares
-- @devuelve: Una lista
genera_lista_direcciones :: [(Int, Int)] -> [Int]
genera_lista_direcciones [(x,y)] = [x,y]
genera_lista_direcciones (x:xs) = [x1,x2]++genera_lista_direcciones xs
    where (x1,x2) = x

-- Función que elige la columna más al medio donde se puede poner una ficha
-- Se le pasa el tablero, la altura, la columna y la lista devuelta por genera_lista_direcciones
-- @parámetros: tablero m columna (x:xs)
-- @devuelve: El índice de la columna encontrada
columna_mas_al_centro :: [[Int]] -> Int -> Int -> [Int] -> Int
columna_mas_al_centro tablero m columna (x:xs) = if fila>=0 && fila<m && igualacero then (columna+x) else columna_mas_al_centro tablero m columna xs
    where fila = (busca_fila tablero 0 (columna+x))-1
          igualacero = if tablero!!fila!!(columna+x) == 0 then True else False

-- Función que pone un 1 o un 2 en el tablero en función del jugador que le toque jugar
-- Se le pasa la lista correspondiente a la fila donde va la ficha, la columna el turno del jugador y un índice para recorrer la lista
-- @parámetros: (x:xs) columna turno n
-- @devuelve: La lista con la ficha colocada en la posición correspondiente
pon_ficha :: [Int] -> Int -> Int -> Int -> [Int]
pon_ficha [x] columna turno n = if n == columna then [turno] else [x]
pon_ficha (x:xs) columna turno n = if n == columna then turno:xs else x:pon_ficha xs columna turno (n+1)

-- Función que busca la máxima fila donde se puede poner una ficha en una columna
-- Se le pasa el tablero, la fila y la columna
-- @parámetros: tablero fila columna
-- @devuelve: La fila máxima donde se puede poner una ficha en la columna indicada
busca_fila :: [[Int]] -> Int -> Int -> Int
busca_fila tablero fila columna = if fila < length tablero && tablero!!fila!!columna == 0
                                  then busca_fila tablero (fila+1) columna
                                  else fila

-- Función que genera las coordenadas donde el jugador contrario puede poner una ficha
-- Se le pasa las dimensiones del tablero y el tablero
-- @parámetros: n m tablero
-- @devuelve: Las coordenadas del tablero donde el contrario puede poner una ficha
posiciones_contrario :: Int -> Int -> [[Int]] -> [(Int, Int)]
posiciones_contrario n m tablero = [(x,y) | x<-[0..m-1], y<-[0..n-1], tablero!!x!!y ==0]

-- Función que devuelve una lista con las coordenadas donde el contrario contrario hace 4 en raya
-- Si en esta lista hay 2 o más elementos el ordenador pierde
-- Se le pasa las dimensiones del tablero y el tablero
-- @parámetros: n m tablero
-- @devuelve: Una lista de coordenadas
comprueba_posiciones :: Int -> Int -> [[Int]] -> [(Int, Int)]
comprueba_posiciones n m tablero = [c | c<-posiciones_contrario n m tablero, hay_cuatro_en_raya (jugada tablero 1 (fst c) (snd c)) n m 1 (fst c) (snd c)]


-- Función para empezar la partida
partida :: [[Int]] -> Int -> Int -> Int -> Int -> IO (Int)
-- Se le pasa las dimensiones del tablero el turno y un tercer parámetros que si es 3 indicará que se juega contra el ordenador, si es 2 indicará que los jugadores son humanos
-- En esta función se piden por pantalla los datos necesarios al jugador para realizar una jugada
-- @parámetros: tablero n m turno 3
-- @devuelve: El turno del jugador que gana

-- Función para jugar con el ordenador
partida tablero n m turno 3 = do
                                if turno == 2 then
                                    do
                                        print ("Ordenador")
                                        let columna = (n `div` 2)   -- Crear función para elegir bien la columna
                                        let columna = columna_mas_al_centro tablero m (n `div` 2) (0:genera_lista_direcciones (genera_direcciones n))
                                        let tordenador = jugada_ordenador n m tablero 2 columna
                                        --print ("Tablero: "++show tordenador)
                                        let x0 = (busca_fila tordenador 0 columna)
                                        if hay_cuatro_en_raya tordenador n m turno x0 columna
                                        then 
                                            return turno
                                        else
                                            if comprueba_empate tordenador then
                                                return 0
                                            else
                                                do
                                                    let tur = cambia_turno turno
                                                    partida tordenador n m tur 3
                                else
                                    do
                                        print ("Jugador "++show turno)
                                        print ("Tablero "++show tablero)
                                        print ("Elija una columna ")
                                        c<-getLine
                                        let columna = atoi c
                                        if columna >= n then
                                            partida tablero n m turno 3
                                        else
                                            do
                                                print ("Columna: "++show columna)
                                                let t = jugada tablero turno 0 columna
                                                print ("Tablero: "++show t)
                                                let x0 = (busca_fila t 0 columna)
                                                --let lHorizontal = coge_horizontales t n m turno x0 columna 0 1
                                                --if length lHorizontal >= 4 && todos_iguales turno lHorizontal
                                                if hay_cuatro_en_raya t n m turno x0 columna
                                                then 
                                                    return turno
                                                else
                                                    if comprueba_empate t then
                                                        return 0
                                                    else
                                                        do
                                                            let tur = cambia_turno turno
                                                            partida t n m tur 3
-- Función para dos jugadores humanos
partida tablero n m turno 2 = do
                                print ("Jugador "++show turno)
                                print ("Tablero "++show tablero)
                                print ("Elija una columna ")
                                c<-getLine
                                let columna = atoi c
                                if columna >= n then
                                    partida tablero n m turno 2
                                else
                                    do
                                        print ("Columna: "++show columna)
                                        let t = jugada tablero turno 0 columna
                                        print ("Tablero: "++show t)
                                        let x0 = (busca_fila t 0 columna)
                                        --let lHorizontal = coge_horizontales t n m turno x0 columna 0 1
                                        --if length lHorizontal >= 4 && todos_iguales turno lHorizontal
                                        if hay_cuatro_en_raya t n m turno x0 columna
                                        then 
                                            return turno
                                        else
                                            if comprueba_empate t then
                                                return 0
                                            else
                                                do
                                                    let tur = cambia_turno turno
                                                    partida t n m tur 2

-- Función que comprueba si se ha producido un empate
-- El empate se producte cuando el la primer fila del tablero (tablero!!0) no hay ningún 0, es decir, no hay hueco para poner una ficha
-- @parámetros: tablero
-- @devuelve: Falso si hay un 0 en la primer fila del tablero, y verdadero si no lo hay
comprueba_empate :: [[Int]] -> Bool
comprueba_empate tablero = if 0 `elem` tablero!!0 then False else True

-- Función que cambia el turno del jugador
-- @parámetros: turno
-- @devuelve: 1 o 2                        
cambia_turno :: Int -> Int
cambia_turno turno = if turno == 1 then 2 else 1

-- Lista con las coordenadas para comprobar el cuatro en raya
-- (0,1) -> Horizontal
-- (1,0) -> Vertical
-- (1,2) -> Diagonal principal
-- (-1,1)-> Diagonal secundaria
direccion=[(0,1),(1,0),(1,2),(-1,1)]

-- Función que genera las coordenadas consecutivas del tablero donde el valor corresponde con el turno
-- Se le pasa el tablero, las dimensiones del tablero, el turno, la coordenada donde se ha puesto la ficha (x0,y0)
-- La lista s va desde -3 hasta 3 para que genere la lista de todas las direcciones a partir de la coordenada (x0,y0)
-- @parámetros: tablero n m turno x0 y0
-- @devuelve: Una lista de quíntuplas en la que los dos primeros componentes son coordenadas, los dos siguientes es la dirección y el último es el valor de la ficha en las coordenadas que corresponderá con el turno que se le pasa como parámetro
genera_coordenadas :: [[Int]] -> Int -> Int -> Int -> Int -> Int-> [(Int, Int, Int, Int, Int)]
genera_coordenadas tablero n m turno x0 y0 = [((x0+s*ix),(y0+s*iy),ix,iy,(tablero!!(x0+s*ix)!!(y0+s*iy))) | s<-[-3..3], (ix,iy)<-direccion, (x0+s*ix)>=0, (y0+s*iy)>=0, x0<m, y0<n, (x0+s*ix)<m, (y0+s*iy)<n, tablero!!(x0+s*ix)!!(y0+s*iy)==turno]


-- Funciones que generan las coordenadas donde hay fichas consecutivas (en horizontal, vertical o las dos diagonales) que corresponden al turno
-- Se le pasa el tablero, las dimensiones del tablero, el turno, la coordenada done se ha puesto la ficha (x0,y0) y la dirección (ix,iy)
-- @parámetros: tablero n m turno x0 y0 ix iy
-- @devuelve: Una lista de quíntuplas en la que los dos primeros componenes son coordenadas, los dos siguientes es la dirección y el último es el valor de la ficha en las coordenadas
coge_horizontales :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Int]
coge_horizontales tablero n m turno x0 y0 ix iy = [v | (x,y,ix,iy,v)<-genera_coordenadas tablero n m turno x0 y0, ix==0, iy==1]

coge_verticales :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Int]
coge_verticales tablero n m turno x0 y0 ix iy = [v | (x, y, ix, iy, v)<-genera_coordenadas tablero n m turno x0 y0, ix==1, iy==0]

coge_diagonal :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Int]
coge_diagonal tablero n m turno x0 y0 ix iy = [v | (x, y, ix, iy, v)<-genera_coordenadas tablero n m turno x0 y0, ix==1,iy==2]

coge_otra_diagonal :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Int]
coge_otra_diagonal tablero n m turno x0 y0 ix iy = [v | (x, y, ix, iy, v)<-genera_coordenadas tablero n m turno x0 y0, ix==(-1), iy==1]

-- Función que comprueba si se ha hecho cuatro en raya, para que sea cuatro en raya la lista que devuelve debe tener 4 o más elementos y que todos sean igual al turno que se le pasa como parámetro
-- Se le pasa el tablero, las dimensiones el turno y la coordenada donde se ha puesto la ficha
-- @parámetros: tablero n m turno x0 y0
-- @devuelve: True si se ha hecho 4 en raya o False si no se ha hecho 4 en raya
hay_cuatro_en_raya :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Bool
hay_cuatro_en_raya tablero n m turno x0 y0 = if (lHlongitud && lH) || (lVlongitud && lV) || (lDlongitud && lD) || (lODlongitud && lOD) then True else False
    where lH = todos_iguales turno (coge_horizontales tablero n m turno x0 y0 0 1)
          lV = todos_iguales turno (coge_verticales tablero n m turno x0 y0 1 0)
          lD = todos_iguales turno (coge_diagonal tablero n m turno x0 y0 1 2)
          lOD = todos_iguales turno (coge_otra_diagonal tablero n m turno x0 y0 (-1) 1)
          lHlongitud = length (coge_horizontales tablero n m turno x0 y0 0 1) >= 4
          lVlongitud = length (coge_verticales tablero n m turno x0 y0 1 0) >= 4
          lDlongitud = length (coge_diagonal tablero n m turno x0 y0 1 2) >= 4
          lODlongitud = length (coge_otra_diagonal tablero n m turno x0 y0 (-1) 1) >= 4

-- Función que comprueba que todos los elementos de una lista son iguales al turno
-- Se le pasa el turno y la lista que se quiere comprobar
-- @parámetros turno lista
-- @devuelve: True si todos los elementos de la lista son igual a turno y Falso si no lo son
todos_iguales :: Int -> [Int] -> Bool
todos_iguales turno [] = False
todos_iguales turno [x,y] = if x==y && y==turno then True else False
todos_iguales turno (x:y:xs) = if x==y && y==turno then todos_iguales turno (y:xs) else False

-- Función principal en la que se reciben los parámetros, se crea el tablero y se elige el modo de juego según la longitud de los argumentos recibidos
-- Muestra el jugador que gana la partida
main = do
        args <- getArgs
        let n = atoi (args!!0)
        let m = atoi (args!!1)
        let t = crea_tablero m n
--        let t = [[0,0,0,0,0,0,0],[0,0,0,0,0,0,0],[0,0,0,0,0,0,0],[0,1,1,1,0,0,0],[2,1,2,2,0,0,0],[1,1,1,2,1,1,1]]
--        let t = [[0,1,2,2,1,1,2],[2,1,2,1,1,2,2],[2,2,1,1,2,2,1],[1,2,2,1,1,2,1],[1,1,2,1,1,1,2],[2,2,1,1,2,2,2]]
--        let t = [[1,1,1,0,2,1,2],[2,1,1,0,1,2,1],[2,2,1,0,2,1,2],[1,1,1,0,1,1,1],[2,2,2,0,2,2,2],[1,1,1,0,1,1,1]]

        if length args == 3 then
            do
                print ("Humano VS. Ordenador")
                turno<-partida t n m 1 3
                if turno == 0 then
                    print ("Empate")
                else
                    if turno == 2 then
                        print ("Ganador: Ordenador")
                    else
                        print ("Ganador: Jugador "++show turno)
        else
            do
                if length args == 2 then
                    do
                        print ("Humano VS. Humano")
                        turno<-partida t n m 1 2
                        if turno==0 then
                            print ("Empate")
                        else
                            print ("Ganador: Jugador "++show turno)
                else
                    do
                        error "Parámetros mal recibidos"
