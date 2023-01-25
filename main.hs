import Prelude
import System.IO
import System.Environment
-- Albert Słotwiński

--[a,b] [x,y,z] -> [[a,b,x],[a,b,y],[a,b,z]] O(m), gdzie m - długość drugiej listy
appendEachElem :: [Int] -> [Int] -> [[Int]]
appendEachElem tab [] = [] --jeżeli nie ma żadnego elementu, który by pasował na kolejne miejsce w wierszu, to zwracana jest tablica pusta (wiersz jest usuwany)
appendEachElem tab (x:xs) = [tab ++ [x]] ++ appendEachElem tab xs 

--sprawdzenie jakich elementów brakuje w aktualnej kolumnie O(n)
missingInCol :: Int -> Int -> [[Int]] -> Bool
missingInCol _ _ [] = True
missingInCol elem i (r:rs) = elem /= (r !! i) && missingInCol elem i rs

--sprawdzenie jakich elementów brakuje na aktualnej pozycji (uwzględnia kolumnę + wiersz); szuka pasujących cyfr
--jeżeli aktualna pozycja to pierwszy element wiersza (indeks 0) to wstawiana jest kolejna cyfra (balanced latin square)
missingElems ::  [Int] -> Int -> [Int] -> [[Int]] -> [Int]
missingElems [] _ _ _ = []
missingElems (e:es) i row rows
    | i == 0 = [length rows + 1]
    | e `notElem` row && missingInCol e i rows = e : missingElems es i row rows
    | otherwise = missingElems es i row rows

--z każdego wiersza tworzy wiersze ze wszystkimi pasującymi cyframi, poprzez dodanie ich na koniec aktualnie rozważanego wiersza (długość wiersza: n -> n+1)
--[a] -> [a,b], [a,c], [a,d]
addElem :: [[Int]] -> [[Int]] -> Int -> Int -> [[Int]]
addElem [] _ _ _ = []
addElem (r:rs) rows i n = 
    if i == n then []
    else appendEachElem r (missingElems [1..n] i r rows) ++ addElem rs rows i n

--stworzenie wiersza/wierszy (z odpowiednimi cyframi) poprzez dodawanie po jednej cyfrze, aż do momentu osiągniecia długości wiersza równej n
createRow :: [[Int]] -> [[Int]] -> Int -> Int -> [[Int]]
createRow row rows i n = 
    if i < n then createRow (addElem row rows i n) rows (i+1) n
    else row 

--dodanie kolejnego wiersza do danej macierzy
addRow :: Bool -> [[Int]] -> [[Int]] -> Int -> [[[Int]]]
addRow False _ rows n  = addRow True (createRow [[]] rows 0 n) rows n
addRow _ (r:rs) rows n = (rows ++ [r]) : addRow True rs rows n
addRow _ [] rows n = []

--dodanie po jednym wierszu do każdej z macierzy (dla tablicy macierzy z n wierszy, zwróci tablicę macierzy z n+1 wierszy)
generateMatrixs :: Int -> [[[Int]]] -> [[[Int]]]
generateMatrixs n ms = foldr (\ m -> (++) (addRow False [[]] m n)) [] ms

--generuje macierze z odpowiednią liczbą wierszy, spełniające warunki zbalansowanej macierzy łacińskiej
generate :: Int -> Int -> [[[Int]]] -> [[[Int]]]
generate i n arr
    | n == 1    = [[[1]]] --macierz 1x1
    | i == 0    = generate (i+2) n (generateMatrixs n [[[1..n]]]) --przy pierwszej iteracji dodaje wiersz [1..n] oraz jego następnik (wiersz drugi)
    | i < n     = generate (i+1) n (generateMatrixs n arr) --dodawanie po jednym wierszu
    | otherwise = arr --return tablica z macierzami

getBalancedMatrixs :: Int -> ([[[Int]]], Int)
getBalancedMatrixs n = (result, length result)
    where result = generate 0 n [[[]]] 


-- kod do zredukowanych
---------------------------------------------------
-- kod do wzajemnie ortogonalnych


missingElemsNB ::  [Int] -> Int -> [Int] -> [[Int]] -> [Int]
missingElemsNB [] _ _ _ = []
missingElemsNB (e:es) i row rows
    | e `notElem` row && missingInCol e i rows = e : missingElemsNB es i row rows
    | otherwise = missingElemsNB es i row rows

addElemNB :: [[Int]] -> [[Int]] -> Int -> Int -> [[Int]]
addElemNB [] _ _ _ = []
addElemNB (r:rs) rows i n = 
    if i == n then []
    else appendEachElem r (missingElemsNB [1..n] i r rows) ++ addElemNB rs rows i n

createRowNB :: [[Int]] -> [[Int]] -> Int -> Int -> [[Int]]
createRowNB row rows i n = 
    if i < n then createRowNB (addElemNB row rows i n) rows (i+1) n
    else row 

addRowNB :: Bool -> [[Int]] -> [[Int]] -> Int -> [[[Int]]]
addRowNB False _ rows n  = addRowNB True (createRowNB [[]] rows 0 n) rows n
addRowNB _ (r:rs) rows n = (rows ++ [r]) : addRowNB True rs rows n
addRowNB _ [] rows n = []

generateMatrixsNB :: Int -> [[[Int]]] -> [[[Int]]]
generateMatrixsNB n ms = foldr (\ m -> (++) (addRowNB False [[]] m n)) [] ms

generateNB :: Int -> Int -> [[[Int]]] -> [[[Int]]]
generateNB i n arr
    | n == 1    = [[[1]]] --macierz 1x1
    | i == 0    = generateNB (i+2) n (generateMatrixsNB n [[[1..n]]]) --przy pierwszej iteracji dodaje wiersz [1..n] oraz jego następnik (wiersz drugi)
    | i < n     = generateNB (i+1) n (generateMatrixsNB n arr) --dodawanie po jednym wierszu
    | otherwise = arr --return tablica z macierzami

getMatrixsNB :: Int -> [[[Int]]]
getMatrixsNB n = generateNB 0 n [[[]]]

getFlattenMatrixsNB :: Int -> [[Int]]
getFlattenMatrixsNB n = map concat (getMatrixsNB n)


--sprawdza czy w liście jest powtórzenie elementu danego argumentem
searchForElem :: (Int, Int) -> [(Int, Int)] -> Bool
searchForElem _ [] = False
searchForElem e (x:xs)
    | e == x    = True
    | otherwise = searchForElem e xs

--sprawdza czy w liście występują jakieś powtórzenia
searchForRepetitions :: [(Int, Int)] -> Bool
searchForRepetitions [] = True
searchForRepetitions (x:xs)
    | searchForElem x xs = False
    | otherwise = searchForRepetitions xs

--sprawdza czy dwie macierze są wzajemnie ortogonalne
checkIfMutuallyOrthogonal :: [Int] -> [Int] -> Bool
checkIfMutuallyOrthogonal xs ys = searchForRepetitions (zip xs ys)

--każdy element z drugiej listy porównuje ze wszystkimi elementami pierwszej listy (mutually_orthogonal) i zwraca liste macierzy wzajemnie ortogonalnych
getMutuallyOrthogonalToElem :: [[Int]] -> [[Int]] -> [[Int]]
getMutuallyOrthogonalToElem m_o [] = m_o
getMutuallyOrthogonalToElem m_o (r:rs)
    | and (map (checkIfMutuallyOrthogonal r) m_o) = getMutuallyOrthogonalToElem (r:m_o) rs
    | otherwise = getMutuallyOrthogonalToElem m_o rs

--szuka listy macierzy wzajemnie ortogonalnych, która będzie zawierała więcej niż jeden element
getMutuallyOrthogonalList :: [[Int]] -> [[Int]]
getMutuallyOrthogonalList [] = []
getMutuallyOrthogonalList (x:xs)
    | length result > 1 = result
    | otherwise = getMutuallyOrthogonalList xs
    where result = getMutuallyOrthogonalToElem [x] xs 

--tworzy ze splaszczonej listy macierz (macierz == tabica zawierająca tablice wierszy)
getMatrixStructure :: Int -> [a] -> [[a]]
getMatrixStructure n = foldr (\v a ->
    case a of
        (x:xs) -> if length x < n then (v:x):xs else [v]:a
        _ -> [[v]]
    ) []

createMutuallyOrthogonal :: Int -> [[[Int]]]
createMutuallyOrthogonal n = map (getMatrixStructure n) (getMutuallyOrthogonalList (getFlattenMatrixsNB n))


--sprawdza czy k jest liczba pierwsza
isPrime :: Int -> Bool
isPrime k = if k > 1 then null [x | x <- [2..k-1], k `mod` x == 0] else False

--k - numer generowanej macierzy, i - numer generowanego wiersza, j - numer generowanej kolumny
getVal :: Int -> Int -> Int -> Int -> Int
getVal k i j n = (k * (i-1) + (j-1)) `mod` n + 1

--tworzy wiersz
createR :: Int -> Int -> Int -> Int -> [Int]
createR k i j n
    | j > n = []
    | otherwise = getVal k i j n : createR k i (j+1) n

--tworzy macierz
createM :: Int -> Int -> Int -> Int -> [[Int]]
createM k i j n
    | i > n = []
    | otherwise = createR k i j n : createM k (i+1) j n

--tworzy tablice macierzy
createMs :: Int -> Int -> Int -> Int -> [[[Int]]]
createMs k i j n
    | k == n = []
    | otherwise = createM k i j n : createMs (k+1) i j n

getMutuallyOrthogonal :: Int -> ([[[Int]]], Int)
getMutuallyOrthogonal n
    | n < 3 || n == 6 = ([[[]]], 0)
    | isPrime n = (createMs 1 1 1 n, n-1)
    | otherwise = (result, length result)
    where result = createMutuallyOrthogonal n




latinsq :: Int -> FilePath -> FilePath -> IO ()
latinsq n file1 file2 = do writeFile file1 (show (getBalancedMatrixs  n))
                           writeFile file2 (show (getMutuallyOrthogonal n))