{-# LANGUAGE BangPatterns #-}

module Lib where

import Control.Exception

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Лекционная часть

data Point = Point
  { ptX :: Double
  , ptY :: Double
  }
  deriving Show

type Name = String
newtype Team = Team Int

data Guests =
    Contestant { team :: Team, name :: Name }
  | Coach { team :: Team, name :: Name }
  | Volunteer { name :: Name }

data Complex = Complex
  { re :: !Double
  , im :: !Double
  }

-- Разминка

warmUp = assert (all id [t1, t2, t3, t4, t5, t6, t7, t8]) ()
  where
    s = "Wow"

    -- x = 'W', y = "ow"
    t1 = case s of
      (x : y) -> True
      _ -> False

    -- x = 'W', y = "ow"
    t2 = case s of
      ((:) x y) -> True
      _ -> False

    t3 = case s of
      [x, y] -> False
      _ -> True

    -- x = 'W', y = 'o', z = "w"
    t4 = case s of
      (x : y : z) -> True
      _ -> False

    -- x = 'W', y = 'o', z = 'w'
    t5 = case s of
      (x : [y, z]) -> True
      _ -> False

    -- x = 'W', y = 'o', z = 'w'
    t6 = case s of
      (x : y : z : []) -> True
      _ -> False

    -- x = 'W', y = 'o', z = 'w', w = []
    t7 = case s of
      (x : y : z : w) -> True
      _ -> False

    t8 = case s of
      (x : y : z : w : []) -> False
      _ -> True

warmUp2 = assert (f "False" == "all") ()
  where
    -- Из-за тильды образец "e" не матчится до момента когда нам
    -- нужна переменная оттуда (то есть никогда, потому что там нет
    -- переменных. Поэтому r матчится с 'a', а u -- с 'l'. Далее,
    -- (:) r $ (:) u [u] ===
    -- (:) r ((:) u [u]) ===
    -- r:(u:[u]) ===
    -- [r, u, u] ===
    -- ['a', 'l', 'l'] ===
    -- "all"
    f (~ t : ~ r : ~ u : ~ "e") = (:) r $ (:) u [u]

warmUp3 = undefined
  where
    -- Это 42, потому что в хаскелле функция от двух аргументов --
    -- отдельная сущность, а не синтаксический сахар. И это
    -- считается WHNF.
    t1 = seq ((\True y -> "AAA") undefined) 42

    -- А это не WHNF. И поэтому это undefined.
    t2 = seq ((\True -> \y -> "AAA") undefined) 42

-- Обработка списков

countEven :: [Int] -> Int
countEven [] = 0
countEven (x:xs)
  | x `mod` 2 == 0 = 1 + countEven xs
  | otherwise      = countEven xs

countEven' :: [Int] -> Int
countEven' = go 0
  where
    -- Техника аккумулятора. Так рекурсия становится хвостовой
    -- (то есть все возвращаемые значения -- значение или
    -- рекурсивный вызов без навешивания чего-то сверху).
    -- Компилятор это свернёт в цикл.
    --
    -- Восклицательный знак гарантирует строгость. Так мы не
    -- накапливаем большой хвост вычислений (1 + (1 + ...)),
    -- жрущий линию памяти и делающий кучу аллокаций.
    -- (Нужно расширение BangPatterns, см. шапку файла)
    go !acc [] = acc
    go !acc (x:xs)
      | x `mod` 2 == 0 = go (1 + acc) xs
      | otherwise      = go acc xs

-- Сделать почти эту штуку хвосторекурсивно -- доп в домашке.
oddElementsFrom :: [Int] -> [Int]
oddElementsFrom [] = []
oddElementsFrom (x:xs)
  | x `mod` 2 == 1 = x : oddElementsFrom xs
  | otherwise      = oddElementsFrom xs

subtractLists :: [Int] -> [Int] -> [Int]
subtractLists []     []     = []
subtractLists []     (y:ys) = (-y) : subtractLists [] ys
subtractLists (x:xs) []     = x : subtractLists xs []
subtractLists (x:xs) (y:ys) = (x - y) : subtractLists xs ys

nullifyOddIndexed :: [Int] -> [Int]
nullifyOddIndexed []       = []
nullifyOddIndexed [x]      = [x]
nullifyOddIndexed (x:_:xs) = x : 0 : nullifyOddIndexed xs

-- А можно так:
nullifyOddIndexed' :: [Int] -> [Int]
nullifyOddIndexed' = go 0
  where
    go _ [] = []
    go i (x:xs)
      | i `mod` 2 == 0 = x : nullifyOddIndexed xs
      | otherwise      = 0 : nullifyOddIndexed xs

flipNeighbours :: [a] -> [a]
flipNeighbours []       = []
flipNeighbours [x]      = [x]
flipNeighbours (x:y:xs) = y : x : flipNeighbours xs

revList :: [a] -> [a]
revList = go []
  where
    -- Эта штука даже хвосторекурсивна. Строгость здесь не нужна,
    -- так как ответ всё равно занимает линейную память.
    go acc []     = acc
    go acc (x:xs) = go (x:acc) xs

sublistOfLenFrom :: Int -> Int -> [a] -> [a]
sublistOfLenFrom n k l      | n < 0     = []
                            | k < 0     = sublistOfLenFrom (n + k) 0 l
sublistOfLenFrom 0 _ _      = []
sublistOfLenFrom _ _ []     = []
sublistOfLenFrom n k (x:xs) | k == 0    = x : sublistOfLenFrom (n - 1) 0 xs
                            | otherwise = sublistOfLenFrom n (k - 1) xs

-- Стандартные функции для списков

-- map берёт функцию и список и применяет функцию к каждому
-- элементу списка. Возвращает список результатов.
twiceAll :: [Int] -> [Int]
twiceAll = map (2 *)

twiceEven :: [Int] -> [Int]
twiceEven = map f
  where
    f x | x `mod` 2 == 0 = 2 * x
    f x = x

-- zip берёт два списка и возвращает список пар. Если длины разные,
-- заканчивает там, где заканчивался самый короткий.
enumerateElems :: [a] -> [(a, Int)]
enumerateElems l = zip l [0..]
  -- Можно ещё так:
  -- enumerateElems = flip zip [0..]

nullifyOddIndexed'' :: [Int] -> [Int]
nullifyOddIndexed'' = map f . enumerateElems
  -- Или же, это можно было бы записать как
  -- nullifyOddIndexed'' xs = map f (enumerateElems xs)
  where
    f (_, i) | i `mod` 2 == 1 = 0
    f (a, _) = a

-- filter берёт список и функцию, возвращающую Bool, и выкидывает
-- из списка все элементы, для которых функция вернула False.
delMoreThen :: [Int] -> Int -> [Int]
delMoreThen l x = filter (<= x) l

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith3 :: (a -> b -> c -> d) -> [a] -> b -> [c] -> [d]
sum3Short :: [Int] -> [Int] -> [Int] -> [Int]
sum3Short = zipWith3 (\x y z -> x + y + z)

-- take n берёт первые n элементов списка
-- drop n выкидывает первые n элементов списка
-- Обе функции тотальные.
sublistOfLenFrom' :: Int -> Int -> [Int] -> [Int]
sublistOfLenFrom' n k l
  | k < 0 = sublistOfLenFrom' (n + k) 0 l
  | otherwise = take n $ drop k l
