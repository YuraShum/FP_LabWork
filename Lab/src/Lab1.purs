module Lab1 where

import Prelude

import Data.List.Types (List(..))
import Effect (Effect)
import Effect.Console (log)

-- Створення singleton за допомогою Cons
-- Cons -додає елемент у початок списку
singleton :: forall a. a -> List a
singleton a = (Cons a Nil)


-- Сворення функції null яка повертає
-- значення true - якщо список не пустий
-- folse- якщо пустий.
null :: forall a. List a -> Boolean
-- використовуємо Nil, що означає кінець списку
-- в іншому випадку якщо шось знаходиться в списку окрім 
-- Nil він не пустий.
null Nil = true
null _ = false



-- додання елементу в кінець списку за допомогою рекурсії
snoc :: forall a. List a -> a -> List a
snoc Nil x = singleton(x)
snoc (Cons head tail) x = Cons head $ snoc tail x

--Отримання довжини списку за допомогою рекурсії
length :: forall a. List a -> Int 
length a = lengthR a 0

lengthR :: forall a. List a -> Int -> Int
lengthR Nil totalCount = totalCount
lengthR (Cons head tail) currentCount = lengthR tail (currentCount  + 1)



test :: Effect Unit
test = do
  -- виведення результату 
  log $ show $ singleton("secondElement")
  log $ show $ null $ singleton("not null")
  let a = Cons "firstElement" $ singleton("secondElement")
  log $ show $ snoc a "theLastElement"
  log $ show $ length a
