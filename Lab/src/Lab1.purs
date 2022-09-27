module Lab1 where

import Prelude

import Data.List.Types (List(..), (:))
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


-- Додання елементу в кінець списку
-- реалізація виконана через подвійний реверс 
-- списку, так як використовувати foldr не бажано (що дозволяє заповнення списку почнаючи з правої частини)
snoc :: forall a. List a -> a -> List a
-- спочатку обертаємо масив та додаємо елемент за допомогою Cons
-- Потім виконуємо обернення масиву знову.
snoc xs x = reverse $ Cons x $ reverse xs

-- реалізація методу реверс
reverse :: List ~> List
reverse = go Nil
  where
  go acc Nil = acc
  go acc (x : xs) = go (x : acc) xs

--Отримання довжини списку за допомогою рекурсії
length :: forall a. List a -> Int 
length a = lengthR a 0

lengthR :: forall a. List a -> Int -> Int
lengthR Nil totalCount = totalCount
lengthR (Cons head tail) currentCount = lengthR tail (currentCount  + 1)



test :: Effect Unit
test = do
  -- виведення результату 
  log $ show $ singleton("2")
  log $ show $ null $ singleton("not null")
  let a = Cons "1" $ singleton("2")
  log $ show $ snoc a "theLastElement"
  log $ show $ length a
