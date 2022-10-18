module Lab2 where

import Prelude

import Data.List (List(..), length, reverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)


-- Пошук індексу
findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex fn = go 0
  where
  go :: Int -> List a -> Maybe Int
  -- випадок перебору значень з двома випадками
  -- fn x = Just n якщо число задовольняє предикат 
  -- виводимо індекс числа, у іншому випадку збільшуємо індекс на +1
  -- та повторюємо обхід
  go n (Cons x xs)| fn x = Just n
                  | otherwise = go (n + 1) xs
  -- Випадок коли завершується список
  go _ Nil = Nothing


-- знаходження індекса останнього елемента, для якого виконується предикат
findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
-- Записуємо значення довжини списку та обертаємо його 
-- у такому випадку число n буде максимальним у першій ітерації
findLastIndex fn xs = go (length(xs) - 1) (reverse(xs))
  where
  go :: Int -> List a -> Maybe Int
  --fn x = Just n якщо число задовольняє предикат 
  -- виводимо індекс числа, у іншому випадку зменшуємо індекс на -1
  -- та повторюємо обхід
  go n (Cons a as)| fn a = Just n
                  | otherwise = go (n - 1) as
  -- Випадок коли завершується список         
  go _ Nil = Nothing


--zip візьме 2 списки та заархівує їх в один список
zip :: forall a b. List a -> List b -> List (Tuple a b)
zip xs ys = reverse $ go xs ys Nil
  where
  -- випадок коли перший список закінчився, а існують ще значення в другому 
  --тоді виводимо значення акумулятора
  go Nil _ acc = acc
  -- випадок коли другий список закінчився, а існують ще значення в першому
  --тоді виводимо значення акумулятора
  go _ Nil acc = acc
  -- генеруємо Tuple, із отриманих значень списків a,b та зберігаємо в акумуляторі,
  -- виконуємо до завершення елементів у будь-якому із списків
  go (Cons a as) (Cons b bs) acc = go as bs $ Cons (Tuple a b) acc


-- зворотній до zip,у результаті виконання отримуємо два списки 
unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip xy = go xy Nil Nil
  where
  -- випадок коли закінчилися значення Списку, поверне значення акумуляторів,
  --де acc1 це значення першого списку, acc2 значення другого списку
  go Nil acc1 acc2 = Tuple (reverse acc1) (reverse acc2)
  -- з кортежу отримуємо значення де (fst a) повертає перший компонент кортежу,
  -- (snd a) повертає другий компонент кортежу.
  go (Cons a as) acc1 acc2 = go as (Cons (fst a) acc1) (Cons (snd a) acc2)


--Відфільтрувати список, зберігаючи елементи, які задовольняють функцію предиката.
filter :: forall a. (a -> Boolean) -> List a -> List a
-- Випадок коли повністю пройдений список
filter _ Nil = Nil
-- Якщо елемент списку задовольняє виконання предикату 
-- тоді цей елемент додано до кінцевого результату, якщо не задовольняє - 
-- запис елемента не відбувається, переходимо до наступної ітерації
filter p (Cons x xs)| p x = Cons x $ filter p xs
                    | otherwise = filter p xs


--оптимізація filter за допомогою хвостової рекурсії
tailRecursionFilter :: forall a. (a -> Boolean) -> List a -> List a
tailRecursionFilter p = go Nil
  where
  -- випадок коли список закінчився, виводимо значення акумулятора
  go acc Nil = reverse acc
  go acc (Cons x xs)
    -- якщо елемент задовольняє предикат, додаємо елемент до акумулятора
    | p x = go (Cons x acc) xs
    -- в іншому випадку пропускаємо елемент
    | otherwise = go acc xs


-- take поверне вказану кількість елементів зі списку 
-- або стільки, скільки зможе, якщо список замалий.
take :: forall a. Int -> List a -> List a
-- випадок коли задане число кількості елементів закінчилося раніше списку
take n _ | n < 1 = Nil
-- Випадок коли список закінчився раніше за число кількості елементів
take _ Nil = Nil
-- Виконуємо рекурсивний виклик функції кожну ітерацію записуємо 
-- значення елементу списку 
take n (Cons x xs) = Cons x $ take (n-1) xs


--оптимізація take за допомогою хвостової рекурсії
tailRecursionTake :: forall a. Int -> List a -> List a
tailRecursionTake = go Nil
  where
  --випадок коли задане число кількості елементів закінчилось раніше списку,
  -- виводимо значення акумулятора
  go acc n _ | n < 1 = reverse acc
  -- Випадок коли список закінчився раніше за число кількості елементів
  go acc _ Nil = reverse acc
  -- рекурсивний виклик функції після додавання x до акумулятора та зменшення n
  go acc n (Cons x xs) = go (Cons x acc) (n - 1) xs

test :: Effect Unit
test = do
  let firstList = Cons 5 (Cons 4 (Cons 1 (Cons 2 (Cons 1 Nil))))
  let secondList = Cons "fifth" (Cons "fourth" (Cons "third" (Cons "second" (Cons "first" Nil))))
  log $ show $ firstList
  log "1) findIndex: "
  log $ show $ findIndex (_ <= 1) firstList
  log "2) findLastIndex: "
  log $ show $ findLastIndex (_ <= 1) firstList
  let zipList = zip firstList secondList
  log "3) zip: "
  log $ show $ zipList
  log "4) unzip: "
  log $ show $ unzip zipList
  log $ show $ fst $ unzip zipList
  log $ show $ snd $ unzip zipList
  log "5) filter: "
  log $ show $ filter (_ <= 2) firstList
  log "6) tailRecursionFilter: "
  log $ show $ tailRecursionFilter (_ <= 3) firstList
  log "7) take: "
  log $ show $ take 3 firstList
  log "8) tailRecursionTake: "
  log $ show $ tailRecursionTake 3 firstList