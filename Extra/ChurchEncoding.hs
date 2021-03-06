module ChurchEncoding where

-- В задании вы познакомитесь с кодировкой Чёрча.
-- Это способ кодировать различные данные с помощью лямбда-функций.

-- Например, можно кодировать истинностные значения (booleans) следующим образом:
true = \a b -> a
false = \a b -> b
-- Если словами, то истинностное значение - это функция от двух переменных,
-- которая возвращает первый аргумент, если значение это истина, и второй, если ложь.

-- Вот так можно перевести закодированное значение в обычный тип Bool языка Haskell.
churchToBool c = c True False

-- И можно определить операции с такими значениями. Например, логическое отрицание:
not' a = a false true

-- Условный оператор можно определить вот так. Это аналог if a then b else c
if' a b c = a b c
-- Но закодированное таким образом истинностные значение по сути само является условным оператором,
-- вместо if' a b c можно просто писать a b c.

-- Упражнения. Определите следующие логические операции над истинностными значения в кодировке Чёрча.
and' a b = undefined
or' a b = undefined


-- Теперь мы научимся кодировать с помощью лямбда-функций натуральные числа.
-- Число - это функция от двух аргументов, которая применяет второй аргумент к первому количество раз,
-- равное кодируемому числу. Например:
zero = \f x -> x
one = \f x -> f x
two = \f x -> f (f x)

-- Такой функцией мы можем перевести закодированное число в обычное число для языка Haskell.
churchToInt c = c (\n -> n + 1) 0
-- Начинаем с числа 0, и прибавляем единицу нужное количество раз.
-- Можете самостоятельно определить функцию для перевода числа из языка Haskell в кодирование Чёрча.
intToChurch n = undefined

-- Вот так можно определить функцию, прибавляющую к числу единицу.
addOne a = \f x -> f (a f x)

-- Упражнения. Определите следующие функции.
-- Возвращает true, если a это 0, иначе false.
isZero a = undefined
-- Возвращает true, если a это чётное число, иначе false.
isEven a = undefined

-- Сложение чисел.
add a b = undefined
-- Умножение чисел.
mul a b = undefined
-- Возведение числа a в степень b.
pow a b = undefined
-- Задание со звёздочкой. Вычитание из числа a числа b. Если a < b, вернуть 0.
sub a b = undefined

-- Функция print умеет печатать в консоль типы Bool и Int, можем тестировать код с её помощью.
main :: IO ()
main = print (churchToBool (not' true))
