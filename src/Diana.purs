-- 'module Diana' is specially treated by the compiler,
-- to support short-circuit functions 'Diana.and' and 'Diana.or'.
module Diana
    (Unit(..), unit, ($), call, log,
     (==), eq, (!=), neq, (&&), and,
     (<), lt, (>), gt, (<=), le, (>=), ge,
     (+), add, (-), sub, (*), mul, (/), div,
     (%), mod, neg, (**), pow,
     class Eq, class Ord,
     class Addable,
     class Subable,
     class Divisible,
     class Multipliable,
     class Modulo,
     class Negateable,
     class Power)
where

foreign import data Unit :: Type
foreign import require :: forall a. String -> a
foreign import unit :: Unit
foreign import log :: forall a . a -> Unit


infixr 0 call as $
call :: forall a b. (a -> b) -> a -> b
call f x = f x


foreign import _add :: forall a b c. a -> b -> c
foreign import _sub :: forall a b c. a -> b -> c
foreign import _mul :: forall a b c. a -> b -> c
foreign import _mod :: forall a b c. a -> b -> c
foreign import _neg :: forall a. a -> a
foreign import _idiv :: Int -> Int -> Int
foreign import _fdiv :: Number -> Number -> Number
foreign import _div :: forall a b c. a -> b -> c
foreign import _power :: forall a b c. a -> b -> c
foreign import _eq :: forall a b. a -> b -> Boolean
foreign import _neq :: forall a b. a -> b -> Boolean
foreign import _lt :: forall a b. a -> b -> Boolean
foreign import _gt :: forall a b. a -> b -> Boolean
foreign import _ge :: forall a b. a -> b -> Boolean
foreign import _le :: forall a b. a -> b -> Boolean
foreign import _and :: Boolean -> Boolean -> Boolean 
foreign import _or :: Boolean -> Boolean -> Boolean 


class Addable a b c | a b -> c where
    add :: a -> b -> c

class Subable a b c | a b -> c where
    sub :: a -> b -> c

class Divisible a b c | a b -> c where
    div :: a -> b -> c

class Multipliable a b c | a b -> c where
    mul :: a -> b -> c

class Modulo a b c | a b -> c where
    mod :: a -> b -> c

class Negateable a where
    neg :: a -> a

instance Addable Int Int Int where
    add = _add

instance Addable Number Number Number where
    add = _add

instance Addable Number Int Number where
    add = _add

instance Addable Int Number Number where
    add = _add

instance Subable Int Int Int where
    sub = _sub

instance Subable Number Number Number where
    sub = _sub

instance Subable Number Int Number where
    sub = _sub

instance Subable Int Number Number where
    sub = _sub

instance Multipliable Int Int Int where
    mul = _mul

instance Multipliable Number Number Number where
    mul = _mul

instance Multipliable Number Int Number where
    mul = _mul

instance Multipliable Int Number Number where
    mul = _mul

instance Divisible Int Int Int where
    div = _idiv

instance Divisible Number Number Number where
    div = _fdiv

instance Divisible Number Int Number where
    div = _div

instance Divisible Int Number Number where
    div = _div

class Eq a where
    eq:: a -> a -> Boolean

class  Eq a <= Ord a where
    lt :: a -> a -> Boolean

instance Eq Int where
    eq = _eq

instance Eq Number where
    eq = _eq

instance Ord Int where
    lt = _lt

instance Ord Number where
    lt = _lt

class Power a b c | a b -> c where
    pow :: a -> b -> c

instance Power Int Int Int where
    pow = _power

instance Power Int Number Number where
    pow = _power

instance Power Number Int Number where
    pow = _power

instance Power Number Number Number where
    pow = _power

not :: Boolean -> Boolean
not true = false
not false = true

neq :: forall a. (Eq a) => a -> a -> Boolean
neq x y = not (eq x y)

ge :: forall a . (Ord a) => a -> a -> Boolean
ge x y = not (lt x y)

and ::  Boolean -> Boolean -> Boolean 
and = _and

or ::  Boolean -> Boolean -> Boolean 
or = _or

gt :: forall a . (Ord a) => a -> a -> Boolean
gt x y = and (neq x y) (not (lt x y))

le :: forall a . (Ord a) => a -> a -> Boolean
le x y = or (eq x y) (lt x y)

infixl 1 and as &&
infixl 2 or as ||
infixl 3 eq as ==
infixl 3 neq as !=
infixl 4 lt as <
infixl 4 gt as >
infixl 4 le as <=
infixl 4 ge as >=
infixl 6 add as +
infixl 6 sub as -
infixl 7 mul as *
infixl 7 div as /
infixl 7 mod as %
infixl 8 pow as **