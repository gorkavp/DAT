PARCIAL 2022-2023

What is the Haskell equivalent of the C/Java/Python expression combine(prettify(lawn),construct(house,concrete))?
a) combine prettify (lawn) construct (house concerete)
b) combine (prettify lawn (counstruct house concrete))
c) combine (prettify lawn) (construct house concrete) -- correcte

What is the C/Java/Python equivalent of the Haskell expression send metric (double population + increase)?
a) send(metric(double(population+increase)))
b) send(metric(double(population)+increase))
c) send(metric,double(population)+increase) -- correcte
d) send(metric,double(population+increase))

Which one of the following claims is true in Haskell?
a) It’s impossible to reuse the name of a variable
b) It’s possible to reassign a value to a variable
c) An if always requires both then and else -- correcte

What does the function f x = if even (x + 1) then x + 1 else f (x - 1) do?
-- even :: Integral a => a -> Bool
a) Maps every value x to the least even number greater than or equal to  -- correcte
b) Maps every value x to the greatest even number less than or equal to x
c) Maps every value to itself

Why is 3 * "F00" not valid Haskell?
a) 3 and "F00" have different types -- correcte perquè 3 és de tipus Num i "F00" és de tipus String
b) All numeric values need a decimal point
c) "F00" needs the prefix “0x”

Why does 7.0 `div` 2 give an error?
a) Because div is not defined for the type Double -- correcte perquè 7.0 és de tipus Fractional i div és de tipus Integral
b) Because div is not defined for the type Int
c) Because `...` is used for delimiting strings.

How many values does f x = [x,x] return?
a) Zero
b) One -- correcte perquè f :: a -> [a]
c) Two

Why does the expression Nothing 1 cause a type error?
a) Because Nothing takes no arguments -- correcte perquè Nothing no té cap argument
b) Because Nothing returns nothing
c) Because Nothing is a constructor

What is the type of function f x y = if x && y then Right x else Left "foo"? -- x i y son de tipus Bool
a) Bool -> Bool -> Either Bool String
b) String -> String -> Either String String
c) Bool -> Bool -> Either String Bool -- correcte

Which of the following functions could have the type Bool -> Bool -> [Bool]?
a) f x y = [0, y]
b) f x y = [x, True] -- correcte perquè x i y son de tipus Bool i True és de tipus Bool també i per tant la llista és de tipus [Bool]
c) f x y = [y, True]

What is the type of this function? justBoth a b = [Just a, Just b]
a) a -> b -> [Maybe a, Maybe b]
b) a -> a -> [Just a]
c) a -> b -> [Maybe a]
d) a -> a -> [Maybe a] -- correcte perquè a i b son de tipus a i per tant [Just a, Just b] és de tipus [Maybe a]

What's the type of this function? both p q x = p x && q x -- p i q son funcions i x es un valor de tipus a
-- both :: (a -> Bool) -> (a -> Bool) -> a -> Bool
a) a -> Bool -> a -> Bool -> a -> Bool
b) (a -> Bool) -> (a -> Bool) -> a -> Bool -- correcte
c) (a -> Bool) -> (b -> Bool) -> c -> Bool

What's the (most general) type of this function?  applyInOut f g x = f (g (f x)) -- f i g son funcions i x es un valor de tipus a
a) (a -> b) -> (b -> a) -> a -> b -- correcte
b) (a -> b) -> (b -> c) -> a -> c
c) (a -> a) -> (a -> a) -> a -> a

Which one of the following functions adds its first argument to the second?
a) f x x = x + x
b) f x =\y -> x + y -- correcte perquè es equivalent a f x y = x + y o f = \x y -> x + y o f = \x -> \y -> x + y o f = \x -> (\y -> x + y) o f = \x -> (+) x
c) f = \x y -> x + x

Which one of the following functions does not satisfy f 1 ==> 1? -- ==> significa "implica" o "resulta en" o "evalua a" pel que f 1 ==> 1 significa que f 1 resulta en 1
a) f x = (\y -> y) x -- correcte perquè f 1 = (\y -> y) 1 = 1 es equivalent a f x = x o f = \x -> x
b) f x = \y -> y -- incorrecte perquè f 1 = \y -> y no es de tipus Int
c) f x = (\y -> x) x -- correcte perquè f 1 = (\y -> 1) 1 = 1 es equivalent a f x = x o f = \x -> x

Which one of the following functions is correctly typed?
a) f x y = not x; f :: (Bool -> Bool) -> Bool -- no es correcte perquè f x y = not x es de tipus Bool -> Bool -> Bool
b) f x = x ++ "a"; f :: Char -> String -- no es correcte perquè f x = x ++ "a" és de tipus [Char] -> [Char]
c) f x = 'a' : x; f :: String -> String -- correcte perquè f x = 'a' : x afegeix el caràcter 'a' al principi del String x ja que : és l'operador de concatenació de cadenes de caràcters i per tant x ha de ser de tipus String

How many argument does drop 2 take?
a) Zero
b) One -- correcte perquè drop :: Int -> [a] -> [a] i en aquest cas drop 2 :: [a] -> [a]
c) Two

What does this function do? f (_:x:_) = x
a) Returns the first element of a list
b) Returns an arbitrary element of a list
c) Returns all except the first and last elements of a list
d) Returns the second element of a list -- correcte perquè es realitza el pattern matching de la llista i es retorna el segon element

What is the result of reverse $ take 5 . tail $ "This is a test"?
-- tail :: [a] -> [a] retorna una llista sense el primer element
-- take :: Int -> [a] -> [a] retorna una llista amb els primers n elements
-- reverse :: [a] -> [a] retorna una llista amb els elements en ordre invers
a) "i sih" -- correcte
b) "set a"
c) A type error

If f :: a -> b, then what is the type of map (.f)?
a) [b->c] -> [a->c] -- correcte perquè map :: (a -> b) -> [a] -> [b] i per tant map (.f) :: [b->c] -> [a->c]
b) [c->a] -> [c->b]
c) [b->c] -> [a->c]
d) [a] -> [b]

What is the type of the leftmost id in id id?
-- id :: a -> a
-- id . id :: a -> a
-- id id :: a -> a
a) unspecified
b) a
c) a -> a
d) (a -> a) -> (a -> a) -- correcte

What is type of const const?
-- const :: a -> b -> a
a) unspecified
b) (c -> a -> b) -> a
c) c -> (a -> b -> a) -- correcte perquè const és a -> b -> a aleshores const const és (a -> b -> a) -> c -> (a -> b -> a)
d) (a -> b -> c) -> a

What is the type of swap . swap?
-- swap :: (a,b) -> (b,a)
a) (a, b) -> (a, b) -- correcte perquè la composició de swap amb swap retorna la mateixa tupla
b) (a, b) -> (b, a)
c) a -> a

What is the type of \f g x -> (f x, g x)?
a) (a -> b) -> (c -> d) -> (a, c) -> (b, d)
b) (a -> b) -> (a -> c) -> a -> (b, c) -- correcte perquè \f g x -> (f x, g x) és equivalent a \f -> \g -> \x -> (f x, g x)
c) (a -> b) -> (b -> a) -> a -> (b, a)

What is the type of \t -> (fst . fst $ t, (snd . fst $ t, snd t))?
-- fst :: (a, b) -> a
-- snd :: (a, b) -> b
a) (a, (b, c)) -> (a, (b, c))
b) (a, (b, c)) -> ((a, b), c)
c) ((a, b), c) -> (a, (b, c)) -- correcte

What does the function foldr (\x xs -> xs ++ [x]) [] do?
a) It doesn't change its input list at all
b) It changes the associativity of a list from left to right
c) Ir reverses its input list -- correcte perquè foldr (\x xs -> xs ++ [x]) [] és equivalent a foldr (flip (:)) [] i per tant es realitza el reverse de la llista

What does the function foldr (\(x, y) zs -> x : y : zs) [] do?
a) It turns a list of pairs into pairs of lists
b) It turns a pair of lists into a list of pairs
c) It turns a list of paris into a list of elements -- correcte

What is the type of foldr (\n b -> n == 3 && b)?
a) (Foldable t, Eq a, Num a) => Bool -> t a -> Bool -- correcte
b) (Foldable t, Eq a, Num a, Bool a) => b -> t a -> Bool
c) (Foldable t, Eq a, Num a) => Bool -> [a] -> Bool

What is the type of \x -> case x of (True, "Foo") -> show True ++ "Foo"?
a) Either Bool String -> String
b) (Bool, String) -> String -- correcte
c) Show a => (Bool, String) -> a

Why can't we map Nothing?
a) Becasue Nothing doesn't take arguments -- correcte
b) Because Nothing returns nothing
c) Because Nothing is a constructor

If we define data Boing = Frick String Boing (Int -> Bool), what is the type of Frick?
a) Boing
b) String -> Boing -> Int -> Bool -> Boing
c) String -> Boing -> (Int -> Bool) -> Boing -- correcte perquè Frick és un constructor de Boing

If we define data ThreeLists a b c = ThreeLists [a] [b] [c], what is the type of the constructor ThreeLists?
a) [a] -> [b] -> [c] -> ThreeLists
b) a -> b -> c -> ThreeLists a b c
c) [a] -> [b] -> [c] -> ThreeLists a b c -- correcte perquè ThreeLists és un constructor de ThreeLists a b c
d) [a] -> [b] -> [c] -> ThreeLists [a] [b] [c]

If we define data TwoLists a b = TwoLists {aList :: [a], bList :: [b]}, what is the type of the function aList?
a) aList is not a function, it is a field
b) TwoLists a b -> [a] -- correcte perquè aList és un camp que retorna una llista de tipus a
c) [a] -> TwoLists a b
d) [a]

What are the functions in the Eq class?
a) (==), (/=) -- correcte
b) (==)
c) (==), (<), (>)

For which of the following classes can we get automatic instances with deriving?
a) Num
b) Ord -- correcte perquè Ord és una subclasse de Eq i per tant es pot obtenir una instància automàtica de Ord si es té una instància de Eq
c) Size

Which of the following instance declarations is legal?
a) instance Eq Maybe
b) instance Eq (a,a)
c) instance Eq (Maybe Int)
d) instance Eq (a,b)

The declaration instance Num a => Eq (Pair a) tells me that
a) All instances of Num are instances of Eq
b) Pair a is an instance of Eq if a is an instance of Num -- correcte perquè Pair a és una instància de Eq si a és una instància de Num
c) The instance Eq (Pair a) inherits the instance Num a

The declaration class Num a => Fractional a tells me that
a) All instances of Fractional must be instances of Num -- correcte perquè Fractional és una subclasse de Num
b) All instances of Num must be instances of Fractional
c) If I define an instance for Fractional, I also get an instance for Num
d) If I define an instance for Num, I also get an instance for Fractional

Quines opcions retornen un valor de tipus IO (Maybe Int):
-- readInt :: String -> Maybe Int
-- getString :: IO String
a) readInt getString -- incorrecte perquè getString no es de tipus String
b) do -- incorrecte perquè getString no es de tipus String
    let v1 = getString
    pure (readInt v1)
c) do -- correcte
    v1 <- getString -- v1 es de tipus String
    pure (readInt v1) -- pure (readInt v1) es de tipus IO (Maybe Int)
d) do -- incorrecte perquè readInt v1 no es de tipus IO (Maybe Int) solament es de tipus Maybe Int
    v1 <- getString
    readInt v1
e) readInt <$> getString -- correcte perquè readInt s'applica a getString sense modificar el tipus de getString
f) getString >>= pure . readInt -- correcte perquè es l'equivalent de la solució c)
-- pure . readInt es equivalent a \v1 -> pure (readInt v1)
--do 
--    v1 <- exp1 equivalent a exp1 >>= \v1 -> exp2

Definir la funció la funció factorial: fact :: Int -> Int
a)  De forma recursiva:
    fact n = if n <= 0 then 1 else n * fact (n-1)
b)  Amb foldl :: (b -> a -> b) -> b -> [a] -> b i una funció ascendent :: Int -> [Int]  que obte una llista de nombres de 1 a n
    fact n = foldl (*) 1 (ascendent n) -- fact = foldl (*) 1 . ascendent
c)  Definir la funció ascendent :: Int -> [Int]
    acendent n = if n <= 0 then [] else acendent (n-1) <> [n] -- else n : acendent (n-1)

Completar les funcions següents:
-- data StateM s m a = StateMC (s -> m (a, s))
-- runStateM :: StateM s m a -> s -> m (a, s)
a)  instance Monad m => Monad (StateM s m) where
    -- >>= :: StateM s m a -> (a -> StateM s m b) -> StateM s m b
        mx >>= K =
            StateMC $ \s0 -> do -- Monad m , mx :: StateM s m a, K :: a -> StateM s m b
                (x, s1) <- runStateM mx s0 -- runStateM retorna m (a, s)
                (y , s2) <- runStateM (K x) s1 -- K x :: StateM s m b
                pure (y, s2)
                -- les dos ultimes linies es poden substituir per: runStateM (K x) s1 

b)  get :: Monad m => StateM s m s
    get = StateMC $ \s0 -> pure (s0, s0)

c)  put :: Monad m => s -> StateM s m ()
    put new = StateMC $ \s0 -> pure ((), new)