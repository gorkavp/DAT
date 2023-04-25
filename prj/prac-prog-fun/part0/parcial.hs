de tipus IO (Maybe Int)
-- readInt :: String -> Maybe Int
-- getString :: IO String
a) readInt getString -- incorrecte perquè getString no es de tipus String
b) do -- incorrecte perquè getString no es de tipus String
    let v1 = getString
    pure (readInt v1)
c) do -- correcte
    v1 <- getString -- v1 es de tipus String i getString es de tipus IO String
    pure (readInt v1) -- readInt v1 es de tipus Maybe Int
d) do -- incorrecte perquè readInt v1 no es de tipus IO (Maybe Int) solament es de tipus Maybe Int
    v1 <- getString
    readInt v1
e) readInt <$> getString -- correcte perquè readInt s'applica a getString sense modificar el tipus de getString
f) getString >>= pure . readInt -- correcte perquè es l'equivalent de la solució c)
-- pure . readInt es equivalent a \v1 -> pure (readInt v1)
--do 
--    v1 <- exp1 equivalent a exp1 >>= \v1 -> exp2

definir la funció la funció factorial: fact :: Int -> Int
a) de forma recursiva
fact n = if n <= 0 then 1 else n * fact (n-1)
b) amb foldl :: (b -> a -> b) -> b -> [a] -> b i una funció ascendent :: Int -> [Int]  que obte una llista de nombres de 1 a n
fact n = foldl (*) 1 (ascendent n) -- fact = foldl (*) 1 . ascendent
c) definir la funció ascendent :: Int -> [Int]
acendent n = if n <= 0 then [] else acendent (n-1) <> [n] -- else n : acendent (n-1)

completar les funcions següents:
-- data StateM s m a = StateMC (s -> m (a, s))
-- runStateM :: StateM s m a -> s -> m (a, s)
instance Monad m => Monad (StateM s m) where
    mx >>= K = StateMC $ \s0 -> do -- Monad m , mx :: StateM s m a, K :: a -> StateM s m b
        (x, s1) <- runStateM mx s0 -- runStateM retorna m (a, s)
        (y , s2) <- runStateM (K x) s1 -- K x :: StateM s m b
        pure (y, s2)
        -- les dos ultimes linies es poden substituir per: runStateM (K x) s1 

get :: Monad m => StateM s m s
get = StateMC $ \s0 -> pure (s0, s0)

put :: Monad m => s -> StateM s m ()
put new = StateMC $ \s0 -> pure ((), new)

Tipus Test

tipus de 'f'?
f x y = if x && y 
    then Right x
    else Left "foo"

a) Bool -> Bool -> Either Bool String
b) String -> String -> Either String String
c) Bool -> Bool -> Either String Bool -- correcte

tipus de 'justBoth a b = [Just a, Just b]'?
a) a -> b -> [Maybe a, Maybe b]
b) a -> a -> [Just a]
c) a -> b -> [Maybe a]
d) a -> b -> [Maybe a] -- correcte

tipus de 'applyInOUt f g x = f (g (f x))'?
a) (a -> b) -> (b -> a) -> a -> b -- correcte
b) (a -> b) -> (b -> c) -> a -> c
c) (a -> a) -> (a -> a) -> a -> a

tipus de l'expressió 'const const'?
const const :: c -> a -> b -> a
-- const és a' -> b -> a' i el segon const és a -> b -> a
-- aplicant el primer const al segon const es (a -> b -> a) -> c -> (a -> b -> a)

tipus de l'expressió 'swap . swap'? -- swap :: (a, b) -> (b, a)
a) (a, b) -> (a, b) -- correcte

quina definició de 'f' no satisfà 'f 1 == 1'
a) f x = (\y -> y) x
b) f x = \y -> y -- correcte
c) f x = (\y -> x) x

tipus de del primer element de la expressio 'id id'?
-- id :: a -> a
-- id . id :: a -> a
-- id id :: a -> a
a) error
b) a
c) a -> a
d) (a -> a) -> (a -> a) -- correcte
