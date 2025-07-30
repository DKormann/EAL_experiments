import Debug.Trace (trace)
-- import Data.Map (Map, insert)
-- import Data.Maybe (fromJust)

data EAL
  = Var Int
  | Lambda EAL
  | App EAL EAL
  | Box EAL
  | Let EAL EAL
  | Lam (EAL -> EAL)
  | LetH EAL (EAL -> EAL)


-- i is the nesting depth
fold :: forall a . Int -> a -> (Int -> EAL -> a -> a) -> EAL -> a
fold i d f (Var n) = d
fold i d f (App t1 t2) = f i t1 (f i t2 d)
fold i d f (Lambda t) = f (i + 1) t d
fold i d f (Box t) = f i t d
fold i d f (Let t1 t2) = f i t1 (fold (i + 1) d f t2)

mapc :: Int -> (Int -> EAL -> EAL) -> EAL -> EAL
mapc i f (Var n) = Var n
mapc i f (App t1 t2) = App (f i t1) (f i t2)
mapc i f (Lambda t) = Lambda (f (i + 1) t)
mapc i f (Box t) = Box (f i t)
mapc i f (Let t1 t2) = Let (f i t1) (f (i + 1) t2)


logi :: Int -> String -> Show a => a -> a
logi i msg x = trace (replicate (i*2) ' ' ++ msg ++ ":" ++ show x) x




reduceI :: Int -> EAL -> EAL
reduceI i (App t1 t2) = case (reduceI i t1, reduceI i t2) of
  (Lambda body, t2') -> reduceI (i+1) $ sub body 0 t2'
  (t1', t2') -> App t1' t2'
reduceI i (Let t1 t2) = case reduceI i t1 of
  Box t1' -> reduceI i (sub t2 0 t1')
  t1' -> Let t1' $ reduceI i t2
reduceI i e = mapc i reduceI e


reduce = reduceI 0

shift :: Int -> Int -> EAL -> EAL
shift i n (Var k) = Var (if k >= i then k + n else k)
shift i n e = mapc i (`shift` n ) e

sub :: EAL -> Int -> EAL -> EAL
sub (Var n) i t = if n == i then shift 0 i t else Var (if n >= i then n-1 else n)
sub e i t = mapc i (\i e -> sub e i t) e

validate :: EAL -> Bool
validate term = fold 0 True (\i c d -> d && validate c) term && check term
  where
    check (Lambda bod) = (countVar 0 bod <= 1) && checkBox bod 0 0
    check (Let t1 t2) = checkBox t2 1 0
    check _ = True

countVar :: Int -> EAL -> Int
countVar i (Var n) = if n == i then 1 else 0
countVar i x = fold i 0 (\i x c -> c + countVar i x) x

checkBox :: EAL -> Int -> Int -> Bool
checkBox (Var n) b i = (n /= i) || (b == 0)
checkBox (Box t) b i = checkBox t (b-1) i
checkBox x b i = fold i True (\i x c -> c && checkBox x b i) x

repV :: Int -> Int -> EAL -> EAL
repV i n (Var k) = Var (if k == -n then i else k)
repV i n x = mapc i (`repV` n) x

parseLamH :: Int -> EAL -> EAL
parseLamH i (Lam f) = Lambda $ repV 0 i $ parseLamH (i+1) $ f (Var (-i))
parseLamH i (LetH bod f) = Let (parseLamH (i+1) bod) (repV 0 i $ parseLamH (i+1) $ f (Var (-i)))
parseLamH i e = mapc i parseLamH e

parse = parseLamH 1
true = Lambda $ Lambda $ Var 1
false = Lambda $ Lambda $ Var 0

zero = false
suc x = Lam $ \s -> Lam $ \z -> App s x
-- suc x = Lambda $ Lambda $ App (Var 1) x
pair a b = Lambda $ App (App (Var 0) a) b


nil = zero
mkList :: [EAL] -> EAL
mkList = foldr pair nil

mkNat 0 = zero
mkNat n = Lambda $ Lambda $ App (Var 1) (mkNat (n-1))

fmtNat :: EAL -> Maybe Int
fmtNat (Lambda (Lambda (Var 0))) = Just 0
fmtNat (Lambda (Lambda (App (Var 1) x))) = case fmtNat x of Just n -> Just (n+1); _ -> Nothing
fmtNat _ = Nothing

fmtTuple :: EAL -> Maybe [EAL]
fmtTuple (Lambda (Lambda (Var 0))) = Just []
fmtTuple (Lambda (App( App (Var 0) x) tail)) = case fmtTuple tail of Just xs -> Just (x:xs); _ -> Nothing
fmtTuple _ = Nothing


fmt :: [String] -> EAL -> String
fmt ctx (Lambda (Lambda (Var 1))) = "T"
fmt ctx (Lambda (Lambda (Var 0))) = "0"
fmt ctx (Lambda (Lambda (App (Var 1) x))) = case fmtNat x of Just n -> show (n+1); _ -> rep ctx (Lambda (Lambda (App (Var 1) x)))
fmt ctx (Lambda (App (App (Var 0) a) b)) = case fmtTuple b of Just xs -> show (a:xs); _ -> rep ctx (Lambda (App (App (Var 0) a) b))
fmt ctx x = rep ctx x


rep :: [String] -> EAL -> String
rep ctx (Lambda bod) = "λ" ++ v ++ "." ++ fmt (v:ctx) bod
  where v = [toEnum (fromEnum 'a' + length ctx)]
rep ctx (App t1 t2) = "(" ++ fmt ctx t1 ++ " " ++ fmt ctx t2 ++ ")"
rep ctx (Box t) = "#" ++ fmt ctx t
rep ctx (Let t1 t2) = " let #"++ v ++ " = " ++ fmt ctx t1 ++ " in " ++ fmt (v:ctx) t2
  where v = [toEnum (fromEnum 'a' + length ctx)]
rep ctx (Var n) =  if n>=0 && n < length ctx then ctx !! n else "var"++show n
rep ctx (LetH f n) = "LetH"
rep ctx (Lam f) = "LamH"

instance Show EAL where show = fmt []

natMatch :: EAL -> EAL -> EAL -> EAL
natMatch x z s =
  App x $ App s z

ycomb = Lambda $ Let (Var 0) $ Box $ App
  (Lambda $ App (Var 1) $ Let (Var 0) $ Box (App (Var 0) (Var 0)))
  (Lambda $ App (Var 1) $ Let (Var 0) $ Box (App (Var 0) (Var 0)))

n0 = mkNat 0
n1 = mkNat 1
n2 = mkNat 2
n3 = mkNat 3




app2 f a = App (App f a)
app3 f a b = App (app2 f a b)


addt =
  Lam $ \self ->
  Lam $ \x ->
  Lam $ \y ->
    app2 x
      (Lam (\p -> suc $ app3 self self p y))
      y


main :: IO ()
main = do
  print form
  print $ validate form
  print $ reduce form

  where

    form = parse $ app3 addt addt n3 n3

    -- form = parse $ App (Lam (\x -> Lam $ \y -> Lam $ \z -> x)) n0

    





