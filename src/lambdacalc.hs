
data EAL
  = Var Int
  | Lambda EAL
  | App EAL EAL
  | Box EAL
  | Let EAL EAL
  deriving (Eq)

reduce :: EAL -> EAL
reduce (Var n) = Var n
reduce (App t1 t2) = case (reduce t1, reduce t2) of
  (Lambda body, t2') -> reduce (sub body 0 t2')
  (t1', t2') -> App t1' t2'
reduce (Lambda t) = Lambda (reduce t)
reduce (Box t) = Box (reduce t)
reduce (Let t1 t2) = reduce (sub t2 0 t1)

sub :: EAL -> Int -> EAL -> EAL
sub (Var n) i t = if n == i then t else Var n
sub (App t1 t2) i t = App (sub t1 i t) (sub t2 i t)
sub (Lambda bod) i t = Lambda (sub bod (i+1) t)
sub (Box b) i t = Box (sub b i t)
sub (Let t1 body) i t = Let (sub t1 i t) (sub body (i+1) t)

validate :: EAL -> Bool
validate (Var n) = True
validate (Lambda bod) = validate bod && (countVar 0 bod <= 1) && checkBox bod 0 0
validate (App t1 t2) = validate t1 && validate t2
validate (Box t) = validate t
validate (Let t1 t2) = validate t1 && validate t2 && checkBox t2 1 0

countVar :: Int -> EAL -> Int
countVar i (Var n) = if n == i then 1 else 0
countVar i (App t1 t2) = countVar i t1 + countVar i t2
countVar i (Lambda t) = countVar (i+1) t
countVar i (Box t) = countVar i t
countVar i (Let t1 t2) = countVar i t1 + countVar (i+1) t2

checkBox :: EAL -> Int -> Int -> Bool
checkBox (Var n) b i = (n /= i) || (b == 0)
checkBox (App f a) b i = checkBox f b i && checkBox a b i
checkBox (Lambda t) b i = checkBox t b (i+1)
checkBox (Box t) b i = checkBox t (b-1) i
checkBox (Let t1 t2) b i = checkBox t1 b i && checkBox t2 b (i+1)



run :: EAL -> Maybe EAL
run t = if validate t then Just (reduce t) else Nothing


-- low level data structures
true = Lambda $ Lambda $ Var 1
false = Lambda $ Lambda $ Var 0

zero = false
suc x = Lambda $ Lambda $ App (Var 1) x
pair a b = Lambda $ App (App (Var 0) a) b


nil = zero
mkList :: [EAL] -> EAL
mkList = foldr pair nil

mkNat 0 = zero
mkNat n = suc (mkNat (n-1))

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
rep ctx (Box t) = "Box " ++ fmt ctx t
rep ctx (Let t1 t2) = "Let " ++ fmt ctx t1 ++ " in " ++ fmt (v:ctx) t2
  where v = [toEnum (fromEnum 'a' + length ctx)]
rep ctx (Var n) = ctx !! n

instance Show EAL where show = fmt []


natMatch :: EAL -> EAL -> EAL -> EAL
natMatch x z s =
  App x $ App s z








n0 = mkNat 0
n1 = mkNat 1
n2 = mkNat 2

main :: IO ()
main = do
  print
  $ run
  $ natMatch n2 n0 n1





