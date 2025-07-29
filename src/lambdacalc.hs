
data EAL
  = Var Int
  | Lambda EAL
  | App EAL EAL
  | Box EAL
  | Let EAL EAL

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

rep :: EAL -> [String] -> String
rep (Var n) ctx = ctx !! n
rep (Lambda t) ctx = "λ" ++ v ++ "." ++ rep t (v : ctx)
  where v = [toEnum (fromEnum 'a' + length ctx) :: Char]
rep (App t1 t2) ctx = "(" ++ rep t1 ctx ++ " " ++ rep t2 ctx ++ ")"
rep (Box t) ctx = "Box " ++ rep t ctx
rep (Let t1 t2) ctx = "Let " ++ v ++ " = " ++ rep t1 ctx ++ " in " ++ rep t2 (v:ctx)
  where v = [toEnum (fromEnum 'a' + length ctx) :: Char]

instance Show EAL where
  show t = rep t []

instance Eq EAL where
  (Var n) == (Var n') = n == n'
  (App t1 t2) == (App t1' t2') = t1 == t1' && t2 == t2'
  (Lambda t) == (Lambda t') = t == t'
  (Box t) == (Box t') = t == t'
  (Let t1 t2) == (Let t1' t2') = t1 == t1' && t2 == t2'
  _ == _ = False





run :: EAL -> Maybe EAL
run t = if validate t then Just (reduce t) else Nothing



true = Lambda $ Lambda $ Var 1
false = Lambda $ Lambda $ Var 0

pair a b = Lambda $ App (App (Var 0) a) b
cons h t = Lambda $ Lambda $ App (App (Var 1) h) t
nil = Lambda $ Lambda $ Var 0

list :: [EAL] -> EAL
list = foldr cons nil

unPair :: EAL -> (EAL, EAL)
unPair (Lambda ( App (App (Var 0) h) t)) = (h, t)
unPair _ = error "unPair: invalid input"

unList :: EAL -> [EAL]
unList (Lambda (Lambda (Var 0))) = []
unList (Lambda (Lambda ( App (App (Var 1) h) t))) = h : unList t

int 0 = list [false]
int 1 = list [true]
int n = cons (if even n then false else true) (int (n `div` 2))

list2Int :: [EAL] -> Int
list2Int [] = 0
list2Int (h:t) = if h == true then 2  * list2Int t + 1 else 2 * list2Int t

unInt x = list2Int $ unList x 

-- chr :: Char -> EAL
-- chr x = int (fromEnum x)

-- str :: String -> EAL
-- str s = list $ map chr s

-- fromChr :: EAL -> Char
-- fromChr x = toEnum . fromEnum $ x



data Mydata
  = Ls [Mydata]
  | Pr (Mydata, Mydata)
  | Chr Char
  | I Int
  | B Bool
  | Nat Int
  | V EAL


instance Show Mydata where
  show x = case x of
    Ls xs -> show xs
    Pr (x,y) -> "(" ++ show x ++ ", " ++ show y ++ ")"
    Chr c -> show c
    I n -> show n
    B b -> show b
    Nat n -> show n



fromMydata :: Mydata -> EAL
fromMydata x = pair tag dat
  where
    tag = int n
    (n, dat) = case x of
      Ls xs -> (0, list $ map fromMydata xs)
      Pr (x,y) -> (1, pair (fromMydata x) (fromMydata y))
      Chr c -> (2, int (fromEnum c))
      I n -> (3, int n)
      B b -> (4, if b then true else false)
      Nat 0 -> (5, nil)
      Nat x -> (6, fromMydata $ Nat (x-1))
      V t -> (7, t)



fmt :: [String] -> EAL -> String
fmt ctx (Lambda ( App (App (Var 0) n) dat)) = let tag = unInt n in case tag of
  0 -> "[" ++ show (map (fmt ctx) $ unList dat) ++ "]"
  1 -> let (a,b) = unPair dat in "(" ++ fmt ctx a ++ ", " ++ fmt ctx b ++ ")"
  2 -> [toEnum $ unInt dat]
  3 -> show (unInt dat)
  4 -> show (dat == true)
  5 -> "0n"
  6 -> let unnat x =
            let (n, rest) = unPair x
            in if unInt n == 5 then 1 else 1 + unnat rest
        in show (unnat dat) ++ "n"
  7 -> fmt ctx dat

fmt ctx (Lambda bod) = "λ" ++ v ++ "." ++ fmt (v:ctx) bod
  where v = [toEnum (fromEnum 'a' + length ctx)]
fmt ctx (App t1 t2) = "(" ++ fmt ctx t1 ++ " " ++ fmt ctx t2 ++ ")"
fmt ctx (Box t) = "Box " ++ fmt ctx t
fmt ctx (Let t1 t2) = "Let " ++ fmt ctx t1 ++ " in " ++ fmt (v:ctx) t2
  where v = [toEnum (fromEnum 'a' + length ctx)]




main :: IO()
main = do
  -- print form
  putStrLn $ fmt [] form
  -- print $ unInt form


  where
    
    form =
      -- Lambda $ Let (Var 0) $ Box $ App (Var 0) (Var 0)
      -- int 0
      -- pair true false
      Lambda $
      fromMydata
        -- ((Ls [I 1, I 2, I 3]))
        (Nat 4)
        -- (Ls[Chr 'a', Chr 'b', Chr 'c'])




testvalidate :: EAL -> Bool -> String
testvalidate t expected = if validate t == expected then "Valid" else "Invalid: " ++ show t


-- main :: IO ()
-- main = print $ map (uncurry testvalidate) [
--   (Var 0, True),
--   (Lambda $ Var 0, True),
--   (Lambda $ App (Var 0) (Var 0), False),
--   (Lambda $ Let (Var 0) $ App (Var 0) (Var 0), False),
--   (Lambda $ Let (Var 0) $ Box $ App (Var 0) (Var 0), True),
--   (Lambda $ Let (Var 0) $ App (Box $ Var 0) (Var 7), True),
--   (Lambda $ Let (Var 0) (Box $ App (Var 0) (Var 7)), True),
--   (Lambda $ Let (Var 0) (Box $ App (Box $ Var 0) (Var 7)), False),
--   (Lambda $ Let (Var 0) $ App (Var 0) (Var 7), False)
--   ]



