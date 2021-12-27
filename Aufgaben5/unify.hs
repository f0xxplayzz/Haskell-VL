-- data Maybe a = Nothing
-- | Just a
data Type = TVar(String)
    | TyTerm(String, [Type]) deriving (Eq, Show)

unify :: [(Type, Type)] -> [(Type, Type)] -> Maybe[(Type, Type)]

unify ((TyTerm(str1, args1),TyTerm(str2, args2)):types) accu
    |str1 == str2 && args1 == args2 = unify types accu --erase
    | str1 == str2 && (length args1) == (length args2) = unify ((reduce' args1 args2) ++ types) accu --reduce
    | otherwise = Nothing --fail (1)

unify ((TyTerm(a,b),TVar(s)):types) accu = unify ((TVar(s),TyTerm(a,b)):types) accu --swap

unify ((TVar(str1),TyTerm(str2,args)):types) accu
    | not (occuresCheck' (TVar(str1)) args) = unify (map (subst' [(TVar(str1),TyTerm(str2,args))]) types) ((TVar(str1),TyTerm(str2,args)):accu)
    | otherwise = Nothing --fail (2)

unify ((TVar(str1),TVar(str2)):types) accu
    | str1 ==str2 = unify types accu
    |otherwise = unify (map (subst' [(TVar(str1),TVar(str2))]) types) ((TVar(str1),TVar(str2)):accu)

unify [] types = Just types

subst :: [(Type, Type)] -> Type -> Type
subst s (TyTerm(str, args)) = TyTerm(str, map (subst s) args)
subst ((TVar(str1), ty) : sus) (TVar(str2)) | str1 == str2 = ty
    | otherwise = subst sus (TVar(str2))
subst [] ty = ty

subst'::[(Type,Type)] -> (Type,Type) -> (Type,Type)
subst' s (type1,type2) = (subst s type1, subst s type2)

occuresCheck' :: Type -> [Type] -> Bool
occuresCheck' type1 (type2:types)
    | type1 == type2 = True
    | otherwise = occuresCheck' type1 types
occuresCheck' type1 [] = False

reduce' :: [Type] -> [Type] -> [(Type,Type)]
reduce' [] [] = []
reduce' (t1:t1s) (t2:t2s) = (t1,t2): reduce' t1s t2s

unify1 = unify [(TyTerm("f", []), TyTerm("f", []))] []
unify2 = unify [(TyTerm("f", [TyTerm("g", []), TVar("X")]),
    TyTerm("f", [TVar("Y"), TyTerm("h", [])]))] []
unify3 = unify [(TyTerm("f", [TVar("Z"), TVar("Z")]),
    TyTerm("f", [TVar("Y"), TyTerm("h", [])]))] []
unify4 = unify [(TyTerm("f", [TyTerm("g", []), TyTerm("i", [])]),
    TyTerm("f", [TVar("Y"), TyTerm("h", [])]))] []