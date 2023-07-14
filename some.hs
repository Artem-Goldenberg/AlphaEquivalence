module Some where

import Data.Bifunctor
import Data.Function
import Data.List
import Data.Map qualified as M
import Data.Maybe (fromJust)

-- import Data.Graph

type Symb = String

-- Типы-пересечения (1 способ)
infixr 4 :->

infixr 5 :/\

data Type
  = TVar Symb -- типовой атом
  | Type :-> Type -- стрелочный тип
  | Type :/\ Type -- тип пересечение
  | Univ -- универсальный тип
  deriving (Read, Show, Ord) -- Eq уберем в следующем задании

-- Типы-пересечения (2 способ)
newtype IType = I {getTypes :: [SType]} -- тип-пересечение
  deriving (Read, Show, Ord) -- Eq уберем в следующем задании

-- Простые типы
infixr 4 :-->

data SType
  = STVar Symb -- типовой атом
  | IType :--> IType -- стрелочный тип
  deriving (Read, Show) -- Eq уберем в следующем задании

toI :: Type -> IType
toI (TVar sym) = I [STVar sym]
toI (t1 :-> t2) = I [toI t1 :--> toI t2]
toI (t1 :/\ t2) = I (f t1 ++ f t2) where f = getTypes . toI
toI Univ = I []

fromS :: SType -> Type
fromS (STVar sym) = TVar sym
fromS (t1 :--> t2) = fromI t1 :-> fromI t2

fromI :: IType -> Type
fromI (I []) = Univ
fromI (I [t]) = fromS t
fromI (I (h : t)) = fromS h :/\ fromI (I t)

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

reduceS :: SType -> SType
reduceS t@(STVar _) = t
reduceS (t1 :--> t2) = reduceI t1 :--> reduceI t2

reduceI :: IType -> IType
reduceI (I l) = I $ (rmdups . map reduceS) l

instance Ord SType where
  (t :--> _) <= (s :--> _) = t <= s
  _ <= (_ :--> _) = True
  (STVar s) <= (STVar s') = s <= s'
  _ <= _ = False

instance Eq Type where
  (==) = (==) `on` toI

fastEqI :: IType -> IType -> Bool
fastEqI (I ts1) (I ts2) = length ts1 == length ts2 && and (zipWith fastEqS ts1 ts2)

fastEqS :: SType -> SType -> Bool
fastEqS (t :--> t') (s :--> s') = fastEqI t s && fastEqI t' s'
fastEqS (STVar s) (STVar s') = s == s'
fastEqS _ _ = False

instance Eq IType where
  (==) = fastEqI `on` reduceI

instance Eq SType where
  (==) = fastEqS `on` reduceS

atomsS :: SType -> [Symb]
atomsS (STVar sym) = [sym]
atomsS (t1 :--> t2) = atomsI t1 ++ atomsI t2

atomsI :: IType -> [Symb]
atomsI (I ts) = concatMap atomsS ts

-- type Map = [(Symb, [Int])]

getDepthLists :: IType -> M.Map Symb [Int]
getDepthLists t = gdlI 1 t
  where
    gdlI :: Int -> IType -> M.Map Symb [Int]
    gdlI d (I ts) = M.unionsWith (++) $ map (gdlS d) ts

    gdlS :: Int -> SType -> M.Map Symb [Int]
    gdlS d (STVar x) = M.singleton x [d]
    gdlS d (t :--> s) = M.unionWith (++) (gdlI (d + 1) t) (gdlI (d + 1) s)

type Graph = [(Symb, Symb)]

buildGraph :: IType -> Graph
buildGraph t = bgI [] t
  where
    bgI g (I ts) = concatMap (bgS g) ts
    bgS g (STVar x) = g
    bgS g (I [STVar x] :--> I [STVar y]) = (x, y) : g
    bgS g (t :--> s) = ((++) `on` bgI g) t s

somealgo :: IType -> [Symb]
somealgo t = undefined
  where
    ordered = sortOn (dl M.!) (rmdups $ atomsI t) -- possibly incorrect order
    n = length ordered
    dl = getDepthLists t
    g = buildGraph t
    indices = [(i, j) | i <- [0..n], j <- [(i + 1)..n]]
    algo g i j
        | (i, j) `elem` g, hasCycle g = undefined
        | (j, i) `elem` g, hasCycle g = undefined

hasCycle :: Graph -> Bool
hasCycle g@((u, v) : _) = dfs M.empty u v
  where
    dfs visited u v = any isBackward ns -- M.insert v Seen visited)
      where
        ns = [u | (v', u) <- g, v' == v]
        isBackward v'
          | M.findWithDefault False v' visited = True
          | otherwise = dfs (M.insert v True visited) v v'

flatten :: [(Symb, Symb)] -> [Symb]
flatten [] = []
flatten ((a, b):ord) = a : b : flatten ord

aEq :: Type -> Type -> Bool
aEq t1' t2' = let
    f = concatMap atomsS . sort . getTypes
    t1 = reduceI $ toI t1'
    t2 = reduceI $ toI t2'
    atoms1 = f t1
    atoms2 = f t2
    lists1 = getDepthLists t1
    lists2 = getDepthLists t2
    order1 = flatten $ buildGraph t1
    order2 = flatten $ buildGraph t2

    listCmp s1 s2 = fromJust (M.lookup s1 lists1) == fromJust (M.lookup s2 lists2)
    l = zipWith listCmp order1 order2
    in and l

t1 = TVar "a" :/\ TVar "b" :/\ (TVar "a" :-> TVar "b")

t1' = TVar "a" :/\ TVar "b" :/\ (TVar "b" :-> TVar "a")

t2 = TVar "b" :/\ (TVar "a" :-> TVar "b")

t2' = TVar "b" :/\ (TVar "b" :-> TVar "a")

--------------------------------

type Permutation = [(Symb, Symb)]

getAllPermutations :: [Symb] -> [Symb] -> [Permutation]
getAllPermutations syms1 syms2 = map (zip syms1) (permutations syms2)

applyI :: Permutation -> IType -> IType
applyI p (I l) = I $ map (applyS p) l

applyS :: Permutation -> SType -> SType
applyS p (STVar sym)
  | Just (_, new) <- find ((sym ==) . fst) p = STVar new
  | otherwise = STVar sym
applyS p (t1 :--> t2) = applyI p t1 :--> applyI p t2

alphaEqTy :: Type -> Type -> Bool
alphaEqTy = alphaEqITy `on` toI

alphaEqITy t1 t2 = cond1 && cond2 && cond3
  where
    cond1 = ((==) `on` length) atoms1 atoms2
    cond2 = ((==) `on` (length . getTypes)) t1' t2'
    cond3 = any (\p -> applyI p t1' == t2') $ getAllPermutations atoms1 atoms2
    atoms1 = rmdups $ atomsI t1'
    atoms2 = rmdups $ atomsI t2'
    t1' = reduceI t1
    t2' = reduceI t2

alphaEqSTy :: SType -> SType -> Bool
alphaEqSTy x y = alphaEqITy (I [x]) (I [y])
