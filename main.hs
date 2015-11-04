module FieldCover where
import Data.List

type Point a = (a, a) -- (x, y)
type Rect a  = (Point a, Point a) -- (min Point, max Point)

-- из списка точек формирует список вариантов покрытия их прямоугольниками
-- вариант покрытия допускает пересечения прямоугольников
rawVars :: Ord a => [Point a] -> [[Rect a]]
rawVars = map (map pointsToRect) . rawPointVars where
    rawPointVars [] = [[]]
    rawPointVars ps  = psTakes ps >>= \x -> map (x:) $ rawPointVars (ps\\x)

    psTakes ps = nub . map (allPsInRect ps) . go $ ps where
        go []     = []
        go (x:xs) = map (x:) $ subsequences xs

    allPsInRect l s = filter (\(x,y) -> x>=xm && x<=xM && y>=ym && y<=yM) l where
        ((xm,ym), (xM,yM)) = pointsToRect s


-- из списка точек получает минимальный покрывающий его прямоугольник
pointsToRect :: (Ord a) => [Point a] -> Rect a
pointsToRect s = ((minimum xs, minimum ys), (maximum xs, maximum ys)) where
    xs = map fst s
    ys = map snd s

-- из списка прямоугольников определяет факт пересечения любой их пары
rectsCross :: (Ord a) => [Rect a] -> Bool
rectsCross []     = False
rectsCross [r]    = False
rectsCross (r:rs) = any (rect2Cross r) rs || rectsCross rs where

    rect2Cross ((axm,aym), (axM,ayM)) ((bxm,bym), (bxM,byM)) =
        segment2Cross (axm,axM) (bxm,bxM) && segment2Cross (aym,ayM) (bym,byM)

    segment2Cross ab@(a,b) cd@(c,d) = a ? cd || b ? cd || c ? ab || d ? ab
    x ? (y,z) = y<=x && x<=z

-- по фиксированной стоимости прямоугольника, стоимости его единицы площади
-- и списку необходимых к покрытию ячеек поля определяет список вариантов
-- покрытия с минимальной стоимостью
res fixCost areaCost field = filter ((== minCost) . fst) varsWithCost where
    varsWithCost = map (\v -> (sum . map costRect $ v, v)) vars
    vars         = filter (not . rectsCross) . rawVars . sort . nub $ field
    minCost      = minimum $ map fst varsWithCost
    costRect ((xm,ym),(xM,yM)) = fixCost + areaCost * (xM-xm+1) * (yM-ym+1)

main = do
    print $ res 10 1 [(1,1),(2,1),(2,2),(4,3),(4,4),(6,3)]
