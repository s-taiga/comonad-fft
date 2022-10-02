# comonad-fft
FFTをcomonadで実装しようという試み
ベースとして位置情報をつけたトーラスをcomonadのインスタンスにしようとした。
残念ながらcomonad則のうち結合則を満たさなかったため断念

# 確認過程
## 対象のデータ型
```haskell
newtype IndexedList a = IL [(Int, a)] deriving Show

instance Functor IndexedList where
    fmap f (IL ixs) = IL $ map (second f) ixs

rotate :: Int -> [b] -> [b]
rotate n xs = drop n xs ++ take n xs

instance Comonad IndexedList where
    extract (IL ixs) = snd . head $ ixs
    duplicate il@(IL ixs) = IL $ map (\idx -> (idx, IL $ rotate idx ixs)) [0..length ixs - 1]
```
## comonad則確認
### `extend extract      = id`
```haskell
  extend extract $ IL [(0, x0), (1, x1), ... , (n, xn)]
= fmap extract . duplocate $ IL [(0, x0), (1, x1), ... , (n, xn)]
= fmap extract $ IL [(0, IL [(0, x0), (1, x1), ... , (n, xn)]), (1, IL [(1, x1), ... , (n, xn), (0, x0)]), ... , (n, IL [(n, xn), (0, x0), ...])]
= IL $ map (second extract) [(0, IL [(0, x0), (1, x1), ... , (n, xn)]), (1, IL [(1, x1), ... , (n, xn), (0, x0)]), ... , (n, IL [(n, xn), (0, x0), ...])]
= IL [(0, extract $ IL [(0, x0), (1, x1), ... , (n, xn)]), (1, extract $ IL [(1, x1), ... , (n, xn), (0, x0)]), ... , (n, extract $ IL [(n, xn), (0, x0), ...])]
= IL [(0, x0), (1, x1), ... , (n, xn)]   -- (extract $ IL ((n, xn):_) = n)
```
### `extract . extend f  = f`
`f :: IndexedList X -> Y`で
```haskell
f $ IL [(0, x0), (1, x1), ... , (n, xn)] = y00
f $ IL [(1, x1), ... , (n, xn), (0, x0)] = y11
...
f $ IL [(n, xn), (0, x0), ...] = ynn

f $ IL [(m xn), (m+1, xn+1), ...] = ymn
```
とする
```haskell
  extract . extend f $ IL [(0, x0), (1, x1), ... , (n, xn)]
= extract $ fmap f $ IL [(0, IL [(0, x0), (1, x1), ... , (n, xn)]), (1, IL [(1, x1), ... , (n, xn), (0, x0)]), ... , (n, IL [(n, xn), (0, x0), ...])]
= extract $ IL [(0, f $ IL [(0, x0), (1, x1), ... , (n, xn)]), (1, f $ IL [(1, x1), ... , (n, xn), (0, x0)]), ... , (n, f $ IL [(n, xn), (0, x0), ...])]
= extract $ IL [(0, y00), (1, y11), ... , (n, ynn)]
= y00
```
### `extend f . extend g = extend (f . extend g)` ※成り立たない
`f :: IndexedList Y -> Z, g :: IndexedList X -> Y`で、計算結果をそれぞれ`f -> zlmn`, `g -> ymn`のように記載する
* 左辺
```haskell
  extend f . extend g $ IL [(0, x0), (1, x1), ... , (n, xn)]
= extend f $ IL [(0, g $ IL [(0, x0), (1, x1), ... , (n, xn)]), (1, g $ IL [(1, x1), ... , (n, xn), (0, x0)]), ... , (n, g $ IL [(n, xn), (0, x0), ...])]
= extend f $ IL [(0, y00), (1, y11), ... , (n, ynn)]
= IL [(0, f $ IL [(0, y00), (1, y11), ... , (n, ynn)]), (1, f $ IL [(1, y11), ... , (n, ynn), (0, y00)]), ... , (n, f $ IL [(n, ynn), (0, y00), ...])]
= IL [(0, z000), (1, z111), ... , (n, znnn)]
```
* 右辺
```haskell
  extend (f . extend g) $ IL [(0, x0), (1, x1), ... , (n, xn)]
= IL [(0, (f . extend g) $ IL [(0, x0), (1, x1), ... , (n, xn)]), (1, (f . extend g) $ IL [(1, x1), ... , (n, xn), (0, x0)]), ... , (n, (f . extend g) $ IL [(n, xn), (0, x0), ...])]
= IL [(0, f $ extend g $ IL [(0, x0), (1, x1), ... , (n, xn)]),
      (1, f $ extend g $ IL [(1, x1), ... , (n, xn), (0, x0)]),
      ... ,
      (n, f $ extend g $ IL [(n, xn), (0, x0), ...])]

= IL [(0, f $ IL [(0, g $ IL [(0, x0), (1, x1), ... , (n, xn)]), (1, g $ IL [(1, x1), ... , (n, xn), (0, x0)]), ... , (n, g $ IL [(n, xn), (0, x0), ...])]),
      (1, f $ IL [(0, g $ IL [(1, x1), ... , (n, xn), (0, x0)]), ... , (n-1, g $ IL [(n, xn), (0, x0), ...]), (n, g $ IL [(0, x0), (1, x1), ... , (n, xn)])]),
      ... ,
      (n, f $ IL [(0, g $ IL [(n, xn), (0, x0), ...]), (1, g $ IL [(0, x0), (1, x1), ... , (n, xn)]), ...])]
-- ここでduplicateした際に位置を示すインデックスが左辺の計算のものとずれてしまう
= IL [(0, f $ IL [(0, y00), (1, y11), ... , (n, ynn)]),
      (1, f $ IL [(0, y11), ... , (n-1, ynn), (n, y00)]),
      ... ,
      (n, f $ IL [(0, ynn), (1, y00), ...])]
= IL [(0, z000), (1, z011), ... , (n, z0nn)]
```

以上より左辺と右辺が異なるため、このデータ構造はcomonadではない

# 考察
問題の主眼は`duplicate`した際にインデックスが異なる振り直され方をされることであるため、位置情報を保持するデータ構造はcomonadにならないと想像される。
もし位置ベクトルに依拠するデータ構造がcomonadではない場合（要検証）は、例えばコリオリ力が働くモデルでもcomonadではないと思われる。