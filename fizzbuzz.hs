{-@@@

The Haskell implementation of Fizzbuzz builds infinite lists
of fizzes, buzzes and integers.

The functions `fizzbuzz` builds three infinite lists and combine them.

ns      1     2     3     4     5     6     7     8     9     10    11    12    13    14    15    ...
------- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----
fizz    .     .     fizz  .     .     fizz  .     .     fizz  .     .     fizz  .     .     fizz  ...
buzz    .     .     .     .     buzz  .     .     .     .     buzz  .     .     .     .     buzz  ...

$$
fizzbuzz(n) =
    \begin{cases}
        fizz + buzz     & \text{if } fizz \ne Nothing \lor buzz \ne Nothing \\
        n               & \text{if } fizz = buzz = Nothing \\
    \end{cases}
$$

```{.hs include="fizzbuzz.hs" pattern="%-%-%s*fizzbuzz%s*{%s*(.-)%s*%-%-%s*}" format="%1"}
```
@@@-}

import Control.Monad
import Data.Bool
import Data.Maybe
import System.Environment

-- fizzbuzz {

fizzbuzz :: [String]
fizzbuzz = zipWith3 combine fizzes buzzes ns
    where
        ws d w = cycle $ replicate (d-1) Nothing ++ [Just w]
        fizzes = ws 3 "fizz"
        buzzes = ws 4 "buzz" -- bug that shall be detected by the tests
        ns = show <$> [1..]
        combine f b n = fromMaybe n (f<>b)

-- }

main :: IO ()
main = do
    n <- read . head <$> getArgs
    forM_ (zip [1..n] fizzbuzz) $ \(i, s) ->
        putStrLn $ show i ++ "\t" ++ s
