module ChapterExercises where

-- include path should include ./
-- -i.
import Moi


-- 1
get :: Moi s s
get = Moi $ \s -> (s,s)


-- 2
put :: s -> Moi s ()
put s = Moi $ \_ -> ((), s)


-- 3
exec :: Moi s a -> s -> s
exec (Moi sa) = snd . sa


-- 4
eval :: Moi s a -> s -> a
eval (Moi sa) = fst . sa


-- 5
modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)