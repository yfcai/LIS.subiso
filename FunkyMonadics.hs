module FunkyMonadics (
 (=>>), (++>>),
 bind_couple, bind_triple, forget
 ) where


-- my funky monadic shorthands

-- like >>, but preserves first return value instead of second

(=>>) :: Monad m => m a -> m b -> m a
m1 =>> m2 = m1 >>= (m2 >>) . return

-- equivalent to (liftM ++) m1 m2

(++>>) :: Monad m => m String -> m String -> m String
m1 ++>> m2 = m1 >>= \s -> m2 >>= return . (s ++)

-- combine the results of two monads into a tuple!

bind_couple :: Monad m => m a -> m b -> m (a, b)
bind_couple m1 m2 = m1 >>= \x -> m2 >>= \y -> return (x, y)

-- combine the results of three monads into a tuple!

bind_triple :: Monad m => m a -> m b -> m c -> m (a, b, c)
bind_triple m1 m2 m3 = m1 >>= \x->m2 >>= \y->m3 >>= \z->return (x,y,z)

-- memory wipe: equivalent to m1 >> return ()

forget :: Monad m => m a -> m ()
forget m1 = m1 >> return ()
