import Lambda
import Lambda.Eval
import Lambda.Convert

import Test.HUnit

id_, tru, fls, test_ :: Term DeBruijn
id_ = lambda () $ var 0

tru = lambda () $ lambda () $ var 1
fls = lambda () $ lambda () $ var 0

test_ = (lambda () (lambda () (lambda () (apply (apply (var 2) (var 1)) (var 0)))))

e :: String -> Term DeBruijn -> Term DeBruijn -> Test
e des t1 t2 = TestList $ map (TestCase . assertEqual des t1) [eval strict t2, eval lazy t2]

c :: String -> Either String (Term DeBruijn) -> Term (Church String) -> Test
c des t = TestCase . assertEqual des t . strip

main :: IO ()
main = do
    _ <- runTestTT $ TestList [
            e "id id" id_ (apply id_ id_),
            e "id (id (λz. id z))"
                (lambda () (apply id_ (var 0)))
                (apply id_ (apply id_ (lambda () (apply id_ (var 0))))),
            e "test tru tru fls"
                tru
                (apply (apply (apply test_ tru) tru) fls),
            e "test fls tru fls"
                fls
                (apply (apply (apply test_ fls) tru) fls),
            e "test tru v w"
                (lambda () (var 1))
                (apply (apply (apply test_ tru) (lambda () (var 1))) (lambda () (var 2))),
            e "(λ.1 0 2) (λ.0)"
                (apply (apply (var 0) (lambda () (var 0))) (var 1))
                (apply (lambda () (apply (apply (var 1) (var 0)) (var 2))) (lambda () (var 0))),
            e "(id (id id)) (λ.0)"
                (lambda () (var 0))
                (apply (apply id_ (apply id_ id_)) (lambda () (var 0))),
                
            c "(λx. x)"
                (Right id_)
                (lambda "x" $ var "x"),
            c "(λx. y)"
                (Left "y")
                (lambda "x" $ var "y"),
            c "(λs. λz. s (s z))"
                (Right (lambda () $ lambda () (apply (var 1) (apply (var 1) (var 0)))))
                (lambda "s" $ lambda "z" (apply (var "s") (apply (var "s") (var "z"))))
        ]
    return ()