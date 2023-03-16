import qualified Distribution.PackageDescription as Lab2
-- Read <- citim sirul de caractere pe care vrem sa il interpretam ca o comanda
-- Print <- reprezentam sirul de caractere intr-o structura interna (Parsare) si afisam rezultatul la STDOUT
-- Loop <- dupa ce am afisat rezultatul, afissam iar linia de comanda

-- miniHaskell> :q
-- sau :quit

-- miniHaskell> :l file.hs
-- Not Implemented

-- miniHaskell> let x:= 1 in x


-- Pentru a implementa acest interpretor, avem urm fisiere
-- Exp.hs -> definesc doua tipuri de date
--      Var -> pentru variabile
--      newtype Var = Var { getVar :: String }
--      ComplexExp care defineste care sunt expresiile pe care le putem reprezenta
--          Avem reprezentare pentru
--          - variabile
--          - numere naturale
--          - lambda functii
--          - aplicari de functii
--          - let
--          - letrec
--          - liste

-- Lab.hs <- rezolvarile din lab2
-- main.hs <- trebuie sa permita urmatoarele: 
--                      afiseaza linia de comanda
--                      citeste comanda
--                      parseaza comanda
--                      daca era quit -> iese din linia de comanda
--                      daca era load -> afiseaza ca nu e implementata
--                      daca nu s-a putut parsa -> eroare de parsare
--                      altfel, se afiseaza reprezentarea interna e expresiei

--  Parsing.hs <- definim cum parsam expresiile citite, utilizand combinatorii de parsare din Lab2.hs
--  Printing.hs <- definim cum arata afisarea expresiilor
--  REPLcommand.hs <- definim cum parsam comenzile ( quit, load, eval )

