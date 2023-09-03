# Discovering PARI/GP

## [x] Introduction to PARI/GP
#### Installation of pari/gp on ubuntu: 
    sudo apt update
    sudoapt install pari-gp
#### Start with pari/gp
    run the ```gp``` command  on the terminal and start.
#### vars
    variable is not typed.
#### vecteurs
    V = [v1,v2,...,vn] vector online
    - V[i]   gives vi which  is the element of v in position i.
    - if w = [w1,...,wk] is another vector
        -- v+w : sum [v1+w1,...,vn+wn] 
    - B = [b1,...,bn]~ vector ass a column
    -V*B is the matrix product
    - M = [l1;l2;l3] a matrix with 3 lines
    - M[i,] givesle line numer i; M[,j] gives the column number j
        M[i,j] gives element at line i and column j.
    - V = [1..n] initialise the vector V with intigers from 1 to n
    - V[1..k] give the vector [v1,...,vk]
    - M[1..k,1..k,1..k] gives the matrice [l1[1..k];l2[1..k],l3[1..k]]
## [x] Second introduction 
    Function(params) = {
        # defintion here
    }

    fonction anme start with uppercase.

## http://j.deroff.free.fr/rapportter.pdf

Rapport sur l'étude du cryptosystème  du sac à dos.