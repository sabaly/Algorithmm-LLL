read("LLL.gp");

Gen_keys(n,limit=100) = {
    b = [0 | i<-[1..n]];
    for(i=1, n,
        if(i==1,b[1] = random(limit),
            somme = 0;
            for(k=1,i-1, somme += b[k]);
            b[i] = random(limit) + somme;  
        );
    );
    somme = 0;
    for(k=1,n, somme += b[k]);
    m = random(limit) + somme;
    while(m<=somme , m = random(limit)+somme);
    c = random(somme);

    while(gcd(c,m) != 1, c = random(limit));

    a = [c*b[i] % m | i<-[1..n]];
    return ([b, c, m, a]);
}

Chiffrement(pub,x) = {
    n = length(x);
    if(n!=length(pub), print("Erreur - Chiffrement : mauvaise taille"));

    return (sum(i=1,length(x), x[i]*pub[i]));
}

Gen_message(n) = {
    return ([random(2) | i<-[1..n]]);
}

Dechiffrement(private,s) = {
    b = private[1];
    c = private[2];
    m = private[3];

    n = length(b);
    x=vector(n);
    
    /* calcule de c⁻1 */
    c_un = lift(Mod(1/c, m));

    s = (c_un * s) % m;
    print("--- ", s);
    forstep(i=n, 1, -1,
        if(s >= b[i], x[i] = 1; s = s - b[i]; );
    );

    return (x);
}

/*
    Cryptanalyse
*/
Attaque_LLL(pub,s, bis=0) = {
    n = length(pub);
    M = matid(n+1);
    tmp = [0 | i<-[1..(n+1)]];
    for(i=1,n, tmp[i] = -pub[i]);
    tmp[n+1] = s;
    
    M[,n+1] = tmp~;
    
    base = [M[i,] | i<-[1..(n+1)]];
    if(bis==0,
        results = LLL(base),
        tmp = qflll(M, 3);
        n = length(tmp[,1]);
        results = [tmp[i, ] | i<-[1..n]]
        );

    return (results);
}

/* Réseau de Lagarias-Odlyzko */

Attaque_LO(pub,s, bis=0) = {
    n = length(pub);
    N = sqrtint(n)*(random(n) + 1);
    M = matid(n+1);
    tmp = [0 | i<-[1..(n+1)]];
    for(i=1,n, tmp[i] = N*pub[i]);
    tmp[n+1] = -N*s;
    M[,n+1] = tmp~;
    
    base = [M[i,] | i<-[1..(n+1)]];
    if(bis == 0, 
        results = LLL(base),
        tmp = qflll(mattranspose(M),3);
        tmp = mattranspose(tmp);
        n = length(tmp[,1]);
        results = [tmp[i, ] | i<-[1..n]]
        );

    /*Reconstitution du message */
    n = length(pub);
    x = vector(n);
    if(length(results)==0, print("Erreur Attaque "),
        for(i=1,n+1, 
            if(results[i][n+1] == 0,
                x = results[i][1..n];
                
                /* vérification */
                if(Chiffrement(pub,x)==s,test_val = 1,
                    if(Chiffrement(pub,-x)==s, x=-x; test_val = 1,
                        test_val = 0
                    );
                );
                if(test_val==1, 
                    valide = 1;
                    for(k=1, n,
                        if(x[k] != 0 && x[k] != 1, valide = 0; break);
                    );
                    if(valide==1,
                        print("Message = ", x);
                        return (1);
                    )
                );
            );
        );
    );
    
    return (0);
}

/* Coster, La Macchia, Odlyzko et Schnorr */
Attaque_MOS(pub,s, bis=0) = {
    n = length(pub);
    N = floor(sqrtint(n)/2) + random(100);
    M = matid(n+1);
    tmp = [0 | i<-[1..(n+1)]];
    for(i=1,n, tmp[i] = N*pub[i]);
    tmp[n+1] = N*s;
    
    M[,n+1] = tmp~;
    for(j=1,n, M[n+1,j] = 0.5);
    base = [M[i,] | i<-[1..(n+1)]];
    if(bis==0, 
        results = LLL(base),
        tmp = qflll(mattranspose(M));
        tmp = mattranspose(tmp);
        n = length(tmp[,1]);
        results = [tmp[i, ] | i<-[1..n]]
        );
    /* recherche du claire */
    liste1 =  [];
    liste2 = [];
    n = length(pub);
    for(k=1,n+1,
        tmp = results[k][1..n];
        if(abs(tmp[1..n]) == [0.5 | i<-[1..n]],
            liste1 = [if(tmp[l] == 0.5, 1, 0) | l<-[1..n]];
            liste2 = [if(tmp[l] != 0.5, 1, 0) | l<-[1..n]];
            break;
        );

    );
    if(length(liste1) != 0,
        if(Chiffrement(pub,liste1) == s, print("Message = ", liste1); return (1),
            if(Chiffrement(pub, liste2) == s, print("Message = ", liste2); return (1))
        )
    );
    return (0);
}


/* Joux et Sterne */
def recherche_claire(r, pub,s) = {

}
Attaque_JS(pub,s, bis=0) = {
    n = length(pub);
    N = n*(1+random(100));
    /* Matrice de la base */
    M = matrix(n+1,n+2);
    for(i=1, n+1, 
        for(j=1, n+1, 
            if(i==j, M[i,j] = n+1, M[i,j] = -1)
        );
    );
    tmp = vector(n+1);
    for(i=1,n, tmp[i] = N*pub[i]);
    tmp[n+1] = -N*s;
    M[,n+2] = tmp~;
    base = [M[i,] | i<-[1..(n+1)]];
    if(bis==0, 
        results = LLL(base),
        tmp = qflll(mattranspose(M),3);
        tmp = mattranspose(tmp);
        n = length(tmp[,1]);
        results = [tmp[i,] | i<-[1..n]]
        );
    /* recherche du claire */
    x=0;
    y=0;
    n=length(pub);
    
    for(i=1, n+1,
        if(results[i][n+2] == 0,
            v = results[i][1..(n+1)];
            for(k=1, n+1,
                good = 1;
                for(l=k+1, n+1,
                    if(v[k]*v[l] < 0,
                        if(abs(v[k] - v[l]) == n+2,
                            if(v[k]<0 && y == 0, y= v[k], if(v[k]>0 && x==0, x=v[k]));
                            if(v[l]<0 && y == 0, y= v[l], if(v[l]>0 && x==0, x=v[l]));
                            ,
                            good=0;
                        );
                    );
                    if(good==0, break);
                );
                if(good==0, x=0; y=0; break);
            );
            if(good == 1, break;)
        );
    );

    if(x*y >= 0, return(-1));

   /* Construction du message */
    found=1;
    X = vector(n);
    for(i=1, n,
        X[i] = floor((v[i] - y)/(x-y))
    );
    tmp = Chiffrement(pub,X);
    if(tmp != s, found=0 );
    
    if(found==0,
        found=1;
        X = vector(n);
        for(i=1, n,
            X[i] = (x-v[i])/(x-y);
        );
        tmp = Chiffrement(pub,X);
    );
    if(tmp != s, found=0, print("Message = ", X););
    
    return (found);
}


