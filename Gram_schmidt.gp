/*
    Norme euclidienne carree
*/
Norm_carree(u) = {
    carre_u = [x*x | x<-u ];
    resultat = [1 |  x<-u]*carre_u~;
    return (resultat);
};
/*
    Produit scalaire de u et v
*/
Prod_scalaire(u,v) = {
    n = length(u);
    if(n!=length(v), return (-1));
    produit = [u[i]*v[i] | i<-[1..n]];
    return ([1 |  x<-[1..n]]*produit~);
}

/*
    Retourne l'entier le plus proche de x
*/
Plus_proche_entier(x) = {
    n = floor(x);
    if(x - n + 0.5 >= 1, return (n+1));
    return (n);
}

/*
    Orthogonalisation de Gram Schmidt
*/
Gram_schmidt(v) = {
    n = length(v);
    W = [v[i] | i<-[1..n]];
    M = matrix(n,n);
    for(i=1,n,
        M[i,i] = 1;
    );

    for(i=1,n,
        m = i-1;
        mu_i_j = [Prod_scalaire(v[i],W[k])/Norm_carree(W[k]) | k<-[1..m]];
        tmp = [mu_i_j[k]*W[k] | k<-[1..m]];
        somme = [];
        for(k=1,m,
            if(k==1,somme=tmp[k],somme = somme+tmp[k]);
        );
        if(length(somme)==0, w_i = v[i],
            w_i = v[i]-somme;
        );
        W[i] = w_i;
        for(j=1,m,
            M[i,j] = mu_i_j[j];
        );
    );

    return ([W,M]);
}

/*
    Tests

v = [[1,1,1,1], [1,1,-1,-1], [0,-1,2,1]];
[v_orth,M] = Gram_schmidt(v);
print("M = ", M);
print("v_ort : ", v_orth);

*/