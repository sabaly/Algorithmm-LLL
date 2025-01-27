compute_g(N,f, m) = {
    d = length(Vec(f)) - 1; /* degré de f*/
    list_g = vector(d*(m+1));
    ind = 1;
    for(v=0, m, 
        for(u=0, d-1, 
            list_g[ind] = N^(m-v) * x^u * f^v;
            ind++;
        );
    );

    return (list_g);
}

compute_base(N, d, X, list_g) = {
    /*begin = gettime();*/
    n = length(list_g);
    c = length(Vec(list_g[n]));
    M = matrix(n,c);
    line = 1;
    
    while(line <= n, 
        list_g[line] = subst(list_g[line], x,  x*X);
        tmp = Vecrev(list_g[line]);
        for(i=1,length(tmp),
            M[line, i] = tmp[i];
        );
        line++;
    );
    /*print("duree : ", gettime() - begin);*/
    return (M);
}

norm_pol(p) = {
    p_vec = Vec(p);
    norm_p = 0;
    for(i=1, length(p_vec), norm_p += p_vec[i]^2);

    return (sqrt(norm_p));
}

compute_h(base, N, m, w) = {
    /* calcul du determinant */
    line = length(base);
    det=1;
    for(i=1, line,
        det*= base[i,i];
    );

    if(2^(w/4)*det^(1/w) < N^m / sqrt(w), 
        /* appliquer LLL à la base */
        print("passed");
        base_result = qflll(mattranspose(base), 3);
        base_result = mattranspose(base_result);

        return (Polrev(base_result[1,]));
    );
    return(-1);
}

is_integer(x) = {
    if((x - floor(x)) == 0, return (1));

    return (0);
}

is_in(x, liste, ind_limit=0) = {
    if(ind_limit <= 0, return (0));

    for(k=1, ind_limit,
        if(liste[k] == x, return (1));
    );

    return(0);
}

/*
    Retourne l'entier le plus proche de x
*/
Plus_proche_entier(x) = {
    n = floor(x);
    if(x - n + 0.5 >= 1, return (n+1));
    return (n);
}

compute_solutions(real_roots, X) = {
    tmp_sols = vector(length(real_roots));
    ind = 0;
    for(i = 1, length(real_roots), 
        if(real_roots[i] == 0 && !is_in(0, tmp_sols, ind), ind++,
            val = floor(real_roots[i]);
            if(abs(real_roots[i])  < X && is_integer(real_roots[i]) && !is_in(val, tmp_sols, ind),
                ind++;
                tmp_sols[ind] = val;
            );
        );
    );
    if(ind == 0, return ([]));
    ret = tmp_sols[1..ind];

    return (ret);
}

Coppersmith(N, f, epsilon=1.E-2) = {
    d = length(Vec(f))-1; /* degre de f*/
    
    X = floor(N^(1/d)); 
    epsilon = 1/d - log(X)/log(N);

    if(epsilon == 0, m = 0,
        m =  Plus_proche_entier((1-d*epsilon)*(d-1)/(d*d*epsilon));
    );
    print("d = ", d, " / epsi = ", epsilon, "/ Premier m = ", m);
    stop = 0;
    indice = 0;
    while(stop == 0,
        w = d*(m+1);
        print("N = ", N, " / m = ", m, " / X = ", X);
        
        /* calcul du determinant */
        left = 2^(w/4)*X^((w-1)/2)*N^(m/2);
        
        if(left < N^m / sqrt(w),
            print("ici");
            list_g = compute_g(N, f, m);
            base = compute_base(N, d, X, list_g);
            base_result = qflll(mattranspose(base), 3);
            base_result = mattranspose(base_result);
            
            h = Polrev(base_result[1,]);
        
            indice = 0;
            if(h!=-1,
                sols = vector(d);
                h = subst(h, x, x/X);
                
                /* calcul les racines réels de h ici */
                racines_reels = polrootsreal(h);
                /*print("reels =", racines_reels);*/

                if(length(racines_reels) != 0,
                    t_sols = compute_solutions(racines_reels, X);
                    /*print("t_sols : ", t_sols);*/
                    for(i=1, length(t_sols), 
                        if((subst(f, x, t_sols[i]) % N) == 0 && !is_in(t_sols[i], sols, indice), 
                            indice++;
                            sols[indice] = t_sols[i];
                        );
                    );
                );
            );
            if(indice!=0, stop = 1, m++);,
            m++;
        );
        if(m==0, stop=1);
    );

    if(indice == 0, return(-1));

    return (sols[1..indice]);
}

default(parisize, "1000M");

