read("sacados.gp");

/*
 Test LLL sur example page 505
    Matrice = matid(7);
    Matrice[,7] = [-366,-385,-392,-401,-422,-437,1215]~;
    base = [Matrice[i,] | i<-[1..7]];
    results = LLL(base);
    print(results);
*/

get_max(liste) = {
    maxi = liste[1];
    for(i=1,length(liste), 
        if(maxi < liste[i], maxi = liste[i]);
    );
    return (maxi);
}

params(n,limit=100) = {
    /* génération clefs */
    infos = Gen_keys(n,limit);

    /* génération message */
    x = Gen_message(n);

    /* Chiffrement */
    pub = infos[4];
    s = Chiffrement(pub, x);

    return ([infos, x, s]);
}

prob_generator(n, a, b) = {
    k = random()%(floor(n/a) - floor(n/b)) + floor(n/b);
    if(k < n, limit = 2^k, limit = 2^(k-n));
    print("k = ", k);

    prob = params(n, limit);
    pub = prob[1][4]; 
    /* densité du problème */
    logs = [if(pub[i]!=0, log(pub[i])/log(2)) | i<-[1..n] ];
    maxi = get_max(logs);

    print("a = ", a, " / b = ", b);
    print("densite = ", n/maxi);
    return (prob);
}



test_attacks(n,limit=100) = {
    param = params(n,limit);

    pub = param[1][4];
    x = param[2];
    print("Message généré : ", x); 
    s = param[3];

    /* densité du problème */
    logs = [if(pub[i]!=0, log(pub[i])/log(2)) | i<-[1..n] ];
    maxi = get_max(logs);

    /* Attaque_LLL */
    results = Attaque_LLL(pub, s);
    
    if(length(results)==0, print("Erreur Attaque "),
        reussi = 0;
        vec = [1 | i<-[1..n]];
        Norm = -1;
        for(i=1,n+1, 
            tmp =  Norm_carree(results[i][1..n]);
            if(Norm <=tmp, vec = results[i][1..n]; Norm = tmp);
            if(x==results[i][1..n], 
                print("Message trouvé : ", results[i][1..n]);
                reussi = 1;
                break;
            );
        );
        if(reussi == 0,   
            print("Message non trouvé");
            print("Plus petit vecteur trouvé : ", vec);
        );
     );
    
    if(maxi!=0,density = n/maxi;
        print("Densité : ", density);
    );
}

count(r, x, cpt=0) = {
    n = length(x);
    if(length(r)==0, print("Erreur Attaque "),
        for(i=1,n+1, 
            if(x==r[i][1..n], 
                cpt++;
                return (1);
            );
        );
    );
    return (0);
}
test_precision_anc(n, m=10,limit=100) = {
    compteur = 0;

    stats = vector(m);
    index = 1;
    for(k=1,m,
        param = params(n,limit);

        pub = param[1][4];
        x = param[2];
        s = param[3];

        /* densité du problème */
        logs = [if(pub[i]!=0, log(pub[i])/log(2)) | i<-[1..n]];
        maxi = get_max(logs);
        densite = n/maxi;
        stats[index] = densite ;

        if(attack == 1,
            /* Attaque_LLL */
            results = Attaque_LLL(pub, s);
            if(count(results, x, compteur),
                compteur++;
            );
            ,
            if(attack == 2, 
                results = Attaque_LLL_bis(pub,s);
                if(count(results, x, compteur),
                    compteur++;
                );
                ,

                if(attack == 3, 
                    results = Attaque_MOS(pub,s);
                    if(results[1] == x || results[2]==x, 
                        compteur++;
                    );
                )
            )
        )
    );

    
    precision = 100*compteur / m;

    return ([stats, precision]);
}
test_precision(n, m=10,limit=100) = {
    compteur = 0;
    compteur_bis = 0;
    cpt_mos = 0;

    stats = matrix(4,m+1);
    stats[1,1] = "densite";
    for(k=1,m,
        line = 1;
        col = k+1;
        param = params(n,limit);

        pub = param[1][4];
        x = param[2];
        s = param[3];

        /* densité du problème */
        logs = [if(pub[i]!=0, log(pub[i])/log(2)) | i<-[1..n]];
        maxi = get_max(logs);
        densite = n/maxi;
        stats[line,col] = densite;
        line++;

        /* Attaque_LLL */
        results = Attaque_LLL(pub, s);
        if(count(results, x, compteur),
            stats[line, col] = 1;
            compteur++;
        );
        line++;

        r_bis = Attaque_LLL_bis(pub,s);
        if(count(r_bis, x, compteur_bis),
            stats[line, col] = 1;
            compteur_bis++;
        );
        line++;

        r_mos = Attaque_MOS(pub,s);
        if(r_mos[1] == x || r_mos[2]==x, 
            cpt_mos++;
            stats[line, col] = 1;
        );
    );

    print("1./ ", compteur, " Reussite sur ", m, " soit  " , 100*compteur/m, "%");
    print("2./ ", compteur_bis, " Reussite sur ", m, " soit  " , 100*compteur_bis/m, "%");
    print("3./ ", cpt_mos, " Reussite sur ", m, " soit  " , 100*cpt_mos/m, "%");

    return (stats);
}



Test_Att_Lagarias_Odlyzko(n, m=10) = {
    densites = vector(m*8);
    precisions = vector(m*8);
    stats = vector(m);
    ind = 1;
    limits = [10, 20, 30, 80, 100, 200, 500, 1000];
    for(i=1, 8,
        for(k=1, m,
            param = params(n,limits[i]);

            pub = param[1][4];
            x = param[2];
            s = param[3];

            /* densité du problème */
            logs = [if(pub[i]!=0, log(pub[i])/log(2)) | i<-[1..n]];
            maxi = get_max(logs);
            densite = n/maxi;

            /* Attaque_LLL */

            r_bis = Attaque_LLL_bis(pub,s); 
            if(count(r_bis, x),
                stats[ind] = densite;
                ind++;
            );
        );
    );

}


