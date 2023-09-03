read("sacados/sacados.gp");

get_max(liste) = {
    maxi = liste[1];
    for(i=1,length(liste), 
        if(maxi < liste[i], maxi = liste[i]);
    );
    return (maxi);
}

params(n,limit) = {
    /* génération clefs */
    infos = Gen_keys(n,limit);

    /* génération message */
    x = Gen_message(n);

    /* Chiffrement */
    pub = infos[4];
    s = Chiffrement(pub, x);

    return ([infos, x, s]);
}

count(r, x) = {
    n = length(x);
    if(length(r)==0, print("Erreur Attaque "),
        for(i=1,n+1, 
            if(x==r[i][1..n] || x=-r[i][1..n], 
                return (1);
            );
        );
    );
    return (0);
}

test_attack(attack, n,  limit, m, bis=0) = {
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
        index++;
        /* Attaque_LLL */
        if(attack == 1,
            results = Attaque_LO(pub, s, bis);
            if(results==1,
                compteur++;
            ),
            if(attack == 2, 
                results = Attaque_MOS(pub,s, bis);
                if(results==1,
                    compteur++;
                ),
                if(attack == 3, 
                    results = Attaque_JS(pub,s, bis);
                    if(results==1, 
                        compteur++;
                    )
                )
            )
        );
    );

    
    p = 100*compteur/m;

    return ([stats, p]);
}
stats_efficacite(n, k, attack=1, bis=0, m=10) = {
    final_stats = matrix(k, 2);
    file = concat("attack", attack);
    if(bis==1, file = concat(file, "_bis"));
    file = concat(file, ".txt");
    write(file, n);
    line = 1;
    for(i = 1, k,
        limit = 2^i;
        results = test_attack(attack, n, limit, m, bis);
        densite_sorted = vecsort(results[1]);
        densite_med = densite_sorted[floor((m+1)/2)];
        p = results[2];
        write1(file, densite_med);
        write1(file, "\t");
        write(file, p);
        final_stats[line, ] = [densite_med, p];
        line++;
    );

    return (final_stats);
}



