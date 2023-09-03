read("Gram_schmidt.gp");

LLL(V) = {
    len_v = length(V);
    /* etape 1 */
    w = [x | x<-V];
    
    /* etape 2 */
    [w_orth, N] = Gram_schmidt(V);

    /* etape 3*/
    i = 2;
    while(i<=len_v,
        /* etape 3.a */
        forstep(j=i-1,1,-1,
            mu = Plus_proche_entier(N[i,j]);
            w[i] = w[i] - mu*w[j];
            for(k=1,j,
                N[i,k] = N[i,k] - mu*N[j,k];
            );
        );
        /* etape 3.b */
        if(i>=2 && Norm_carree(w_orth[i-1]) > 2*Norm_carree(w_orth[i]), 
            /* etape 3.b.i */
            alpha = N[i,i-1];
            beta = alpha*Norm_carree(w_orth[i-1])/(Norm_carree(w_orth[i])+alpha*alpha*Norm_carree(w_orth[i-1]));
            
            /* etape 3.b.ii */
            tmp = w_orth[i] + alpha*w_orth[i-1];
            w_orth[i] = w_orth[i-1] - beta*tmp;
            w_orth[i-1] = tmp;
            /* etape 3.b.iii */
            tmp = w[i];
            w[i] = w[i-1];
            w[i-1] = tmp;
            tmp = N[i,];
            N[i,]  = N[i-1,];
            N[i-1,] = tmp; 
            /* etape 3.b.iv */
            for(k=i-1, len_v,
                tmp = beta*N[k,i-1] + (1-alpha*beta)*N[k,i];
                N[k,i] = N[k,i-1] - alpha*N[k,i];
                N[k,i-1] = tmp;
            );
            /* etape 3.b.v */
            i--;
        , i++)
    );
    return (w);
}

