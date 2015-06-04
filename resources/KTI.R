KTI<-function(KTDM_mat, SIGN){
    KTDM_mat = abs(KTDM)
    SIGN = sign(KTDM)
    n = dim(KTDM_mat)[1]
    W = matrix(data = 0,nrow = n,ncol = n)
    colnames(W) = rownames(W) = colnames(KTDM_mat)
    pb = txtProgressBar(min = 1,max = n,style = 3)
    for(i in 1:n){
        for(j in 1:n){
            if(SIGN[i,j]>0){
                #confronto KTDM_mat[i,j] con la distribuzione di valori positivi
                pos_index_i = which(SIGN[i,]==1)
                mi = mean(KTDM_mat[i,pos_index_i])
                si = sd(KTDM_mat[i,pos_index_i])
                
                pos_index_j = which(SIGN[,j]==1)
                mj = mean(KTDM_mat[pos_index_j,j])
                sj = sd(KTDM_mat[pos_index_j,j])
                
                zi = max(0, ((KTDM_mat[i,j]-mi)/si))
                zj = max(0, ((KTDM_mat[i,j]-mj)/sj))
                
                W[i,j] = SIGN[i,j] * sqrt(zi^2+ zj^2)        
            }else{
                #confronto KTDM_mat[i,j] con la distribuzione di valori negativi
                neg_index_i = which(SIGN[i,]==-1)
                mi = mean(KTDM_mat[i,neg_index_i])
                si = sd(KTDM_mat[i,neg_index_i])
                
                neg_index_j = which(SIGN[,j]==-1)
                mj = mean(KTDM_mat[neg_index_j,j])
                sj = sd(KTDM_mat[neg_index_j,j])
                
                zi = min(0, ((KTDM_mat[i,j]-mi)/si))
                zj = min(0, ((KTDM_mat[i,j]-mj)/sj))
                
                W[i,j] = SIGN[i,j] * sqrt(zi^2+ zj^2)  
            }
        }
        setTxtProgressBar(pb,i)
    }
    close(pb)
    return(W)
}
