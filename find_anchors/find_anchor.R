set.seed(10123010)
fun<-function(x) {
    x[,1]->gr
    x[,-1]->resp
    J<-nrow(resp)
    K<-ncol(resp)
    y<-list()
    jj<-list()
    kk<-list()
    for (i in 1:ncol(resp)) {
        resp[,i]->y[[i]]
        1:nrow(resp)->jj[[i]]
        rep(i,nrow(resp))->kk[[i]]
    }
    do.call("c",y)->y
    do.call("c",jj)->jj
    do.call("c",kk)->kk
    #
    L<-list(J=J,K=K,N=length(y),jj=jj,kk=kk,y=y,g=gr)
                                        #
    library(rstan)
    rstan_options(auto_write = TRUE)
    options(mc.cores = parallel::detectCores())
                                        #
    fit <- stan(file='irt_ml.stan',data=L,iter=1000,chains=1) #you need to be in right working directory
    ##save(fit,file=paste("colfit-",nm,".Rdata",sep=""))
    fit
}

load(file="/home/bd/Dropbox/projects/psychometrics/ell_dtf/pub/dat_y2004_g7_s1.Rdata") ##will need to modify for local machine
ifelse(df$In.ELL.Program.Continuously=="U",NA,df$In.ELL.Program.Continuously)->df$ell
ifelse(df$ell=="Y",2,1)->df$ell
df[!is.na(df$ell),]->df
grep("^mc",names(df))->index
df[,c("ell",names(df)[index])]->mc

per<-numeric()
for (i in 2:ncol(mc)) {
    sum(is.na(mc[,i]))/nrow(mc)->per[i-1]
}
mc[,c(TRUE,per<.05)]->mc
mc[rowSums(is.na(mc))==0,]->mc
sample(1:nrow(mc),2000)->index
mc[index,]->mc
fun(mc)->fit






anch<-function(fit) {
    extract(fit)$beta->B
    colMeans(B[,,1])->b1
    colMeans(B[,,2])->b2
    f<-function(x) {
        apply(x,2,quantile,c(.025,.975))->qu
    }
    f(B[,,1])->ci1
    f(B[,,2])->ci2
    overlap<-function(ci1,ci2) {
        ci1[1] >= ci2[1] & ci1[1]<=ci2[2] -> test1
        ci1[2] >= ci2[1] & ci1[2]<=ci2[2] -> test2
        ci2[1] >= ci1[1] & ci2[1]<=ci1[2] -> test3
        ci2[2] >= ci1[1] & ci2[2]<=ci1[2] -> test4
        (test1 | test2) | (test3 | test4)
    }
    anchor<-logical()
    for (i in 1:ncol(ci1)) overlap(ci1[,i],ci2[,i])->anchor[i]
    anchor
}
anch(fit)->anchor



pf<-function(fit,plot=FALSE) {
    extract(fit)$beta->B
    colMeans(B[,,1])->b1
    colMeans(B[,,2])->b2
    f<-function(x) {
        apply(x,2,quantile,c(.025,.975))->qu
    }
    f(B[,,1])->ci1
    f(B[,,2])->ci2
    rbind(ci1,ci2)->ends
    t(ends)->ends
    range(ends)->xl
    anch(fit)->anchor
    comp<-numeric()
    for (i in 1:length(b1)) compare(B[,i,1],B[,i,2])->comp[i]
    if (plot) {
        par(mgp=c(2,1,0),mar=c(3.2,3,2,2.5))
        plot(NULL,xlim=xl,ylim=c(0,1+nrow(ends)),xlab="parameter estimates",ylab="",yaxt="n")
        mtext(side=2,1:nrow(ends),at=1:nrow(ends),las=1,line=.2)
        for (i in 1:nrow(ends)) {
            ifelse(anchor[i],"black","red")->col
            segments(ends[i,1],i-.1,ends[i,2],i-.1,col=col,lwd=2)
            segments(ends[i,3],i+.1,ends[i,4],i+.1,col=col,lwd=2)
            #text(b1[i],i-.4,round(b1[i],digits=2),cex=.7)
            #text(b2[i],i+.4,round(b2[i],digits=2),cex=.7)
        }
        ##
        mtext(side=4,line=.2,las=1,round(comp,digits=2),at=1:nrow(ends),cex=.7)
    }
    data.frame(cbind(b1,b2,anchor,comp))
}
pf(fit,plot=TRUE)


est.fun<-function(resp,anchor
                  ) {
    co<-function(co) {
        co[-length(co)]->co
        do.call("rbind",co)->co
        co[,1:3]
    }
    base<-c("free_means","free_var")
    rowSums(is.na(resp))==0 -> index
    resp[index,]->resp
    ifelse(resp[,1]==2,"e","n")->gr
    models <- paste('F1 = 1-',ncol(resp)-1,sep="")
    library(mirt)
    list(NCYCLES=2000)->tech
    multipleGroup(TOL=.00005,technical=tech,resp[,-1],group=gr,method="EM",itemtype="Rasch",models,invariance=c(base,"slopes","intercepts"),verbose=FALSE)->m1
    #cbind(co(coef(m1)$P),co(coef(m1)$T))->tt
    #dump("tt","")#print(tt)
    multipleGroup(TOL=.00005,technical=tech,resp[,-1],group=gr,method="EM",itemtype="Rasch",models,invariance=c(base,anchor),verbose=FALSE)->m2
    #cbind(co(coef(m2)$P),co(coef(m2)$T))->tt
    #dump("tt","")#print(tt)
    #tcc(co(coef(m1)$P))->sc
    list(m1=m1,m2=m2)
}    
names(mc)[-1][anchor]->anchor.names
est.fun(mc,anchor.names)->out

fun<-function(l) {
    l$m1->m1
    l$m2->m2
    coef(m1)$n$GroupPars[1]-coef(m1)$e$GroupPars[1]->e.prior
    sqrt(coef(m1)$n$GroupPars[2])->s.prior
    coef(m2)$n$GroupPars[1]-coef(m2)$e$GroupPars[1]->e.post
    sqrt(coef(m2)$n$GroupPars[2])->s.post
    c(coef(m1)$n$GroupPars,coef(m1)$e$GroupPars,coef(m2)$n$GroupPars,coef(m2)$e$GroupPars)->zz
    write.table(zz,"")
    c(e.prior/s.prior,e.post/s.post)
}
fun(out)


