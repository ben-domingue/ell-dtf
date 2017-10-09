##not yet ready

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
    fit <- stan(file='/home/bd/Dropbox/projects/colombia/community_colleges/src/anchor/irt-ml.stan',data=L,iter=4000,chains=5)
    ##save(fit,file=paste("colfit-",nm,".Rdata",sep=""))
}

load(file="dat_y2004_g04_s1.Rdata")
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
sample(1:nrow(mc),5000)->index
mc[index,]->mc
fun(mc)





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
    ifelse(resp[,1]==2,"T","P")->gr
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



## for (nm in c("QR_String","CR_String")) {
##     load(paste("/home/domingue/colombia/community_colleges/",nm,".Rdata",sep=""))
##     rowSums(is.na(tmp))==0->test
##     tmp[test,]->tmp
##     sample(1:nrow(tmp),10000)->index
##     tmp[index,]->x
##     fun(x,nm)
## }



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
compare<-function(b1,b2) {
    mean(b1)->m1
    mean(b2)->m2
    sd(b1)->s1
    sd(b2)->s2
    abs(m1-m2)/sqrt(s1^2+s2^2)
}




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


library(rstan)
anch.out<-list()
plot<-TRUE
if (plot) {
    pdf("/tmp/fig.pdf",width=7,height=7)
    par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,2,2))
}
for (nm in c("QR_String","CR_String")) {
    load(paste("colfit-",nm,".Rdata",sep=""))
    pf(fit,plot=plot)->anch.out[[nm]]
    if (plot) mtext(side=3,line=0.2,gsub("_String","",nm))
}
if (plot) legend("bottomright",c("anchor","variant"),lty=c(1),lwd=2,col=c("black","red"),bty="n")
dev.off()

f<-function(nm,out) {
    L<-list()
                                        #
    out[[nm]]->x
    which.min(x$comp)->i
    paste(nm,i,sep="")->L$one
    paste(nm,which(x$anchor==1),sep="")->L$anch
    order(x$comp)->index
    paste(nm,index,sep="")->L$all
    L
}
lapply(names(anch.out),f,anch.out)->anchor
names(anch.out)->names(anchor)
    
save(anchor,file="anchor-stan.Rdata")
