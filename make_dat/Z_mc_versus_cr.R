list.files()->lf
grep("y2001",lf)->i1
grep("y2002",lf)->i2
lf[-c(i1,i2)]->lf


eff.size<-function(fn) {
    print(fn)
    load(fn)
    ifelse(df$In.ELL.Program.Continuously=="U",NA,df$In.ELL.Program.Continuously)->df$ell
    ifelse(df$ell=="Y","yes","no")->df$ell
    df[!is.na(df$ell),]->df
    grep("^mc",names(df))->mc
    grep("^cr",names(df))->cr
    df[,c(mc,cr)]->resp
    per<-numeric()
    for (i in 1:ncol(resp)) {
        sum(is.na(resp[,i]))/nrow(resp)->per[i]
    }
    resp[,per<.1]->resp
    resp[rowSums(is.na(resp))==0,]->resp
    sample(1:nrow(resp),10000)->index
    resp[index,]->resp
    df$ell[index]->ell
    ##
    base<-c("free_means","free_var")
    library(mirt)
    list(NCYCLES=5000)->tech
    fun<-function(m1) {
        coef(m1)$yes$GroupPars[1]-coef(m1)$no$GroupPars[1]->e.prior
        sqrt(coef(m1)$no$GroupPars[2])->s.prior
        e.prior/s.prior
    }
    ##just mc/rasch
    grep("^mc",names(resp))->mc
    resp[,mc]->mc
    models <- paste('F1 = 1-',ncol(mc)-1,sep="")
    multipleGroup(TOL=.00005,technical=tech,mc,group=ell,method="EM",itemtype="Rasch",models,invariance=c(base,"slopes","intercepts"),verbose=FALSE)->m1
    fun(m1)->e1
    ##mc/3pl
    multipleGroup(TOL=.00005,technical=tech,mc,group=ell,method="EM",itemtype="3PL",models,invariance=c(base,"slopes","intercepts"),verbose=FALSE)->m2
    fun(m2)->e2
    ##all/rasch
    models <- paste('F1 = 1-',ncol(resp)-1,sep="")
    multipleGroup(TOL=.00005,technical=tech,resp,group=ell,method="EM",itemtype="Rasch",models,invariance=c(base,"slopes","intercepts"),verbose=FALSE)->m3
    fun(m3)->e3
    ##all/3pl
    apply(resp,2,max)->tab
    ifelse(tab==1,"3PL","gpcm")->it
    multipleGroup(TOL=.00005,technical=tech,resp,group=ell,method="EM",itemtype=it,models,invariance=c(base,"slopes","intercepts"),verbose=FALSE)->m4
    fun(m4)->e4
    ##
    c(e1,e2,e3,e4)
}
out<-list()
for (i in 1:length(lf)) eff.size(lf[i])->out[[lf[i] ]]

do.call("rbind",out)->tab

