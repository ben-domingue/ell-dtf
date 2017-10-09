list.files()->lf
##grep("y2001",lf)->i1
NULL->i1
grep("y2002",lf)->i2
lf[-c(i1,i2)]->lf
out<-list()


for (fn in lf) {
    print(fn)
    load(fn)
    ##
    as.numeric(df$Scale.Score)->df$ss
    grep("^cr",names(df))->cr
    grep("^mc",names(df))->mc
    df[,c(mc,cr)]->ir
    rowSums(ir,na.rm=TRUE)->df$rs
    rowSums(df[,mc],na.rm=TRUE)->df$rs.mc
    rowSums(df[,cr],na.rm=TRUE)->df$rs.cr
    ##
    ifelse(df$In.ELL.Program.Continuously=="U",NA,df$In.ELL.Program.Continuously)->df$ell
    ifelse(df$ell=="Y",1,0)->df$ell
    df[!is.na(df$ell),]->df
    ##
    df[,c("rs.mc","rs.cr","rs","ss")]->tmp
    cor(tmp,use='p')->out[[fn]]
}

f<-function(x) x[lower.tri(x,diag=FALSE)]
lapply(out,f)->tab
do.call("rbind",tab)->tab
barplot(tab,beside=TRUE)
