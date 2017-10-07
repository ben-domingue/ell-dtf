list.files()->lf
##grep("y2001",lf)->i1
NULL->i1
grep("y2002",lf)->i2
lf[-c(i1,i2)]->lf
out<-list()

for (fn in lf) {
    print(fn)
    load(fn)
    as.numeric(df$Scale.Score)->df$ss
    grep("^cr",names(df))->cr
    grep("^mc",names(df))->mc
    df[,c(mc,cr)]->ir
    rowSums(ir,na.rm=TRUE)->df$rs
                                        #print(cor(rs,as.numeric(df$Scale.Score),use='p'))
                                        #for (nm in c("Language.Background Language.Proficiency","ELL.Program.Bilingual","ELL.Program.ESL","In.ELL.Program.Continuously")) print(table(df[[nm]]))
    ifelse(df$In.ELL.Program.Continuously=="U",NA,df$In.ELL.Program.Continuously)->df$ell
    ifelse(df$ell=="Y",1,0)->df$ell
    df[!is.na(df$ell),]->df
    es<-function(x,y) {
        mean(x[y==1],na.rm=TRUE)-mean(x[y==0],na.rm=TRUE) -> m
        sd(x[y==0],na.rm=TRUE) -> s
        m/s
    }
    es(df$ss,df$ell) -> es.ss
    es(df$rs,df$ell) -> es.rs
    c(unique(df$Content.Area),cor(df$ss,df$rs,use='p'),es.ss,es.rs)->out[[fn]]
}
do.call("rbind",out)->out


by(out[,2],out[,1],summary)
plot(out[,3],out[,4])
