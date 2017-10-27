read.csv("summarize_language_proficiency.csv")->x
split(x,x$test_grade_year)->tmp
f<-function(x) {
    x$count/sum(x$count)->x$freq
    x
}
lapply(tmp,f)->tmp
data.frame(do.call("rbind",tmp))->x
factor(x$Language.Proficiency,exclude=NULL)->x$Language.Proficiency



par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))
boxplot(mean_scale_score~Language.Proficiency,x,ylab="mean std scale score")
boxplot(freq~Language.Proficiency,x,ylab="proportion")
