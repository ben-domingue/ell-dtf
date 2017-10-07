c("Content.Area", "Year", "District.Number", 
  "School.Number", "Student.ID", "Grade", "Gender", 
  "Ethnicity", "In.School.Continuously", "In.District.Continuously", 
  "Free.Reduced.lunch.Status", 
  "Language.Background", "Language.Proficiency", "ELL.Program.Bilingual", 
  "ELL.Program.ESL", "In.ELL.Program.Continuously", "Gifted.and.Talented", 
  "IEP", "Primary.Disability", "Plan_504", "title1", "Homeless", 
  "Migrant", "Immigrant", "Expelled", "Accommodation", "Reason.Not.Tested", 
  "Scale.Score", "Proficiency.Level")->stud

for (year in 2002:2007) {
    data <- read.csv(paste("CSAP_RD_MT_",yr,".txt",sep=""),header=TRUE,as.is=TRUE,strip=TRUE)
    data$Grade <- as.numeric(data$Grade)
    for (g in 5:10) {
                                        #for (c in unique(data$Content.Area)) {
        for (c in c(1,3)) {
            print(c(year,g,c))
            if (year==2001 & c==3) next
            if (year<2005 & c==3 & g %in% c(3,4)) next
            tmp <- data[data$Grade==g & data$Content.Area==c,]
                                        # Parse the item responses
            ir1 <- ir2 <- NULL
            for (i in 46:51) {
                if (i<49) {
                    for (j in 1:45) {
                        ir1 <- cbind(ir1, as.numeric(substr(tmp[,i],j,j)))
                    }
                } else {
                    for (j in 1:10) {
                        ir2 <- cbind(ir2, as.numeric(substr(tmp[,i],j,j)))
                    }
                }
            }
                                        # Recode item responses
            if (c==1) {
                content <- "R"
                ir1[ir1<5] <- 0
                ir1[ir1>=5] <- 1
                ir2[ir2>6] <- NA
            } else {
                content <- "M"
                ir1[ir1<6 & ir1>0] <- -1
                ir1[ir1>=6|ir1==0] <- 1
                ir1[ir1==-1] <- 0
                ir2[ir2>5] <- NA
            }
            tmp1 <- apply(!is.na(ir1),2,sum)
            ir1 <- ir1[,tmp1>0]
            tmp1 <- apply(!is.na(ir2),2,sum)
            colnames(ir1)<-paste("mc_",1:ncol(ir1),sep="")
            ir2 <- ir2[,tmp1>0]
            colnames(ir2)<-paste("cr_",1:ncol(ir2),sep="")
            ir <- cbind(ir1,ir2)
            data.frame(ir)->ir
            tmp[,stud]->per
            data.frame(cbind(per,ir))->df
            ##
            save(df,file=paste("/home/bd/Dropbox/projects/psychometrics/ell_dtf/pub/dat_y",year,"_g",g,"_s",c,".Rdata",sep=""))
        }
    }
}

        ##     ##   Identify grade span
## gr <-5:9
## ##   Identify years
## yr <- 2002:2007
## ##   Sort the item map
## d <- d[order(d$year,d$grade,d$subject,d$type,d$num),]
## d1 <- d1[order(d1$year,d1$grade,d1$subject,d1$type,d1$num),]
## ##   Create a list to store the information from the item map
## ##   for the specified grades and years
## math <- math1 <- vector("list",length(gr)*length(yr))
## ##   Loop through the grades and years and populate the objects {math} and {math1}
## k <- 1
## for (i in 1:length(gr)) {
## 	for (j in 1:length(yr)) {
## 		math[[k]] <- d[d$grade==gr[i] & d$year==yr[j] & d$subject==3,]
## 		math[[k]]$m.num <- 1:nrow(math[[k]])
## 		math1[[k]] <- d1[d1$grade==gr[i] & d1$year==yr[j] & d1$subject==3,]
## 		math1[[k]]$m.num <- 1:nrow(math1[[k]])
## 		k <- k+1
## 	}
## }
## ##  Identify items to be excluded (math only)
## exclude <- NULL
## for (i in 1:length(math)) {
## 	if (nrow(math[[i]])==nrow(math1[[i]])) {
## 		next
## 	} else {
## 		exclude <- rbind(exclude, math[[i]][(math[[i]]$id%in%math1[[i]]$id)==FALSE,])
## 	}
## }

    

setwd("/home/bd/Dropbox/projects/psychometrics/ell_dtf/data")
for (yr in 2001:2008) {
    paste("CSAP_RD_MT_",yr,".txt",sep="")->fn
    read.csv(fn,colClasses="character")->x
    paste(x$Grade,x$Content.Area)->x$grade.subject
    split(x,x$grade.subject)->L
    proc<-function(tmp) {
        unique(x$Year)->yr
        unique(tmp$Grade)->gr
        unique(tmp$Content.Area)->sub
        grep("Constructed.Response",names(tmp),fixed=TRUE)->i1
        grep("Multiple.Choice",names(tmp),fixed=TRUE)->i2
        tmp[,i1]->cr
        tmp[,i2]->mc
        c("Content.Area", "Year", "District.Number", 
          "School.Number", "Student.ID", "Grade", "Gender", 
          "Ethnicity", "In.School.Continuously", "In.District.Continuously", 
          "Free.Reduced.lunch.Status", 
          "Language.Background", "Language.Proficiency", "ELL.Program.Bilingual", 
          "ELL.Program.ESL", "In.ELL.Program.Continuously", "Gifted.and.Talented", 
          "IEP", "Primary.Disability", "Plan_504", "title1", "Homeless", 
          "Migrant", "Immigrant", "Expelled", "Accommodation", "Reason.Not.Tested", 
          "Scale.Score", "Proficiency.Level")->stud
        tmp[,stud]->stud
        proc.cr<-function(y) {
                                        #text munging
            cr<-list()
            for (i in 1:ncol(y)) {
                ifelse(is.na(y[,i]),"",y[,i])->z
                strsplit(z,"")->z
                sapply(z,length)->N
                ## names(tab)->lens
                ## if (length(lens)!=1 | !("0" %in% lens)) {
                ##     lens[lens>0]->len
                ##     which(N==0)->index
                ##     if (length(index)>0) {
                ##         for (ii in index) z[[ii]]<-rep(NA,len)
                ##     }
                ##     do.call("rbind",z)->z
                ##     paste("cr",i,".",1:len,sep="")->colnames(z)
                ##     z->cr[[i]]
                ## }
                max(N,na.rm=TRUE)->M
                if (M>0) {
                    M-N -> del
                    which(del>0) ->  index
                    for (ind in index) c(z[[ind]],rep(NA,del[ind]))->z[[ind]]
                    do.call("rbind",z)->z
                    paste("cr",i,".",1:M,sep="")->colnames(z)
                    z->cr[[i]]
                }
            }
            do.call("cbind",cr)->cr
                                        #condition responses
            rem<-logical()
            for (i in 1:ncol(cr)) {
                cr[,i]->v
                ifelse(v==" ",NA,v)->v
                #print(sort(unique(v)))
                ifelse(v>5,NA,v)->v
                v->cr[,i]
                all(is.na(v))->rem[i]
            }
            cr[,!rem]->cr
            cr
        }
        proc.cr(cr)->cr
        proc.mc<-function(y) {
                                        #text munging
            mc<-list()
            for (i in 1:ncol(y)) {
                ifelse(is.na(y[,i]),"",y[,i])->z
                strsplit(z,"")->z
                sapply(z,length)->N
                ## if (length(tab)>2) stop("length")
                ## names(tab)->lens
                ## if (!(length(lens)==1 & "0" %in% lens)) {
                ##     lens[lens>0]->len
                ##     which(N==0)->index
                ##     if (length(index)>0) {
                ##         for (ii in index) z[[ii]]<-rep(NA,len)
                ##     }
                ##     do.call("rbind",z)->z
                ##     paste("mc",i,".",1:len,sep="")->colnames(z)
                ##     z->mc[[i]]
                ## }
                max(N,na.rm=TRUE)->M
                if (M>0) {
                    M-N -> del
                    which(del>0) ->  index
                    for (ind in index) c(z[[ind]],rep(NA,del[ind]))->z[[ind]]
                    do.call("rbind",z)->z
                    paste("cr",i,".",1:M,sep="")->colnames(z)
                    z->mc[[i]]
                }
            }
            do.call("cbind",mc)->mc
                                        #condition responses
            rem<-logical()
            for (i in 1:ncol(mc)) {
                mc[,i]->v
                ifelse(v==" ",NA,v)->v
                #print(sort(unique(v)))
                ifelse(v>4,1,0)->v
                v->mc[,i]
                all(is.na(v))->rem[i]
            }
            mc[,!rem]->mc
            mode.central<-function(x) {
                table(x)->tab
                names(tab)[which.max(tab)]
            }
            ##print(apply(mc,2,mode.central))
            mc
        }
        proc.mc(mc)->mc
        ##
        apply(mc,2,as.numeric)->mc
        apply(cr,2,as.numeric)->cr
        paste(yr,gr,sub,max(apply(cr,2,max,na.rm=TRUE)),round(cor(rowSums(mc,na.rm=TRUE),rowSums(cr,na.rm=TRUE)),2),sep="___")->txt
        print(txt)
        data.frame(stud,mc,cr)->df
        save(df,file=paste("/home/bd/Dropbox/projects/psychometrics/ell_dtf/pub/dat_y",yr,"_g",gr,"_s",sub,".Rdata",sep=""))
        NULL
    }
    for (i in 1:length(L)) try(proc(L[[i]]))
}


