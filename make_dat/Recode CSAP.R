### Parse CSAP datafiles ###

####################################################################
##########  Stuff that needs to be updated before running ##########
####################################################################

# Path where raw data folder and output folders are stored
path.in <- "D:/My Documents/Dropbox/carnegie/Data/csap_original_data/"
path.out <- "D:/My Documents/Dropbox/carnegie/Data/csap_original_data/Parsed/"

##   Location of the file ItemMap.txt
path.map <- "D:/My Documents/Dropbox/data/item_map/"

####################################################################

##   Import the complete item map
d <- read.table(paste(path.map,"ItemMap.txt",sep=""),sep="\t",header=T)
##   Import the math item map
d1 <- read.table(paste(path.map,"MathItemMap.txt",sep=""),sep="\t",header=T)

##   Identify grade span
gr <-5:9

##   Identify years
yr <- 2002:2007

##   Sort the item map
d <- d[order(d$year,d$grade,d$subject,d$type,d$num),]
d1 <- d1[order(d1$year,d1$grade,d1$subject,d1$type,d1$num),]

##   Create a list to store the information from the item map
##   for the specified grades and years
math <- math1 <- vector("list",length(gr)*length(yr))

##   Loop through the grades and years and populate the objects {math} and {math1}
k <- 1
for (i in 1:length(gr)) {
	for (j in 1:length(yr)) {
		math[[k]] <- d[d$grade==gr[i] & d$year==yr[j] & d$subject==3,]
		math[[k]]$m.num <- 1:nrow(math[[k]])
		math1[[k]] <- d1[d1$grade==gr[i] & d1$year==yr[j] & d1$subject==3,]
		math1[[k]]$m.num <- 1:nrow(math1[[k]])
		k <- k+1
	}
}

##  Identify items to be excluded (math only)
exclude <- NULL
for (i in 1:length(math)) {
	if (nrow(math[[i]])==nrow(math1[[i]])) {
		next
	} else {
		exclude <- rbind(exclude, math[[i]][(math[[i]]$id%in%math1[[i]]$id)==FALSE,])
	}
}


## Parse CSAP math item responses
for (k in yr) {
	data <- read.csv(paste(path.in,"CSAP_RD_MT_",k,".txt",sep=""),header=TRUE,as.is=TRUE,strip=TRUE)
	data$Grade <- as.numeric(data$Grade)

	# Create a parsed file for each grade and content area
	#for (g in unique(data$Grade)) {
	for (g in 5:9) {
		#for (c in unique(data$Content.Area)) {
		for (c in 3) {
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
			ir2 <- ir2[,tmp1>0]
			ir <- cbind(ir1,ir2)
			
			if (c==3) {
				# Exclude items (math only)
				if (nrow(exclude[exclude$year==k & exclude$subject==c & exclude$grade==g,])>0) {
					ex <- exclude$m.num[exclude$year==k & exclude$subject==c & exclude$grade==g]
					ir <- ir[,-ex]
				}
				colnames(ir) <- c(paste("MC",d1$num[d1$grade==g & d1$year==k & d1$subject==c & d1$type==1],sep=""),
				paste("CR",d1$num[d1$grade==g & d1$year==k & d1$subject==c & d1$type==2],sep=""))
			} else {
				colnames(ir) <- c(paste("MC",d$num[d$grade==g & d$year==k & d$subject==c & d$type==1],sep=""),
				paste("CR",d$num[d$grade==g & d$year==k & d$subject==c & d$type==2],sep=""))
			}
			write.table(ir, paste(path.out,"c",c,"_",k,"_",g,"_responses.txt",sep=""),row.names=F,sep=" ")	
			
		}
	}
}


