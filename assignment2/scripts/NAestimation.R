library(Hmisc)
head_train<-read.csv('head.csv', na.strings='NULL')
#head_t<-head_train[]
#boxplot(head_train[,1], main='test')
#print(colnames(head_train))
# pdf('boxplots.pdf')
# for(name in colnames(head_train)){
# 	 if(substring(name, first=1, last=4)!='comp' & substring(name, first=1, last=4)!='date' ){
# 	 	fname <- paste(name,'.csv', sep='')
# 	 	feature<-read.csv(fname, header=FALSE, sep=',', na.strings='NULL')
# #	 	feature<- as.numeric(feature)
# 	 	boxplot(feature[,2], main=paste('Boxplot', name, 'and mean (red)'), xlab=name, ylab='value')
# 		points(mean(feature[,2]), col='red',pch=19)
# 	}
# }
# dev.off()
srch_id<-read.csv('srch_id.csv', header=FALSE, sep=',', na.strings='NULL', col.names=c('index','srch_id'))
nr_samples=length(srch_id[,1])
review<-read.csv('prop_review_score.csv', header=FALSE, sep=',', na.strings='NULL' , col.names=c('index','review'))
da<-merge(srch_id,review,by='index')
remove(srch_id)
remove(review)
location<-read.csv('prop_location_score2.csv', header=FALSE, sep=',', na.strings='NULL' , col.names=c('index','location'))
da<-merge(da,location,by='index')
remove(location)
query_aff<-read.csv('srch_query_affinity_score.csv', header=FALSE, sep=',', na.strings='NULL' , col.names=c('index','qa'))
da<-merge(da,query_aff,by='index')
remove(query_aff)
click_bool<-read.csv('click_bool.csv', header=FALSE, sep=',', na.strings='NULL', col.names=c('index','click'))
da<-merge(da,click_bool,by='index')
remove(click_bool)
booking_bool<-read.csv('booking_bool.csv', header=FALSE, sep=',', na.strings='NULL', col.names=c('index','book'))
da<-merge(da,booking_bool,by='index')
remove(booking_bool)


heights_click<-c(mean(da$click[is.na(da$review)==FALSE]),mean(da$click[is.na(da$review)]))
heights_book <-c(mean(da$book[is.na(da$review)==FALSE]),mean(da$book[is.na(da$review)]))
pdf('review_na.pdf', width=10, height=7)
par(mfrow=c(1, 2), mar = c(7, 4, 4, 2) + 0.5)
barplot(heights_click, ylim=c(0,0.06),
        ylab="Percentage clicked",
        main="percentage clicked NA category of reviews",
        names.arg = c('Not NA', 'NA'),
        col=c('blue','red'))
barplot(heights_book, ylim=c(0,0.06),
        ylab="Percentage booked",
        main="percentage booked NA category of reviews",
        names.arg = c('Not NA', 'NA'),
        col=c('blue','red'))
dev.off()

heights_click<-c(mean(da$click[is.na(da$location)==FALSE]),mean(da$click[is.na(da$location)]))
heights_book <-c(mean(da$book[is.na(da$location)==FALSE]),mean(da$book[is.na(da$location)]))
pdf('location_na.pdf', width=10, height=7)
par(mfrow=c(1, 2), mar = c(7, 4, 4, 2) + 0.5)
barplot(heights_click, ylim=c(0,0.06),
        ylab="Percentage clicked",
        main="percentage clicked NA category of location 2",
        names.arg = c('Not NA', 'NA'),
        col=c('blue','red'))
barplot(heights_book, ylim=c(0,0.06),
        ylab="Percentage booked",
        main="percentage booked NA category of location 2",
        names.arg = c('Not NA', 'NA'),
        col=c('blue','red'))
dev.off()

heights_click<-c(mean(da$click[is.na(da$qa)==FALSE]),mean(da$click[is.na(da$qa)]))
heights_book <-c(mean(da$book[is.na(da$qa)==FALSE]),mean(da$book[is.na(da$qa)]))
pdf('query_affinity_na.pdf', width=10, height=7)
par(mfrow=c(1, 2), mar = c(7, 4, 4, 2) + 0.5)
barplot(heights_click, ylim=c(0,0.06),
        ylab="Percentage clicked",
        main="percentage clicked NA category of query affinity",
        names.arg = c('Not NA', 'NA'),
        col=c('blue','red'))
barplot(heights_book, ylim=c(0,0.06),
        ylab="Percentage booked",
        main="percentage booked NA category of query affinity",
        names.arg = c('Not NA', 'NA'),
        col=c('blue','red'))
dev.off()

