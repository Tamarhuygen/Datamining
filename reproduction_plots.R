#setwd('training_by_col')
head_train<-read.csv('head.csv', na.strings='NULL')
#head_t<-head_train[]
#boxplot(head_train[,1], main='test')
print(colnames(head_train))
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
price_usd<-read.csv('price_usd.csv', header=FALSE, sep=',', na.strings='NULL' , col.names=c('index','price'))
da<-merge(srch_id,price_usd,by='index')
remove(srch_id)
remove(price_usd)
click_bool<-read.csv('click_bool.csv', header=FALSE, sep=',', na.strings='NULL', col.names=c('index','click'))
da<-merge(da,click_bool,by='index')
remove(click_bool)
booking_bool<-read.csv('booking_bool.csv', header=FALSE, sep=',', na.strings='NULL', col.names=c('index','click'))
da<-merge(da,booking_bool,by='index')
remove(booking_bool)
boxplot(da$price)
hist(da$price)
foo<-function(x){
  result <-log10(x+1)
  return(result)
}
da$price<-lapply(da$price, foo)
boxplot(da$price)
hist(da$price)
# boxplot(srch_id[,2])
# site_id<-read.csv('site_id.csv', header=FALSE, sep=',', na.strings='NULL'))