library(Hmisc)
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
booking_bool<-read.csv('booking_bool.csv', header=FALSE, sep=',', na.strings='NULL', col.names=c('index','book'))
da<-merge(da,booking_bool,by='index')
remove(booking_bool)
boxplot(da$price)
hist(da$price)
prices<-as.matrix(da$price)
binned.prices<-cut2(prices, m=500000)
heights_click <-tapply(da$click,binned.prices,mean)
heights_book <-tapply(da$book,binned.prices,mean)
heights_click <-tapply(da$click,binned.prices,mean)
pdf('prices.pdf', width=10, height=7)
par(mfrow=c(1, 2), mar = c(7, 4, 4, 2) + 0.5)
barplot(heights_click, ylim=c(0,0.06),
        ylab="Percentage clicked",
        main="clicked per price category",
        las=2,
        col=rainbow(9))
barplot(heights_book, ylim=c(0,0.06),
        ylab="Percentage booked",
        main="booked per price category",
        las=2,
        col=rainbow(9))
dev.off()

# foo<-function(x){
#   result <-log10(x+1)
#   return(result)
# }
# da$log_price<-lapply(da$price, foo)
# boxplot(da$log_price)
# hist(dalog_price)
# boxplot(srch_id[,2])
# site_id<-read.csv('site_id.csv', header=FALSE, sep=',', na.strings='NULL'))