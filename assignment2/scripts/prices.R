library(Hmisc)
library(plyr)



srch_id<-read.csv('srch_id.csv', header=FALSE, sep=',', na.strings='NULL', col.names=c('index','srch_id'))
nr_samples=length(srch_id[,1])
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
location<-read.csv('prop_location_score2.csv', header=FALSE, sep=',', na.strings='NULL' , col.names=c('index','location'))
da<-merge(da,location,by='index')
remove(location)
query_aff<-read.csv('srch_query_affinity_score.csv', header=FALSE, sep=',', na.strings='NULL' , col.names=c('index','qa'))
da<-merge(da,query_aff,by='index')
remove(query_aff)
review<-read.csv('prop_review_score.csv', header=FALSE, sep=',', na.strings='NULL' , col.names=c('index','review'))
da<-merge(da,review,by='index')
remove(review)

# boxplot(da$price)
# points(mean(da$price), col='red', pch=19)
# boxplot(da$location)
# points(mean(da$location), col='red', pch=19)
# hist(da$price)

#Un normalized prices
prices<-as.matrix(da$price)
binned.prices<-cut2(prices, g=12)
heights_book <-tapply(da$book,binned.prices,mean)
heights_click <-tapply(da$click,binned.prices,mean)
pdf('prices.pdf', width=10, height=7)
par(mfrow=c(1, 2), mar = c(7, 4, 4, 2) + 0.6)
barplot(heights_click, ylim=c(0,0.06),
        ylab="Percentage clicked",
        main="clicked per price category",
        las=2,
        col=rainbow(12))
barplot(heights_book, ylim=c(0,0.06),
        ylab="Percentage booked",
        main="booked per price category",
        las=2,
        col=rainbow(12))
dev.off()


#Unnormalized location
location<-as.matrix(da$location)
binned.location<-cut2(location, g=12)
heights_book <-tapply(da$book,binned.location,mean, na.rm = TRUE)
heights_click <-tapply(da$click,binned.location,mean, na.rm = TRUE)
heights_book<-append(heights_book, mean(da$book[is.na(da$location)]))
names(heights_book)[13]<-'NA'
heights_click<-append(heights_click, mean(da$click[is.na(da$location)]))
names(heights_click)[13]<-'NA'
pdf('location.pdf', width=10, height=7)
par(mfrow=c(1, 2), mar = c(7, 4, 4, 2) + 0.6)
barplot(heights_click, ylim=c(0,0.1),
        ylab="Percentage clicked",
        main="clicked per location 2 score category",
        las=2,
        col=rainbow(13))
barplot(heights_book, ylim=c(0,0.1),
        ylab="Percentage booked",
        main="booked per location 2 score category",
        las=2,
        col=rainbow(13))
dev.off()

# Unnormalized query affinity
qa<-as.matrix(da$qa)
binned.qa<-cut2(qa, g=12)
heights_book <-tapply(da$book,binned.qa,mean, na.rm=TRUE)
heights_click <-tapply(da$click,binned.qa,mean, na.rm=TRUE)
heights_book<-append(heights_book, mean(da$book[is.na(da$qa)]))
names(heights_book)[13]<-'NA'
heights_click<-append(heights_click, mean(da$click[is.na(da$qa)]))
names(heights_click)[13]<-'NA'
pdf('qa.pdf', width=10, height=7)
par(mfrow=c(1, 2), mar = c(7, 4, 4, 2) + 0.6)
barplot(heights_click, ylim=c(0,0.06),
        ylab="Percentage clicked",
        main="clicked qa category",
        las=2,
        col=rainbow(13))
barplot(heights_book, ylim=c(0,0.06),
        ylab="Percentage booked",
        main="booked per qa category",
        las=2,
        col=rainbow(13))
dev.off()

#Unnormalized review score
review<-as.matrix(da$review)
#binned.review<-cut2(review, g=6)
heights_book <-tapply(da$book,review,mean, na.rm=TRUE)
heights_click <-tapply(da$click,review,mean, na.rm=TRUE)
heights_book<-append(heights_book, mean(da$book[is.na(da$review)]))
names(heights_book)[11]<-'NA'
heights_click<-append(heights_click, mean(da$click[is.na(da$review)]))
names(heights_click)[11]<-'NA'
pdf('review.pdf', width=10, height=7)
par(mfrow=c(1, 2), mar = c(7, 4, 4, 2) + 0.6)
barplot(heights_click, ylim=c(0,0.05),
        ylab="Percentage clicked",
        main="clicked per review category",
        las=2,
        col=rainbow(11))
barplot(heights_book, ylim=c(0,0.05),
        ylab="Percentage booked",
        main="booked per review category",
        las=2,
        col=rainbow(11))
dev.off()



#normalized prices
da<-ddply(da, .(srch_id), mutate, log_price = log(price+1), 
      mn_price = mean(log_price), sd_price = sd(log_price),
      nrm_price = (log_price-mn_price)/sd_price)

prices<-as.matrix(da$nrm_price)
binned.prices<-cut2(prices, g=12)
heights_book <-tapply(da$book,binned.prices,mean)
heights_click <-tapply(da$click,binned.prices,mean)
pdf('normalized_price.pdf', width=10, height=7)
par(mfrow=c(1, 2), mar = c(7, 4, 4, 2) + 0.6)
barplot(heights_click, ylim=c(0,0.06),
        ylab="Percentage clicked",
        main="clicked per normalized price category",
        las=2,
        col=rainbow(12))
barplot(heights_book, ylim=c(0,0.06),
        ylab="Percentage booked",
        main="booked per normalized price category",
        las=2,
        col=rainbow(12))
dev.off()


da<-ddply(da, .(srch_id), mutate, 
          mn_loc = mean(location, na.rm=TRUE), sd_loc = sd(location, na.rm=TRUE),
          nrm_loc = (location-mn_loc)/sd_loc)

loc<-as.matrix(da$nrm_loc)
binned.loc<-cut2(loc, g=12)
heights_book <-tapply(da$book,binned.loc,mean, na.rm=TRUE)
heights_click <-tapply(da$click,binned.loc,mean, na.rm=TRUE)
heights_book<-append(heights_book, mean(da$book[is.na(da$location)]))
names(heights_book)[13]<-'NA'
heights_click<-append(heights_click, mean(da$click[is.na(da$location)]))
names(heights_click)[13]<-'NA'
pdf('normalized_locations.pdf', width=10, height=7)
par(mfrow=c(1, 2), mar = c(7, 4, 4, 2) + 0.6)
barplot(heights_click, ylim=c(0,0.08),
        ylab="Percentage clicked",
        main="clicked per normalized location category",
        las=2,
        col=rainbow(13))
barplot(heights_book, ylim=c(0,0.08),
        ylab="Percentage booked",
        main="booked per normalized location category",
        las=2,
        col=rainbow(13))
dev.off()

#normalized query affinity
da<-ddply(da, .(srch_id), mutate, 
          mn_qa = mean(qa, na.rm=TRUE), sd_qa = sd(qa, na.rm=TRUE),
          nrm_qa = (qa-mn_qa)/sd_qa)

qa<-as.matrix(da$nrm_qa)
binned.qa<-cut2(qa, g=12)
heights_book <-tapply(da$book,binned.qa,mean, na.rm=TRUE)
heights_click <-tapply(da$click,binned.qa,mean, na.rm=TRUE)
heights_book<-append(heights_book, mean(da$book[is.na(da$qa)]))
names(heights_book)[13]<-'NA'
heights_click<-append(heights_click, mean(da$click[is.na(da$qa)]))
names(heights_click)[13]<-'NA'
pdf('normalized_qa.pdf', width=10, height=7)
par(mfrow=c(1, 2), mar = c(7, 4, 4, 2) + 0.6)
barplot(heights_click, ylim=c(0,0.15),
        ylab="Percentage clicked",
        main="clicked per normalized qa category",
        las=2,
        col=rainbow(13))
barplot(heights_book, ylim=c(0,0.15),
        ylab="Percentage booked",
        main="booked per normalized qa category",
        las=2,
        col=rainbow(13))
dev.off()

#normalized review score
da<-ddply(da, .(srch_id), mutate, 
          mn_review = mean(review, na.rm=TRUE), sd_review = sd(review, na.rm=TRUE),
          nrm_review = (review-mn_review)/sd_review)

review<-as.matrix(da$nrm_review)
binned.review<-cut2(review, g=12)
heights_book <-tapply(da$book,binned.review,mean, na.rm=TRUE)
heights_click <-tapply(da$click,binned.review,mean, na.rm=TRUE)
heights_book<-append(heights_book, mean(da$book[is.na(da$review)]))
names(heights_book)[13]<-'NA'
heights_click<-append(heights_click, mean(da$click[is.na(da$review)]))
names(heights_click)[13]<-'NA'
pdf('normalized_review.pdf', width=10, height=7)
par(mfrow=c(1, 2), mar = c(7, 4, 4, 2) + 0.6)
barplot(heights_click, ylim=c(0,0.05),
        ylab="Percentage clicked",
        main="clicked per normalized review category",
        las=2,
        col=rainbow(13))
barplot(heights_book, ylim=c(0,0.05),
        ylab="Percentage booked",
        main="booked per normalized review category",
        las=2,
        col=rainbow(13))
dev.off()

############### testing #########################
# # boxplot(srch_id[,2])
# # site_id<-read.csv('site_id.csv', header=FALSE, sep=',', na.strings='NULL'))
# # Summarize a dataset by two variables
# dfx <- data.frame(
#   group = c(rep('A', 8), rep('B', 15), rep('C', 6)),
#   sex = sample(c("M", "F"), size = 29, replace = TRUE),
#   age = runif(n = 29, min = 18, max = 54),
#   book = sample(c(TRUE,FALSE), size = 29, replace = TRUE),
#   click = sample(c(TRUE,FALSE), size = 29, replace = TRUE)
# )
# nr<-length(dfx[,1])
# 
# age<-as.matrix(dfx$age)
# binned.age<-cut2(age, m=nr/12)
# heights_book <-tapply(dfx$book,binned.age,mean)
# heights_click <-tapply(dfx$click,binned.age,mean)
# #pdf('normalized_prices.pdf', width=10, height=7)
# par(mfrow=c(1, 2), mar = c(7, 4, 4, 2) + 0.5)
# barplot(heights_click, ylim=c(0,1),
#         ylab="Percentage clicked",
#         main="clicked per normalized price category",
#         las=2,
#         col=rainbow(12))
# barplot(heights_book, ylim=c(0,1),
#         ylab="Percentage booked",
#         main="booked per normalized price category",
#         las=2,
#         col=rainbow(12))
# #dev.off()
# 
# 
# # Note the use of the '.' function to allow
# # group and sex to be used without quoting
# ddply(dfx, .(group), mutate, mean=mean(age), sd=sd(age),
#       norm= (age-mean)/sd)
# 
