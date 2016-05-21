

slopes <- as.matrix(covs$slope)
binned.slopes=cut2(slopes, m=100)
heights <- tapply(covs$y,binned.slopes,mean)
barplot(heights, ylim=c(0,1),
        ylab="Probability of permafrost",
        xlab="Slope",     
        col="lightgrey")