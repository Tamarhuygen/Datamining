cg<-function(ix, rel){
  if(ix > 1){
    return(rel/log2(ix))
  }
  else{
    return(rel)
  }
}

relevance<-function(click, book){
  rel<-click+4*book
  return(rel)
}

da<-ddply(da, .(srch_id), mutate, rel = relevance(click, book), ix = rank(-nrm_price, ties.method = "first"))
da<-da[with(da,order(da$srch_id, da$nrm_price)),]
da$score<-cumsum(mapply(cg, da$ix, da$rel))
head(da)
da<-da[with(da, order(da$srch_id,-da$rel,-da$nrm_price)),]
da<-ddply(da, .(srch_id), function(x) data.frame(x, ixi=1:nrow(x)))
da$iscore<-cumsum(mapply(cg, da$ixi, da$rel))
da$ndcg<-da$score/da$iscore
max_ndcg<-aggregate(da$ndcg, by =list(da$srch_id), FUN = max)
mean_ncdg<-mean(max_ndcg[,2])

id<-c(1,1,1,1,2,2,2,2,2,2)
re<-c(0,1,5,1,5,1,5,0,1,1)
pr<-c(3,4,6,5,2,3,1,5,6,7)
foo<-data.frame(id,pr,re)
# foo<-ddply(foo, .(id), mutate, ix= rank(-pr, ties.method = "first"))
# foo<-foo[with(foo,order(id,-pr)),]
# foo$score<-mapply(cg, foo$ix, foo$re)
# foo$score<-cumsum(foo$score)
# foo<-foo[with(foo, order(id,-re,-pr)),]
# foo<-ddply(foo, .(id), function(x) data.frame(x, ixi=1:nrow(x)))
# foo$iscore<-cumsum(mapply(cg, foo$ixi, foo$re))
# foo<-foo[with(foo,order(id,-pr)),]
# foo$ndcg<-foo$score/foo$iscore
# max_ndcg<-aggregate(foo$ndcg, by =list(foo$id), FUN = max)
# max_ndcg[,2]
# mean(max_ndcg[,2])
