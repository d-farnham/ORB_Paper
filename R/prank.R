prank<-function(x){
  r<-rank(x)/sum(!is.na(x))
  r[is.na(x)]<-NA
  r
}