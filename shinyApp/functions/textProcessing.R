######################################################################
# Function to compute (variance-weighted) log-odds
#  x is a V-length count vector for the numerator group
#  y is a V-length count vector for the denominator group
#  prior is the pseudo count
slogodds <- function(x,y,prior) {
  pcons <- (x+ prior)/sum(x + prior)
  plibs <- (y+ prior)/sum(y + prior)
  
  ocons <- pcons/(1-pcons)
  olibs <- plibs/(1-plibs)
  
  lor <- log(ocons) - log(olibs)
  sd.lor <-  sqrt(1/(x + prior) + 1/(y + prior))
  
  list(lor=lor, slor=(lor/sd.lor))
}
######################################################################
# Function to compute mutual information
mi <- function(i, j) {    
  counts <- table(i,j)
  N <- sum(counts)
  tab <- counts/N
  null <- rowSums(tab) %o% colSums(tab)
  logterm <- ifelse(tab > 0, log2(tab/null), 0)
  mi <- sum(tab*logterm)
  return(mi)
}
######################################################################
# Compute variance-weighted log-odds and mutual information
cmp.slor.mis <- function(textm, rating) {
  textm<-as.matrix(textm)
  prior <- 1
  cons <- Matrix::colSums(textm[rating >= 0,], na.rm=T)  # Conservative posts
  libs <- Matrix::colSums(textm[rating < 0,], na.rm=T)  # Liberal posts
  slor <- slogodds(cons, libs, prior)$slor  # Use the variance-weighted log-odds
  mis <- apply(textm, 2, function(x) mi(x>0, rating))
  results <- data.frame(slor =  slor, mis = mis) %>% mutate(col = slor > 0)
  return(results)
}
######################################################################
# Remove thread-specific words
group.max.conc<-function(texts,groups){
  group.id<-unique(groups)
  cts<-array(NA, c(ncol(texts),length(group.id)))
  for(g in 1:length(group.id)){
    cts[,g]<-colSums(texts[groups==group.id[g],])
  }
  max.conc<-apply(cts,1,max)/rowSums(cts)
  return(texts[,max.conc<0.8])
}
mediansplit<-function (item, q = 0.5) 
{
  cutoff <- quantile(item, probs = q, na.rm = T)
  return(1 * (item > cutoff))
}
######################################################################