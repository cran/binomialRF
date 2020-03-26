## ----setup, include = FALSE----------------------------------------------
  library('randomForest')
  library('data.table')
  library('stats')
  library('binomialRF')

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=T, warning=F, message=F----------------------------------------
set.seed(324)

### Generate multivariate normal data in R10
X = matrix(rnorm(1000), ncol=10)

### let half of the coefficients be 0, the other be 10
trueBeta= c(rep(3,2), rep(0,8))

### do logistic transform and generate the labels
z = 1 + X %*% trueBeta    
pr = 1/(1+exp(-z))        
y = rbinom(100,1,pr)


## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(cbind(round(X,2),y), 10))

## ----echo=T, warning=F, message=F----------------------------------------
require(correlbinom)
rho = 0.33
ntrees = 250

cbinom = correlbinom(rho, successprob =  1/ncol(X), trials = ntrees, 
                                  precision = 1024, model = 'kuk')


## ----echo=T, warning=F, message=F----------------------------------------
binom.rf <- binomialRF::binomialRF(X,factor(y), fdr.threshold = .05,
                     ntrees = ntrees,percent_features = .6,
                     fdr.method = 'BY', user_cbinom_dist = cbinom, 
                     sampsize = round(nrow(X)*.33))
print(binom.rf)


## ----echo=F, warning=F, message=F----------------------------------------
# set.seed(324)

binom.rf <- binomialRF::binomialRF(X,factor(y), fdr.threshold = .05,
                     ntrees = ntrees,percent_features = 1,
                     fdr.method = 'BY', user_cbinom_dist = cbinom, sampsize = round(nrow(X)*.33))

cat('\n\nbinomialRF 100%\n\n')
print(binom.rf)

binom.rf <- binomialRF::binomialRF(X,factor(y), fdr.threshold = .05,
                     ntrees = ntrees,percent_features = .8,
                     fdr.method = 'BY', user_cbinom_dist = cbinom, sampsize = round(nrow(X)*.33))

cat('\n\nbinomialRF 80%\n\n')
print(binom.rf)

binom.rf <- binomialRF::binomialRF(X,factor(y), fdr.threshold = .05,
                     ntrees = ntrees,percent_features = .6,
                     fdr.method = 'BY', user_cbinom_dist = cbinom, sampsize = round(nrow(X)*.33))

cat('\n\nbinomialRF 60%\n\n')
print(binom.rf)

## ----echo=F, warning=F, message=F----------------------------------------
set.seed(324)

binom.rf1000 <- binomialRF::binomialRF(X,factor(y), fdr.threshold = .05,
                     ntrees = ntrees,percent_features = .5,
                     fdr.method = 'BY', user_cbinom_dist = cbinom, sampsize = round(nrow(X)*.33))



rho = 0.33
ntrees = 500

cbinom = correlbinom(rho, successprob =  1/ncol(X), trials = ntrees, precision = 1024, model = 'kuk')

binom.rf500 <- binomialRF::binomialRF(X,factor(y), fdr.threshold = .05,
                     ntrees = ntrees,percent_features = .5,
                     fdr.method = 'BY', user_cbinom_dist = cbinom, sampsize = round(nrow(X)*.33))




cat('\n\nbinomialRF 250 trees\n\n')
print(binom.rf500)

cat('\n\nbinomialRF 500 trees \n\n')
print(binom.rf1000)


