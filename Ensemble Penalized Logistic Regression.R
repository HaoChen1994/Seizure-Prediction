#not run
library(glmnet)

n1 <- length(X1_omega)
n2 <- length(X2_omega)
boot_strap <- 100

p <- 21
tr1 <- floor(n1/3*2)
tr2 <- floor(n2/3*2)
ts1 <- n1-tr1
ts2 <- n2-tr2

K=1

X1_vec<-matrix(nrow=n1,ncol=p*(p-1)/2)
W1_vec <- array()
X2_vec<-matrix(nrow=n2,ncol=p*(p-1)/2)
W2_vec <- array()
a <- matrix(nrow=boot_strap,ncol=p*(p-1)/2)
Y_pre <- matrix(nrow=boot_strap,ncol=(ts1+ts2))
predictions <- matrix(nrow = K, ncol = (ts1+ts2))
a_sum <- matrix(nrow = K, ncol = 21)

Y_pre.vote<-array()
var.iden <- list()
error.rate <- array()

mut_in_f<-function(x){
        1/2*log((1+x)/(1-x))
}

W1<-lapply(lapply(X1_omega,cov2cor),mut_in_f)
W2<-lapply(lapply(X2_omega,cov2cor),mut_in_f)

#Generate data for logistic regression
for (i in 1:(n1)) {
        W1_vec <- as.vector(W1[[i]][upper.tri(W1[[i]], diag = FALSE)])
        X1_vec[i,] <- c(W1_vec)
}
for (i in 1:(n2)) {        
        W2_vec <- as.vector(W2[[i]][upper.tri(W2[[i]], diag = FALSE)])
        X2_vec[i,] <- c(W2_vec)
}
set.seed(666)
g1 <- sample(1:n1, tr1, replace = F)
g2 <- sample(1:n2, tr2, replace = F)
for(k in 1:K){
        print(k)
        for (j in 1:boot_strap) {
                print(j)
                ids1 <- sample(g1, min(tr1,tr2), replace=T)
                ids2 <- sample(g2, min(tr1,tr2), replace=T)
                x1 <- X1_vec
                x2 <- X2_vec
                #training data
                X_vec <- rbind(x1[ids1,],x2[ids2,])
                Y_vec <- array(c(rep(1,min(tr1,tr2)),rep(0,min(tr1,tr2))),dim = c((min(tr1,tr2)+min(tr1,tr2)),1))
                #test data
                X_vec.pre <- rbind(x1[-g1,],x2[-g2,])
                Y_vec.pre <- array(c(rep(1,nrow(x1[-g1,])),rep(0,nrow(x2[-g2,]))),dim = c(nrow(X_vec.pre),1))
                
                cv.fit <- cv.glmnet(X_vec,Y_vec,family='binomial',type.measure="class")
                
                Y_pre[j,] <- as.vector(as.numeric(predict(cv.fit, newx = X_vec.pre, s = "lambda.min", type = "class")))
                
                #sensitivity and specificity
                coefficients <- coef(cv.fit,s=cv.fit$lambda.min)
                diff <- as.vector(coefficients[-1]) 
                a[j,] <- diff
                for (a_t in 1:((p*(p-1)/2))) {
                        if (a[j,a_t]!=0){
                                a[j,a_t]<-1                        
                        }
                }
        }
        predictions[k,] <- colMeans(Y_pre)
        
        Y_pre.roc <- array()
        for (e in 1:(ts1+ts2)) {
                if(predictions[k,e] > 0.5){
                        Y_pre.roc[e]<-1
                } else if(predictions[k,e] < 0.5){
                        Y_pre.roc[e]<-0
                } else if(predictions[k,e] == 0.5){
                        Y_pre.roc[e]<-sample(c(0,1),1)
                }
        }
        
        error.rate[k] <- sum((Y_vec.pre - Y_pre.roc)^2) / length(Y_vec.pre)
        print("Our method done!")
}
boxplot(error.rate,ylim=c(0,1))
mean(error.rate)
sd(error.rate)
####################ROC Curve########################
library(ROCR)
predictions1 <- colMeans(predictions)
pred <- prediction(predictions1, Y_vec.pre)
perf1 <- performance(pred,"acc","cutoff")
perf <- performance(pred,"sens","fpr")
auc1 <- performance(pred,"auc")
auc <- as.numeric(auc1@y.values)
print(auc)
f1 <- performance(pred,"f")
perf2 <- performance(pred, "prec", "rec")
plot(perf2)
plot(perf,colorize=TRUE)
perf@x.values
perf@y.values
max(perf1@y.values[[1]])