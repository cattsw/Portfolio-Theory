
# calc annually mean and std for two stocks
data <- read.csv(file="./stock_price_data.csv", header = T)
library(xts)
data$DATE<- as.Date(as.character(data$DATE), "%Y%m%d")
stock_r <- xts(data[,2:3],data$DATE)
r_mean_ann <- 12*sapply(stock_r,mean)
r_var_ann <- 12*sapply(stock_r,var)
r_std_ann <- sqrt(12)*sapply(stock_r,sd)
r_cov <- cov(stock_r[,"MSFT"],stock_r[,"GM"])
r_corr <- cor(stock_r[,"MSFT"],stock_r[,"GM"])
cat("Annually mean returns:\n",r_mean_ann )
cat("Annually variances:\n",r_var_ann )


#For different allocation of two stocks,compute the portfolio returns and standard deviation 
w <- matrix(c(seq(1,0,-0.01),seq(0,1,0.01)),nrow=101,ncol=2)
r_p <- w%*%r_mean_ann
var_cov <- matrix(c(r_var_ann[1],r_cov,r_cov,r_var_ann[2]),2,2)
var_p <- diag(w%*%var_cov%*%t(w))
std_p <- sqrt(var_p)
plot(std_p,r_p,xlab="standard deviation",ylab="return",main="mean-standard deviation graph",type="o",pch=1,col="blue")

# minimum variance portfolio
d_sigma_p<- function(w){2*w*r_var_ann[1] - 2*(1-w)*r_var_ann[2] + (2-4*w)*r_cov}
w_min_var <- uniroot(d_sigma_p, interval = c(-1e+08, 1e+08))$root
r_min_var <- c(w_min_var,1-w_min_var)%*%r_mean_ann
var_min_var <- c(w_min_var,1-w_min_var)%*%var_cov%*%c(w_min_var,1-w_min_var)
std_min_var <- sqrt(var_min_var)

# tangency portfolio
r_f <- 0.02
sharp_ratio <- function(w){((w*r_mean_ann[1]+(1-w)*r_mean_ann[2]-r_f)/sqrt(w^2*r_var_ann[1] + (1-w)^2*r_var_ann[2] + 2*w*(1-w)*r_cov))}
w_max_sr <- optimize(sharp_ratio,interval=c(0,1),maximum=TRUE)$maximum
r_max_sr <- c(w_max_sr,1-w_max_sr)%*%r_mean_ann
var_max_sr <- c(w_max_sr,1-w_max_sr)%*%var_cov%*%c(w_max_sr,1-w_max_sr)
std_max_sr <- sqrt(var_max_sr)
max_sr <- (r_max_sr - r_f)/std_max_sr 
plot(std_p,r_p,xlab="standard deviation",ylab="return",main="tangency portfolio",type="o",pch=1,col="blue")
abline(r_f,max_sr,col="red")

# Optimal portfolio
ra=4
utility <- function(w){w*r_max_sr+(1-w)*r_f-ra/2*w^2*var_max_sr}
w_optimal <- optimize(utility,interval=c(0,1),maximum=TRUE)$maximum



