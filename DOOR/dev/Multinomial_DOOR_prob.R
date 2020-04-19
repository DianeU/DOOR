#data = read.csv("C:/Users/bsc-default/Desktop/Simulation Door Prob/door_tst.csv",header = TRUE,dec = ".",na.strings = "")
 
DOOR_prob_estimate <- function(data,alpha,treatment,door_category){
 
  freq <- get_door_summary(data, treatment, door_category)
  
  
  treatment <- names(freq[2])
  control <- names(freq[3])
  
  pr_c <- unlist(freq[,2]/sum(freq[,2]))
  pr_t <- unlist(freq[,3]/sum(freq[,3]))
  
  n_c <- sum(freq[,2]) #Number of subjects in control group
  n_t <- sum(freq[,3]) #Number of subjects in treatment group
  
  d <- length(pr_c) #Number of door levels
  
  ##replace diagonal elements of Cov with Variance##
  variance_c = pr_c*(1-pr_c)/n_c
  cov_c = -pr_c%*%t(pr_c)/n_c
  diag(cov_c)=variance_c
  
  variance_t = pr_t*(1-pr_t)/n_t
  cov_t = -pr_t%*%t(pr_t)/n_t
  diag(cov_t)=variance_t
  
  empty=matrix(0,nrow=d,ncol=d)
  cov = rbind(cbind(cov_c,empty),cbind(empty,cov_t))
  empty[lower.tri(empty)] <- 1
  indicator=diag(d)*0.5+empty
  
  DOOR_pr=sum((pr_c%*%t(pr_t))*indicator)
  
  delta_c <- rowSums(cbind((rep(1,d)%*%t(pr_t))*indicator))
  delta_t <- colSums(cbind(pr_c%*%t(rep(1,d))*indicator))
  delta <- cbind(t(delta_c),t(delta_t))
  testcov=delta%*%cov%*%t(delta) 
  se=sqrt(testcov)
  power=1-pnorm(0.5/se+1.68-DOOR_pr/se)
  
  lb <- DOOR_pr + qnorm(alpha/2)*se 
  ub <- DOOR_pr - qnorm(alpha/2)*se
  return(list("Door probablility estimate"=DOOR_pr,
              "lower bound"=lb,
              "upper bound"=ub))
  
  
}
 
DOOR_prob_estimate(data,0.05,"trt","doorcat4")                                  
                                  
                                  
                                  
                                  