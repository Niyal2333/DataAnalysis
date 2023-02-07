summary(ds)

DS21 <-subset(ds,work_year=="2021")
DS22 <-subset(ds,work_year=="2022")

View(DS21)
View(DS22)

summary(DS21)
summary(DS22)

stat<-function(x){
  data<-c("Mean"=mean(x, na.rm=TRUE),
          "Median"=median(x, na.rm =TRUE),
          "Standard Deviation" = sd(x, na.rm =TRUE),
          "Length" = length(x))
  return(data)
}

#Hypothesis 1
#Our Hypothesis: Salaries in the field of Data Science in the year 2022 is higher than in the year 2021.
#Lets calculate the P-Value using Z-test.
DS21_stat <- stat(DS21$salary_in_usd)
DS21_stat
DS22_stat <- stat(DS22$salary_in_usd)
DS22_stat

sd_DS21_DS22 <- sqrt(DS21_stat[3]^2/DS21_stat[4] + DS22_stat[3]^2/DS22_stat[4])
sd_DS21_DS22
z_score <- (DS22_stat[1]-DS21_stat[1])/sd_DS21_DS22
z_score

p <- 1 - pnorm(z_score)
p
#the p-value is so small, it returns the value which is almost zero.
#It is almost impossible for that result (the difference in means between the salaries in 2021 and 2022) to happen by chance

plot(x=seq(-15,15, by=0.1),y=dnorm(seq(-15,15,by=0.1),mean=0),type='l',xlab = 'mean difference',  ylab='possibility')
#this plots a normal distribution with mean=0, stdev=1 for you to visualize the z-score

abline(v=z_score, col='red')
#this shows where the observation falls (z-score)

#We found great evidence to support our hypothesis that Salaries in the field of Data Science in the year 2022 is higher than in the year 2021.


#Hypothesis 2
#Our Hypothesis: Salaries of Data Scientist is higher than Data Analyst.
#Lets calculate the P-Value using Z-test.

DSci <-subset(DS22,job_title=="Data Scientist")
DAna <-subset(DS22,job_title=="Data Analyst")

View(DSci)
View(DAna)

summary(DSci)
summary(DAna)

DSci_stat <- stat(DSci$salary_in_usd)
DSci_stat
DAna_stat <- stat(DAna$salary_in_usd)
DAna_stat

sd_DSci_DAna <- sqrt(DSci_stat[3]^2/DSci_stat[4] + DAna_stat[3]^2/DAna_stat[4])
sd_DSci_DEng
z_score <- (DSci_stat[1]-DAna_stat[1])/sd_DSci_DAna
z_score

p <- 1 - pnorm(z_score)
p
#the p-value is so small, it returns the value which is almost zero.
#It is almost impossible for that result (the difference in means between the salaries of Data Scientist and Data Analyst) to happen by chance

plot(x=seq(-15,15, by=0.1),y=dnorm(seq(-15,15,by=0.1),mean=0),type='l',xlab = 'mean difference',  ylab='possibility')
#this plots a normal distribution with mean=0, stdev=1 for you to visualize the z-score

abline(v=z_score, col='red')
#this shows where the observation falls (z-score)

#We found great evidence to support our hypothesis Salaries of Data Scientist is higher than Data Analyst.


#Hypothesis: Salaries in the field of Data Science in the year 2021 is higher than in the year 2020.
#Hypothesis 3
RemoveOutliers <- function(df,column,min_quant=NA,max_quant=NA){
  ##remove values under the min_quant quantile value and above the max_quant quantile values for the column specified
  ## e.g. removerOutliers(df,0.01,0.99) removes the top and bottom 1% of observations for the column
  ## e.g. removerOutliers(df,max_quant=0.99) removes the top 1% (but not bottom) of observations for the column
  min_cut = unname(quantile(df[,column],min_quant))
  max_cut = unname(quantile(df[,column],max_quant))
  cut_df = df[df[column]>=min_cut & df[column]<=max_cut,]
  return(cut_df)
}

Permute_samples <- function(p,df,X){
  p<-sample(p,length(p),FALSE)
  in_sample_mean <- mean(df[p==1,X],na.rm=TRUE)
  out_sample_mean <- mean(df[p==0,X],na.rm=TRUE)
  return(in_sample_mean - out_sample_mean)
}

Permutation <- function(n,df1,c1,c2,w1,w2=NA,tail="R",plot=TRUE){
  #df1 is the source data frame
  #c1 is the column used for identifying the different groups
  #w1 and w2 are the two groups to compare (w1 is test group)
  #c2 is the column with the observed data
  #n is the number of samples
  #tail = R or L if we are using a one tailed test (right ot left), =T for a two tailed test (testing the effect one-way or two-way)
  df <- as.data.frame(df1) #makes sure the data is in a dataframe format
  D_null<-c()
  V1<-df[,c1]
  V2<-df[,c2]
  sub.value1 <- df[df[, c1] == w1, c2]

  m <- length(V1[V1==w1])
  if(!is.na(w2)){
    l <-length(V1[V1==w2])
    sub.value2 <- df[df[, c1] == w2, c2]
    df <- df[(df[, c1] == w1)|(df[, c1] == w2),]
  }
  else{
    l <-length(V1)-m
    sub.value2 <- df[df[, c1] != w1, c2]
  }
  if(tail=="R" | tail=="L"| tail=="T"){
    D_hyp <-  mean(sub.value1, na.rm=TRUE) - mean(sub.value2, na.rm=TRUE)
  }
  else{
    print("ERROR, tail should be R,L or T")+return(NA)
  }
  permutation_sample <- rep(0,m+l)
  permutation_sample[1:m]<-1
  D_null<- replicate(n,Permute_samples(permutation_sample,df,c2))

  if(plot){
    sigma=sd(D_null)
    myhist<-hist(D_null/sigma,100, prob=TRUE)
    multiplier <- myhist$counts / myhist$density
    mydensity <- density(D_null/sigma, adjust=2)
    mydensity$y <- mydensity$y * multiplier[1]
    plot(myhist,xlab="Z-score",main="Permutation Test")
    lines(mydensity, col='blue')
    # print(D_hyp)
    abline(v=D_hyp/sigma, col='red')
  }

  left_side=FALSE
  if(tail=="T"){
    if(D_hyp<0) {
      left_side=TRUE
    }
  }
  if(tail=="R" | (tail=="T" & left_side==FALSE)) {
    M<-mean(D_null>D_hyp) #estimate the one-tailed p-value by counting how many times the permuted samples have a higher difference than our hypothesis
  }
  else{
    if (tail=="L" | (tail=="T" & left_side==TRUE)){
      M<-mean(D_null<D_hyp) #estimate the one-tailed p-value by counting how many times the permuted samples have a lower difference than our hypothesis
    }
  }
  #print(D_hyp)
  #print(sigma)
  #print(D_hyp/sigma)
  #print(D_null)
  return(M)
}

Permutation(10000,ds,"work_year","salary_in_usd",2021,2020,"R")
#This is the right tail test to prove that Average Average Salary in 2021 is more than 2020.
#By the Permutation test, we can say that the Average Data Science Job salary in 2021 is more than in 2020.

#Average salary content with respect to the Experience Level.
x<-tapply(ds$salary, ds$experience_level, mean)
x
class(x)
barplot(x, ylab = "Average Salary", xlab = "Experience Level")

#Average salary content with respect to the Job Title.
y<-tapply(ds$salary, ds$job_title, mean)
y
class(y)
barplot(y, ylab = "Average Salary", xlab = "Job Title")

#Average salary content with respect to the Company Size.
z<-tapply(ds$salary, ds$company_size, mean)
z
class(z)
barplot(z, ylab = "Average Salary", xlab = "Company Size")

#Average salary content of Data Scientist with respect to the Remote Ratio.
w<-tapply(DSci$salary_in_usd, DSci$remote_ratio, mean)
w
class(w)
barplot(w, ylab = "Average Salary", xlab = "Remote Ratio")

#Average salary content with respect to the Work Year.
u<-tapply(ds$salary, ds$work_year, mean)
u
class(u)
plot(u,type="l", ylab = "Average Salary", xlab = "Work Year")


