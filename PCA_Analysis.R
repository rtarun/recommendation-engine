##PCA Algorithm on MyBhutan 'Attractions' dataset to understand the strength and relationship 
##of and between attributes associate with all 116 attraction. Three main steps involved in algorithm:
##1. Compute covariance matrix of mean-adjusted dataset
##2. Compute Eignevectors and Eigenvalues of matrix in step 1.
##3. Choose the top 

## Read the data and formatting
setwd("/Users/tarunruchandani/Documents/IEORE4111/MyBhutan/0420")
dat = read.csv("MBPopulatedData_04262016.csv")[,2:10]

### Start of Analysis
#######################
# n = number of attractions

n = nrow(dat)
m = ncol(dat)

##For PCA to work properly, we have to subtract the mean from each of the data dimensions.
##The mean subtracted is the average across each dimension.

mean.dat=matrix(NA,nrow = n, ncol = m)

for (i in 1:9)
{
mean.dat[1:116,i]=mean(dat[,i])
}
dat.adjusted = dat-mean.dat

##Covariance Matrix of mean adjusted dataset
dat.covariance = cov(dat.adjusted)

##Eigen Values and Eigne Vectors of Covariance Matrix
feature.vectors=eigen(dat.covariance)

##Selecting 5 Eigenvectors correponding to top 4 out of 9 Eigenvalues
##Together they represent 81% of the data

reduced.feature.vectors = feature.vectors$vectors[,1:4]

reduced.feature.vectors.t = t(reduced.feature.vectors)
#reduced.feature.vectors.t

dat.adjusted.t = t(dat.adjusted)

reduced.data=reduced.feature.vectors.t%*%dat.adjusted.t

reduced.data.t = t(reduced.data)
reduced.data.t

write.csv(reduced.data.t, "PCA_Output_04272016.csv")

##Adding mean to reduced data set to get scores as in original data set

#mean.reduced.dat=matrix(NA,nrow = 4, ncol = 116)

#reduced.data

#for (i in 1:4)
#{
#  mean.reduced.dat[i,1:116]=mean(reduced.data[i,])
#}

#mean.reduced.dat

#reduced.data.t.mean = reduced.data.t+mean.reduced.dat
#reduced.data.t.mean




