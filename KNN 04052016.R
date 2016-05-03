## Read the data and formatting
setwd("/inputWorkingDirectory/")
dat = read.csv("Attractions.csv")[,3:13]

### Start of Analysis
#######################
# n = number of attractions
dat=dat[1:116,]
rownames(dat) = dat[,1]
dat = as.matrix(dat[,-1])

n = nrow(dat)
m = ncol(dat)


# Calculate similarity between each two attractions
####

sim = matrix(NA,nrow = n, ncol = n) 
for (i in 1:n)
{
  for (j in 1:n)
  {
    d = dat[i,] - dat[j,]
    sim[i,j] = sqrt(sum(d*d, na.rm = TRUE))
  }
}
colnames(sim) = rownames(dat)
rownames(sim) = rownames(dat)


# Finding the most similar attractions
output = data.frame()

for (u in 1:n)
{
  sim1 = sim[,-u]
  sim2 = sim1[, order(sim1[u,]) ]
  sim2 = sim2[,1:5]
  output1=list()
  for (x in 1:5)
  {
    if (sim2[u,x]<5) {
      y = colnames(sim2)[x]
      output1[x] = y
    } else {
      y = 'NA'
      output1[x] = y
    }
  }
  output2 =matrix(unlist(output1), ncol=length(output1), byrow = TRUE)
  output=rbind(output,output2)
}

rownames(output) = rownames(dat)

write.csv(output, "Recommended Attractions 04272016.csv")
