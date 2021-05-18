#Author: Kathy Murphy
#Date: November 20th, 2020
#Purpose: Unsupervised learning and dimensionality reduction

View(USArrests)

apply(USArrests, 2, mean)
apply(USArrests, 2, sd)
#The apply functions just add together all of the values in each row and then 
#perform the selected function (mean or sd) using that value.

#If column data is used instead, we get the average/sd rates for these crimes
#and urban population percentages across all states.

pr.out <- prcomp(USArrests, scale=TRUE)

names(pr.out)
print(pr.out)

#Same as mean
pr.out$center

#Same as sd
pr.out$scale

#Each  column of rotation is the Principal Component loading
pr.out$rotation

#Principal component score vectors for each state are in x
head(pr.out$x)

#Create a biplot of the first two principal components (PCs)
biplot(pr.out, scale=0)

#Compute the proportion of variance explained (PVE) by each principal component
pr.var <- pr.out$sdev^2
pve <- pr.var / sum(pr.var)

#Scree plots
#Plot the PVE explained by each component and the cumulative PVE
plot(pve, xlab="Principal Component",
     ylab="Proportion of Variance Explained (PVE)",
     ylim=c(0,1),
     type='b')

plot(cumsum(pve), xlab="Principal Component",
     ylab="Cumulative Proportion of Variance Explained",
     ylim=c(0,1),
     type='b')

#By looking for "elbows" in the scree plots produced, two components are 
#sufficient


