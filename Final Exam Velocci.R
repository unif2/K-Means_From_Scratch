A=matrix(data=c(-3.58,-3.02,-4.39,-5.04,-2.22,-2.31,0.88,0.04,1.01,0.43,2.10,3.18,4.59,1.87,2.92,0.13,0.71,-0.24,1.98,-0.14),nrow=10,ncol=2)
B=matrix(data=c(2,1,1,1,2,2,2,1,2,2),nrow=10,ncol=1)

for (n in 1:5){
centroid=matrix(0,2,2)
for (j in 1:2){ # For each cluster let's compute the centroid
m=0 # Initialize number of data points in cluster j
sum=rep(0,2) # Initialize sum for each feature
for (i in 1:10){
if(B[i]==j){
sum=sum+A[i,]
m=m+1
}
}
centroid[j,]=sum/m # jth row of centroid is the centroid for cluster j
}
for (i in 1:10){
distance=rep(0,2)
for (k in 1:2){
distance[k] = (A[i,1]-centroid[k,1])^2 + (A[i,2]-centroid[k,2])^2
}
if(distance[1]<distance[2]){
B[i]=1
}
else{
B[i]=2
}
}
}
B # The final clustering after 5 steps
plot(A[,1],A[,2]) # Can't figure out how to make the first 5 data points a different color