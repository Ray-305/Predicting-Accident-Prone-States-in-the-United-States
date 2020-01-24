
library(readr)
library(tidyverse)
library(dplyr)
library(cluster)
library(gridExtra)

urlfile="https://raw.githubusercontent.com/fivethirtyeight/data/master/bad-drivers/bad-drivers.csv"

mydata<-read_csv(url(urlfile))


head(mydata)

car_acc= mydata %>%
  select (State, `Number of drivers involved in fatal collisions per billion miles`,`Percentage Of Drivers Involved In Fatal Collisions Who Were Speeding`,`Percentage Of Drivers Involved In Fatal Collisions Who Were Alcohol-Impaired`,`Percentage Of Drivers Involved In Fatal Collisions Who Had Not Been Involved In Any Previous Accidents`,`Percentage Of Drivers Involved In Fatal Collisions Who Were Not Distracted`,`Car Insurance Premiums ($)`)


str(car_acc)
colnames(car_acc) <- c("state","accident","speeding","alcohol","not_distracted","no_prevaccident","premium")

head(car_acc)

install.packages("GGally")
library(GGally)

dat_summ <- summary(mydata)
print(dat_summ)

library(tidyverse)
library(GGally)
car_acc %>% 
  select(-state) %>%
  ggpairs()

corr_col <- car_acc %>% 
  select(-state) %>% 
  cor()

# Print the correlation coefficient for all column pairs
print(corr_col)

#fitting regression

fit_reg <- lm(accident~speeding+alcohol+not_distracted+no_prevaccident+premium,car_acc)
fit_coef <- coef(fit_reg)
fit_reg
print(fit_coef)

car::vif(fit_reg)

car_acc_std <- car_acc %>% 
  mutate(speeding=scale(speeding),
         alcohol=scale(alcohol),
         not_distracted=scale(not_distracted), 
         no_prevaccident=scale(no_prevaccident),
         premium=scale(premium))

#pca_fit <- princomp(car_acc_std[,c("speeding", "alcohol", "not_distracted","no_prevaccident","premium")])
#pca_fit <- princomp(car_acc_std[,c("speeding", "alcohol", "not_distracted","no_prevaccident")])
pca_fit <- princomp(car_acc_std[,c("speeding", "alcohol", "not_distracted")])
#pca_fit <- princomp(car_acc_std[,c("speeding", "alcohol", "not_distracted","premium")]) 61%

pca_fit
# Obtain the proportion of variance explained by each principle component
(pr_var <- pca_fit$sdev^2)
(pve <- pr_var / sum(pr_var))

# Plot the proportion of variance explained, draw a point plot connected with lines
data_frame(comp_id=1:length(pve) , pve ) %>%
  ggplot( aes(x=comp_id , y=pve) ) + geom_point() + geom_line() + ggtitle("Proportion of Variance Explained")+ theme(plot.title = element_text(hjust = 0.5))+
  coord_cartesian(ylim=c(0,0.5)) +
  labs(x="", 
       y="")
screeplot(pca_fit)

# Compute the cumulative proportion of variance and extract the variance explained by the first two principal components
cve <- cumsum(pve)
cve_pc2 <- cve[2]
print(cve_pc2)


pcomp1 <- pca_fit$scores[,1]
pcomp2 <- pca_fit$scores[,2]

# Plot the first 2 principle components in a scatterplot using ggplot
data_frame(pcomp1,pcomp2) %>%
  ggplot(aes(pcomp1,pcomp2)+geom_point()+ labs(x="Principle Component 1", y= "Principle Component 2")

# Create a vector of 1 to 10 
k_vec <- 1:10

# Initialise vector of inertias
inertias <- rep(NA, length(k_vec))

# Initialise empty list to save K-mean fits 
mykm <- list()

# Set the seed of random number generator 
set.seed(1)
for (k in k_vec) {
  # for each k, fit a K-mean model with k clusters and save it in the mykm list
  mykm[[k]] <- kmeans(car_acc_std[,c(3,4,5)], centers = k, nstart=50)
  # for each k, get the within-cluster sum-of-squares and save
  inertias[k] <- mykm[[k]]$tot.withinss           
}


# Plot the within-cluster sum-of-squares against the number of clusters used
data_frame(k_vec,inertias) %>%
  ggplot( aes(k_vec, inertias) ) +
  geom_point() + geom_line() +
  labs(x="Number of clusters", y="Intertias")

cluster_id <- as.factor(mykm[[3]]$cluster)

# Color the points of the principle component plot according to their cluster number
data_frame(pcomp1,pcomp2) %>%
  ggplot(aes(x=pcomp1,y=pcomp2,col=cluster_id)) + geom_point() +
  labs(x="Principle Component 1",
       y="Principle Component 2")

#install.packages("factoextra")
library(factoextra)
head(car_acc_std)

#car_temp =car_acc_std[,c(3,4,5)]
#rownames(car_temp) = car_acc_std$state

fviz_cluster(mykm[[3]], car_acc_std[,c(3,4,5)])

#fviz_cluster(list(mykm[[7]], car_acc_std[,c(3,4,5)]))

fviz_nbclust(car_acc_std[,c(3,4,5)], kmeans, method = "wss")
fviz_nbclust(car_acc_std[,c(3,4,5)], kmeans, method = "silhouette")



#distance
linkage.list=c( "average")

dist.list=c("manhattan")

for(i in 1:length(dist.list)){
  for(j in 1:length(linkage.list)){
    
    d = dist(car_acc_std, method=dist.list[i])
    hc=hclust(d, metho=linkage.list[j])
    dev.new()
    plot(hc, main=paste(dist.list[i], "_", linkage.list[j]))
  }
}
