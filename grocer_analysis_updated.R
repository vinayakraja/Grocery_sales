###############################################################################
# GROCERY STORE V RTC ANALYSIS
# AUTHOR:  VINAYAK RAJA
# DESCRIPTION: this script reads in appended data extracted from Alteryx. 
## Basic analyses are performed to validate the data. Relationships are
## investigated among variables. A regression model is created to 
## predict retail demand, with the hopes of isolating the effect 
## of certain customer segments likely to purchase RTC meals.

###############################################################################

library(ggplot2);
library(reshape2);

# Establish a working directory with the data files
setwd("\\\\C:/Home/vraja/Desktop/R_studio/Grocery_analysis")

# Read in the data
alteryx_df <- read.csv("zip-grocery-appended_data2.csv"
                       , stringsAsFactors = FALSE)


# Provide some more meaningful names
colnames(alteryx_df)[1] <- "zip5"
alt_data_colnames <- c("over18_cnt"
                       , "over65_cnt"
                       , "hh_income_median"
                       , "hh_cnt"
                       , "pop_cnt"
                       , "home_food_dol"
                       , "away_food_dol"
                       , "a03_hh_cnt"
                       , "a04_hh_cnt"
                       , "bo7_hh_cnt"
                       , "b08_hh_cnt"
                       , "b09_hh_cnt"
                       , "b10_hh_cnt"
                       , "f22_hh_cnt"
                       , "f23_hh_cnt"
                       , "g24_hh_cnt"
                       , "g25_hh_cnt"
                       , "k37_hh_cnt"
                       , "o51_hh_cnt"
                       , "o53_hh_cnt"
                       , "o54_hh_cnt"
                       , "supermarket_other_dmnd"
                       , "conven_dmnd"
                       , "grocer_dmnd"
)
colnames(alteryx_df)[10:ncol(alteryx_df)] <- alt_data_colnames
colnames(alteryx_df) <- tolower(colnames(alteryx_df))

# Need to create a new variable that is "total working population"
# This can calculated by subtracting pop over 65 (retired)
## from pop over 18
alteryx_df$working_pop_cnt <- alteryx_df$over18_cnt - alteryx_df$over65_cnt

# Understanding the structure of created dataframe and summary of dataframe
str(alteryx_df)
summary(alteryx_df)

#Creating the Complete Case Dataset with no NA.
groc_data<-alteryx_df[complete.cases(alteryx_df),]
sapply(groc_data, function(x) sum(is.na(x)))

sb_df<- groc_data[,2:ncol(groc_data)]
#finding the missing values 
sapply(alteryx_df, function(x) sum(is.na(x)))
# Analysis: out of 33144 ~ 3063 have missing values, so we can run our analysis  as complete case

#Building the histograms and Boxplot for each numeric variable

sub_data<- alteryx_df[,10:ncol(alteryx_df)] # Create subset for creating 

par(mfrow=c(2,2))
colnames <- dimnames(sub_data)[[2]]
for (i in 1:24) {
  hist(sub_data[,i] , main=colnames[i], breaks=20, col="blue", border="white")
  boxplot(sub_data[,i] , main=colnames[i])
  plot(sub_data[,i] , main=colnames[i])
}
# Analysis: Most of the variables are right skewed, need transformation.

#Seeing frequency distribution state wise
xtabs(~stateabbrev, data= alteryx_df )# (CA, NY, PA, OH, TX, IL) are states with most observations    

#Finding Corelations
library(corrplot);
cor_m <- cor(sub_data,use= "complete.obs")
par(mfrow=c(1,1))
corrplot(cor_m,method="shade", shade.col=NA, tl.col="black", tl.srt=45)
#Analysis: Few high corelations in data, no negative corelations

#Principal Component Analysis/ replacing NA values with 0 

sub_data[is.na(sub_data)] <- 0 #replacing with Zero 

fit1<- princomp(sub_data, cor=TRUE)
summary(fit1) # print variance accounted for 
pr_var<-fit1$sdev^2 #eigenvalues/variance
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]
plot(fit1,type="lines",main="Screeplot ") # scree plot 


# Extract loadings from FA fit for first 4 clusters
w=fit1$loadings[,1]
x = fit1$loadings[,2]
y = fit1$loadings[,3]
z = fit1$loadings[,4] 

# create and plot clusters based on the 4 components
hc = hclust(dist(cbind(w,x,y,z)), method = 'ward')
plot(hc, axes=F,xlab='', ylab='',sub ='', main='Comp 1/2/3/4')
rect.hclust(hc, k=4, border='tomato')

# Cluster analysis on loadings
#clust_fit <- hclust(dist(loadings_df))
#plot(clust_fit)
#str(clust_fit)

#ggdendrogram(clust_fit, rotate = TRUE, size = 4, theme_dendro = FALSE, color = "tomato")
#rect.hclust(clust_fit, k=3, border='red')
#Plotting of PCA 
# Control variable colors using their contribution
library("FactoMineR");
library("factoextra");
fviz_pca_var(fit1, col.var="contrib")+
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=55)+theme_bw()

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 4 factors and 3 factors, 
# with varimax rotation 
library(psych);
#conventional method didnt work 
#Error in optim(start, FAfn, FAgr, method = "L-BFGS-B", lower = lower,  : 
#L-BFGS-B needs finite values of 'fn'
#In addition: Warning message:
 # In log(e) : NaNs produced
##fit.2 <- factanal(subdata ,factors=4,rotation="varimax", data= sub_data)
fit_try<-fa(r=cor(sub_data), nfactors=4, rotate="varimax", SMC=FALSE, fm="minres")
print(fit_try, digits=2, cutoff=.3, sort=TRUE)
# Analysis: low threshold value for MR2 component, so considering 3 factore we can see.
fit_try2<-fa(r=cor(sub_data), nfactors=3, rotate="varimax", SMC=FALSE, fm="minres")# considering 3 factors
print(fit_try2, digits=2, cutoff=.3, sort=TRUE)
plot(fit_try)
fa.diagram(fit_try)

#Scatter plot between depedent varaible "Supermkt_demand" vs rest of independent varaibles 
par(mfrow=c(1,3))
for (i in 1:21){plot(sb_df[,i],sb_df[,22], ylab=" data column #1 DEPENDENT VARAIBLE", xlab=paste("data column #", i), main=paste("col 22 vs col ", i))}

# library(Boruta)
# 
# set.seed(123)
# boruta.train <- Boruta(sb_df[,22]~., data = sb_df[,1:21], doTrace = 2)
# print(boruta.train)
# 
# getSelectedAttributes(boruta.train)
# par(mfrow=c(2,2))
# plot(boruta.train)

# 
# 
#  
#  #Multicoolinearity 
# 
 library(car);
# lm_vif <- lm(sb_df[,22]~., data = sb_df[,1:21])
# # Evaluate Collinearity
# vif(lm_vif) # variance inflation factors 
# sqrt(vif(lm_vif)) > 2 # problem?
# summary(lm_vif)

#partition tree 

library(rpart);
 rpart.model <- rpart(sb_df[,22]~., data = sb_df[,1:21], method = "anova")
 printcp( rpart.model) # display the results 
 plotcp(rpart.model) # visualize cross-validation results 
 summary(rpart.model) # detailed summary of splits
 
 # create additional plots 
 par(mfrow=c(1,3)) # plot on one page 
 rsq.rpart(rpart.model) # visualize cross-validation results  	
 
 # plot tree 
 plot(rpart.model, uniform=TRUE, 
      main="Regression Tree ")
 text(rpart.model, use.n=TRUE, all=TRUE, cex=.8)
 
 
#interaction effect
 
 
# 
#  library(glmulti)
#  
#  Step<-glmulti(supermarket_other_dmnd ~  hh_income_median+ hh_cnt + pop_cnt+away_food_dol
# +a03_hh_cnt+ a04_hh_cnt+ bo7_hh_cnt+ b08_hh_cnt+ b09_hh_cnt+ b10_hh_cnt+f22_hh_cnt+ f23_hh_cnt+ g24_hh_cnt+ g25_hh_cnt
# +k37_hh_cnt+ o51_hh_cnt+o53_hh_cnt+o54_hh_cnt+working_pop_cnt , data=sb_df, 
#                level=1, 
#                method="d",
#                crit = "aic", 
#                confsetsize = 128,
#                plotty = T, report = T, 
#                fitfunction = "lm")
#  
#  plot(Step, type="s")
#  
#  
# LM_int <- lm(supermarket_other_dmnd  ~ .*., data=sb_df,  x=T)
# summary(LM_int)
# plot()
# sjp.lmint(LM_int)
#  library(caret)
# #interaction effect
# 
# int1<-lm(supermarket_other_dmnd ~ hh_income_median*hh_cnt , data=sb_df)
# summary(int1)
# ggplot(int1, aes(x =hh_income_median , y = supermarket_other_dmnd)) +
#   geom_ribbon(aes(ymin = LL, ymax = UL, fill = hh_cnt), alpha = .2) +
#   geom_line(aes(colour = hh_cnt), size=1)

#Craeting bins
library(Hmisc);

 
#Creating Bins

sb_df$hh_cnt_bin <- as.factor(cut2(sb_df$hh_cnt, g=3))
sb_df$hh_income_bin<- as.factor(cut2(sb_df$hh_income_median, g=3))
sb_df$working_pop_bin<- as.factor(cut2(sb_df$working_pop_cnt, g=3))
sb_df$away_food_bin<- as.factor(cut2(sb_df$away_food_dol, g=3))


# #counting the no of observations under each level
#  library(plyr)
#  count (sb_df$hh_income_bin)
 

#Between hh_cnt_bin and hh_income_median
sp1= ggplot(data=sb_df, aes(x=hh_income_median, y=supermarket_other_dmnd, color=hh_cnt_bin )) +
  geom_point()+
  scale_color_brewer(palette='Set1') 

sp1 + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
#Between working and hh_income_median
sp3= ggplot(data=sb_df, aes(x=hh_income_median, y=supermarket_other_dmnd, color=working_pop_bin )) +
  geom_point()+
  scale_color_brewer(palette='Set1')

sp3 + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

#Between working and hh_income_median
sp4= ggplot(data=sb_df, aes(x=hh_income_median, y=supermarket_other_dmnd, color=away_food_bin )) +
  geom_point()+
  scale_color_brewer(palette='Set1')

sp4 + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

#Between hh_cnt_bin and hh_income_median
sp2= ggplot(data=sb_df, aes(x=hh_cnt, y=supermarket_other_dmnd, color=hh_income_bin )) +
  geom_point()+
  scale_color_brewer(palette='Set1')

sp2 + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
#Between hh_cnt_bin and away_food
sp5= ggplot(data=sb_df, aes(x=hh_cnt, y=supermarket_other_dmnd, color=away_food_bin )) +
  geom_point()+
  scale_color_brewer(palette='Set1')

sp5 + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
#Between hh_cnt_bin and Working_pop
sp6= ggplot(data=sb_df, aes(x=hh_cnt, y=supermarket_other_dmnd, color=working_pop_bin )) +
  geom_point()+
  scale_color_brewer(palette='Set1')

sp6 + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

#Between  Working_pop and hh_cnt_bin 
sp7= ggplot(data=sb_df, aes(x=working_pop_cnt, y=supermarket_other_dmnd, color=hh_cnt_bin )) +
  geom_point()+
  scale_color_brewer(palette='Set1')

sp7+ geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

#Between  Working_pop and away_food
sp8= ggplot(data=sb_df, aes(x=working_pop_cnt, y=supermarket_other_dmnd, color=away_food_bin )) +
  geom_point()+
  scale_color_brewer(palette='Set1')

sp8+ geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
#Between  Working_pop and Income
sp9= ggplot(data=sb_df, aes(x=working_pop_cnt, y=supermarket_other_dmnd, color= hh_income_bin)) +
  geom_point()+
  scale_color_brewer(palette='Set1')
sp9+ geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

#Between away_food and hh_income

sp10= ggplot(data=sb_df, aes(x=away_food_dol, y=supermarket_other_dmnd, color= hh_income_bin)) +
  geom_point()+
  scale_color_brewer(palette='Set1')
sp10+ geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

#Between away_food and working_pop
sp11= ggplot(data=sb_df, aes(x=away_food_dol, y=supermarket_other_dmnd, color= working_pop_bin)) +
  geom_point()+
  scale_color_brewer(palette='Set1')
sp11+ geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

#Between away_food and hh_cnt
sp12= ggplot(data=sb_df, aes(x=away_food_dol, y=supermarket_other_dmnd, color= hh_cnt_bin)) +
  geom_point()+
  scale_color_brewer(palette='Set1')
sp12+ geom_smooth(method=lm, se=FALSE, fullrange=TRUE)



#Intial Model

int_lm<-lm(supermarket_other_dmnd ~  hh_income_median+ hh_cnt +away_food_dol
+a03_hh_cnt+ a04_hh_cnt+ bo7_hh_cnt+ b08_hh_cnt+ b09_hh_cnt+ b10_hh_cnt+f22_hh_cnt+ f23_hh_cnt+ g24_hh_cnt+ g25_hh_cnt
+k37_hh_cnt+ o51_hh_cnt+o53_hh_cnt+o54_hh_cnt+working_pop_cnt, data=sb_df)
summary(int_lm)
vif(int_lm) # variance inflation factors 
sqrt(vif(int_lm)) > 2 # problem?

#Calculae spatially smoothed income values.
library(geosphere)
km_cutoff <- 20
sb_df$avgincome_circle <- NA
sb_df$avgincome_norm <- NA

#Average everything within a circle of radius km_cutoff kilometers
for (i in 1:nrow(sb_df)) {
  if (i %% 100 == 0)# every 100th observation
    print(i)
  dists <- distHaversine(sb_df[i, c("lon", "lat")],
                         as.matrix(sb_df[, c("lon", "lat")]))
  sb_df$avgincome_circle[i] <- mean(sb_df$hh_income_median[dists < (km_cutoff * 1000)])
}

#Take a weighted average where closer observations get more weight.
km_cutoff <- 50
for (i in 1:nrow(sb_df)) {
  if (i %% 100 == 0)
    #print(i)
  dists <- distHaversine(sb_df[i, c("lon", "lat")],
                         as.matrix(sb_df[, c("lon", "lat")]))
  sb_df$avgincome_norm[i] <- weighted.mean(sb_df$hh_income_median,
                                      dnorm(dists, 0, 1000 * km_cutoff / 2))
}
#Average income Distance distribution within circle.
ggplot(sb_df) +
  aes(x = lon, y = lat, color = avgincome_circle) +
  geom_point()+scale_colour_gradient( low="orange", high="blue")

#Average income Distance distribution with normal distribution
ggplot(sb_df) +
  aes(x = lon, y = lat, color = avgincome_norm) +
  geom_point()+scale_colour_gradient( low="orange", high="blue")

#scatter plot between Average_circle and Average Norm
ggplot(sb_df) +
  aes(x = avgincome_circle, y = avgincome_norm) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "#FF0000")


#creating testing and training dataset
library(dplyr);
train_df<-sample_frac(sb_df, 0.8)
sid<-as.numeric(rownames(train_df)) # because rownames() returns character
test_df<-sb_df[-sid,]
city_icm_avg<- train_df %>%
group_by(stateabbrev,city) %>%
summarise(avg_inc= weighted.mean(hh_income_median,hh_cnt) ) %>%
ungroup()


#Intial Model on training dataest
log <- function(x) ifelse(x <= 0, 0, base::log(x))
library(car);
lm1<-lm(supermarket_other_dmnd ~  hh_income_median +away_food_dol +working_pop_cnt+hh_cnt 
           +a03_hh_cnt+ a04_hh_cnt+ bo7_hh_cnt+ b08_hh_cnt+ b09_hh_cnt+ b10_hh_cnt+f22_hh_cnt+ f23_hh_cnt+ g24_hh_cnt+ g25_hh_cnt
           +k37_hh_cnt+ o51_hh_cnt+o53_hh_cnt+o54_hh_cnt, data=train_df)
summary(lm1)
vif(lm1) # variance inflation factors 
sqrt(vif(lm1)) > 2 # problem?

# Assessing Outliers
outlierTest(lm1) # Bonferonni p-value for most extreme obs
qqPlot(lm1, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(lm1) # leverage plots


# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mtcars)-length(lm1$coefficients)-2)) 
plot(lm1, which=4, cook.levels=cutoff)


# Global test of model assumptions
library(gvlma);


# gvmodel <- gvlma(lm1) 
# 
# summary(gvmodel)

#running the intial model in test dataset
library(MASS)

lm2<-lm(supermarket_other_dmnd ~  hh_income_median +log(away_food_dol) +log(working_pop_cnt) 
        +a03_hh_cnt+ a04_hh_cnt+ bo7_hh_cnt+ b08_hh_cnt+ b09_hh_cnt+ b10_hh_cnt+f22_hh_cnt+ f23_hh_cnt+ g24_hh_cnt+ g25_hh_cnt
        +k37_hh_cnt+ o51_hh_cnt+o53_hh_cnt+o54_hh_cnt, data=test_df)
summary(lm2)
vif(lm2) # variance inflation factors 
sqrt(vif(lm2)) > 2 # problem?

# Assessing Outliers
outlierTest(lm2) # Bonferonni p-value for most extreme obs
qqPlot(lm2, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(lm2) # leverage plots


# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mtcars)-length(lm2$coefficients)-2)) 
plot(lm2, which=4, cook.levels=cutoff)


# # Global test of model assumptions
# library(gvlma);
# gvmodel <- gvlma(lm2) 
# summary(gvmodel)
# 
# #model selecetion 
# library("glmulti")

# #doing model selection
# par(mfrow=c(1,1))
# boxcox(supermarket_other_dmnd ~  hh_income_median +away_food_dol +working_pop_cnt 
#                +a03_hh_cnt+ a04_hh_cnt+ bo7_hh_cnt+ b08_hh_cnt+ b09_hh_cnt+ b10_hh_cnt+f22_hh_cnt+ f23_hh_cnt+ g24_hh_cnt+ g25_hh_cnt
#                +k37_hh_cnt+ o51_hh_cnt+o53_hh_cnt+o54_hh_cnt, data=train_df )
# 
# boxcox((supermarket_other_dmnd^0.9) ~  hh_income_median +away_food_dol +working_pop_cnt 
#        +a03_hh_cnt+ a04_hh_cnt+ bo7_hh_cnt+ b08_hh_cnt+ b09_hh_cnt+ b10_hh_cnt+f22_hh_cnt+ f23_hh_cnt+ g24_hh_cnt+ g25_hh_cnt
#        +k37_hh_cnt+ o51_hh_cnt+o53_hh_cnt+o54_hh_cnt,data=train_df, lambda = seq(0, 1, 0.1))


#proc glm for Super Market Demand

lm3 <- glm(supermarket_other_dmnd ~  hh_income_median +working_pop_cnt +hh_cnt
    +a03_hh_cnt+ a04_hh_cnt+ bo7_hh_cnt+ b09_hh_cnt+  b10_hh_cnt+f22_hh_cnt+ f23_hh_cnt+ g24_hh_cnt+ g25_hh_cnt
    +k37_hh_cnt+ o51_hh_cnt+o53_hh_cnt+o54_hh_cnt, data=train_df, family=Gamma(link=identity)) 

lm3_summary<- summary(lm3)
lm3_summary
# dispersion parameter OR MEAN SQUARE ERROR
lm3_summary$dispersion
#pseodu R sq.
ps_r2<- 1-(lm3$deviance/lm3$null.deviance)
ps_r2

#Multi-colineraity test
vif(lm3) # variance inflation factors 
sqrt(vif(lm3)) > 2 # problem?

#Add predicted values to the data frame
train_df$predicted_value <- predict(lm3)
train_df$residual <- lm3$resid

#Split the data into bins
train_df$prediction_bin <- cut(train_df$predicted_value, 
                                     c(0, 
                                       quantile(train_df$predicted_value, 
                                                seq(.01, .9, by = .1)), 
                                       Inf))

#Make a sequence of values for the x axis for drawing the density.
data_range_sequence <- seq(min(train_df$supermarket_other_dmnd),
                           max(train_df$supermarket_other_dmnd)
                           )

#Record the scale parameter
fit_scale <- 1 / lm3_summary$dispersion

#Loop through the bins
for (bin_index in 1:length(levels(train_df$prediction_bin))) {
  
  #Calculate the mean of the predicted values for this bin
  bin_mean <- mean(train_df$predicted_value[train_df$prediction_bin == levels(train_df$prediction_bin)[bin_index]])
  
  #Obtain the corresponding gamma density
  gamma_density <- dgamma(data_range_sequence, bin_mean / fit_scale, scale = fit_scale)
  
  #Make a histogram (using prob = TRUE to ensure that the area under the bars
  # sums to 1)
  hist(train_df$supermarket_other_dmnd[train_df$prediction_bin == levels(train_df$prediction_bin)[bin_index]], 
       prob = TRUE,
       main = paste("Bin:", levels(train_df$prediction_bin)[bin_index]),
       xlab = "Supermarket_demand")
  
  #Add the density line
  lines(density(train_df$supermarket_other_dmnd[train_df$prediction_bin == levels(train_df$prediction_bin)[bin_index]]), col="blue", lwd=2) # add a density estimate with defaults
  lines(density(train_df$supermarket_other_dmnd[train_df$prediction_bin == levels(train_df$prediction_bin)[bin_index]], adjust=2), lty="dotted", col="darkgreen", lwd=2) 
  
}
# #glm Diagnostic
# These plots are (upper left: residual vs linear predictor, 
# upper right: normal scores plots of standardized deviance residuals,
# # Lower left: approximate Cook statistics against leverage, Lower right: the plot of Cook statistic)
# library(boot)
# lm3_diag <- glm.diag(lm3)
# glm.diag.plots(lm3, lm3_diag)
# 
# #testing datset
# #proc glm 

lm_test <- glm(supermarket_other_dmnd ~  hh_income_median +working_pop_cnt +hh_cnt
             +a03_hh_cnt+ a04_hh_cnt+ bo7_hh_cnt+ b09_hh_cnt+  b10_hh_cnt+f22_hh_cnt+ f23_hh_cnt+ g24_hh_cnt+ g25_hh_cnt
             +k37_hh_cnt+ o51_hh_cnt+o53_hh_cnt+o54_hh_cnt, data=test_df, family=Gamma(link=identity)) 


lm_test_summary<- summary(lm_test)
lm_test_summary
# dispersion parameter OR MEAN SQUARE ERROR
lm_test_summary$dispersion
#pseodu R sq.
ps_r2<- 1-(lm_test$deviance/lm_test$null.deviance)
ps_r2

#Multi-colineraity test
vif(lm_test) # variance inflation factors 
sqrt(vif(lm_test)) > 2 # problem?


#Predicting Model working
getwd()
# save the model to disk
save(lm3, file="Grocery_SuperMKT_prediction.Rdata")
#loading the model
load("Grocery_SuperMKT_prediction.Rdata")
# score the test data and plot pred vs. obs 
# load libraries
library(caret)
library(mlbench)

# estimate skill on validation dataset
predictions <- (predict(lm3, newdata=test_df))
test_df$predicted_value<-predictions
test_df$residual <- (test_df$supermarket_other_dmnd-test_df$predicted_value)

#plot between predicted and actual value
ggplot(data=test_df, aes(x=supermarket_other_dmnd, y=predicted_value)) +
geom_point()+
scale_color_brewer(palette='Set1') 

#plot between predicted and residuals value
ggplot(data=test_df, aes(x=predicted_value, y=residual)) +
geom_point()+
scale_color_brewer(palette='Set1') 

# definately Heteroskedasticity.
sessionInfo(package = NULL)

#Intial Model
lm4 <- glm(supermarket_other_dmnd ~  hh_income_median +away_food_dol  +hh_cnt + working_pop_cnt
           +a03_hh_cnt+ a04_hh_cnt+ bo7_hh_cnt+  b10_hh_cnt+f22_hh_cnt+ f23_hh_cnt+ g24_hh_cnt+ g25_hh_cnt
           +k37_hh_cnt+ o51_hh_cnt+o53_hh_cnt+o54_hh_cnt, data=train_df, family=Gamma(link=identity)) 

lm4_summary<- summary(lm4)
lm4_summary

#Resturants
lm_resturants <- glm(away_food_dol ~  hh_income_median +working_pop_cnt +hh_cnt
           +a03_hh_cnt+ a04_hh_cnt+ bo7_hh_cnt+ b09_hh_cnt+  b10_hh_cnt+f22_hh_cnt+ f23_hh_cnt+ g24_hh_cnt+ g25_hh_cnt
           +k37_hh_cnt+ o51_hh_cnt+o53_hh_cnt+o54_hh_cnt, data=train_df, family=Gamma(link=identity)) 
summary(lm_resturants)

# dispersion parameter OR MEAN SQUARE ERROR
lm_test_summary$dispersion
#pseodu R sq.
ps_r2<- 1-(lm_test$deviance/lm_test$null.deviance)
ps_r2

#proc glm for Super Market Demand

lm_super <- glm(supermarket_other_dmnd ~  hh_income_median +working_pop_cnt +hh_cnt
           +a03_hh_cnt+ a04_hh_cnt+ bo7_hh_cnt+ b09_hh_cnt+  b10_hh_cnt+f22_hh_cnt+ f23_hh_cnt+ g24_hh_cnt+ g25_hh_cnt
           +k37_hh_cnt+ o51_hh_cnt+o53_hh_cnt+o54_hh_cnt, data=train_df, family=Gamma(link=identity)) 

lm_super_summary<- summary(lm_super)
lm_super_summary

# dispersion parameter OR MEAN SQUARE ERROR
lm_super_summary$dispersion
#pseodu R sq.
ps1_r2<- 1-(lm_super$deviance/lm_super$null.deviance)
ps1_r2

#Multi-colineraity test
vif(lm_super) # variance inflation factors 
sqrt(vif(lm_super)) > 2 # problem?


library(dplyr)
train_df <- train_df %>% 
  mutate(non_target_hh_cnt =hh_cnt- (a03_hh_cnt+ a04_hh_cnt+ bo7_hh_cnt+ b09_hh_cnt+  b10_hh_cnt+f22_hh_cnt+ f23_hh_cnt+ g24_hh_cnt+ g25_hh_cnt
                                    +k37_hh_cnt+ o51_hh_cnt+o53_hh_cnt+o54_hh_cnt+b08_hh_cnt) )  
#Model Iterations

lm1<- glm(supermarket_other_dmnd ~  hh_income_median+ non_target_hh_cnt+ hh_income_median*non_target_hh_cnt
          +a03_hh_cnt+ a04_hh_cnt+ bo7_hh_cnt+ b09_hh_cnt+b08_hh_cnt+  b10_hh_cnt+f22_hh_cnt+ f23_hh_cnt+ g24_hh_cnt+ g25_hh_cnt
          +k37_hh_cnt+ o51_hh_cnt+o53_hh_cnt+o54_hh_cnt +
            hh_income_median*a03_hh_cnt+ hh_income_median*a04_hh_cnt+ hh_income_median*bo7_hh_cnt+ 
            hh_income_median*b09_hh_cnt+hh_income_median*b08_hh_cnt+  hh_income_median*b10_hh_cnt+
            hh_income_median*f22_hh_cnt+ hh_income_median*f23_hh_cnt+ hh_income_median*g24_hh_cnt+ 
            hh_income_median*g25_hh_cnt
          +hh_income_median*k37_hh_cnt+ hh_income_median*o51_hh_cnt+hh_income_median*o53_hh_cnt+
            hh_income_median*o54_hh_cnt - 1, data=train_df, family=Gamma(link=identity)) 
summary(lm1)

lm1<- lm(supermarket_other_dmnd ~  hh_income_median+ non_target_hh_cnt+hh_income_median*non_target_hh_cnt
         +a03_hh_cnt+ a04_hh_cnt+ bo7_hh_cnt+ b09_hh_cnt+b08_hh_cnt+  b10_hh_cnt+f22_hh_cnt+ f23_hh_cnt+ g24_hh_cnt+ g25_hh_cnt
         +k37_hh_cnt+ o51_hh_cnt+o53_hh_cnt+o54_hh_cnt +
           hh_income_median*a03_hh_cnt+ hh_income_median*a04_hh_cnt+ hh_income_median*bo7_hh_cnt+ 
           hh_income_median*b09_hh_cnt+hh_income_median*b08_hh_cnt+  hh_income_median*b10_hh_cnt+
           hh_income_median*f22_hh_cnt+ hh_income_median*f23_hh_cnt+ hh_income_median*g24_hh_cnt+ 
           hh_income_median*g25_hh_cnt
         +hh_income_median*k37_hh_cnt+ hh_income_median*o51_hh_cnt+hh_income_median*o53_hh_cnt+
           hh_income_median*o54_hh_cnt - 1, data=train_df) 

lm1<- lm(supermarket_other_dmnd ~  hh_income_median
         +
           hh_income_median*a03_hh_cnt+ hh_income_median*a04_hh_cnt+ hh_income_median*bo7_hh_cnt+ hh_income_median*non_target_hh_cnt
           +hh_income_median*b09_hh_cnt+hh_income_median*b08_hh_cnt+  hh_income_median*b10_hh_cnt+
           hh_income_median*f22_hh_cnt+ hh_income_median*f23_hh_cnt+ hh_income_median*g24_hh_cnt+ 
           hh_income_median*g25_hh_cnt
         +hh_income_median*k37_hh_cnt+ hh_income_median*o51_hh_cnt+hh_income_median*o53_hh_cnt+
           hh_income_median*o54_hh_cnt - 1
         -a03_hh_cnt- a04_hh_cnt- bo7_hh_cnt- b09_hh_cnt-b08_hh_cnt-  b10_hh_cnt-f22_hh_cnt- f23_hh_cnt- g24_hh_cnt- g25_hh_cnt
         -k37_hh_cnt- o51_hh_cnt-o53_hh_cnt-o54_hh_cnt - non_target_hh_cnt, data=train_df) 

lm1<- lm(away_food_dol ~  hh_income_median+ non_target_hh_cnt
         +a03_hh_cnt+ a04_hh_cnt+ bo7_hh_cnt+ b09_hh_cnt+b08_hh_cnt+  b10_hh_cnt+f22_hh_cnt+ f23_hh_cnt+ g24_hh_cnt+ g25_hh_cnt
         +k37_hh_cnt+ o51_hh_cnt+o53_hh_cnt+o54_hh_cnt +
           hh_income_median*a03_hh_cnt+ hh_income_median*a04_hh_cnt+ hh_income_median*bo7_hh_cnt+ 
           hh_income_median*b09_hh_cnt+hh_income_median*b08_hh_cnt+  hh_income_median*b10_hh_cnt+
           hh_income_median*f22_hh_cnt+ hh_income_median*f23_hh_cnt+ hh_income_median*g24_hh_cnt+ 
           hh_income_median*g25_hh_cnt
         +hh_income_median*k37_hh_cnt+ hh_income_median*o51_hh_cnt+hh_income_median*o53_hh_cnt+
           hh_income_median*o54_hh_cnt - 1, data=train_df) 

lm1<- lm(away_food_dol ~  hh_income_median+ non_target_hh_cnt
         +
           hh_income_median*a03_hh_cnt+ hh_income_median*a04_hh_cnt+ hh_income_median*bo7_hh_cnt+ 
           hh_income_median*b09_hh_cnt+hh_income_median*b08_hh_cnt+  hh_income_median*b10_hh_cnt+
           hh_income_median*f22_hh_cnt+ hh_income_median*f23_hh_cnt+ hh_income_median*g24_hh_cnt+ 
           hh_income_median*g25_hh_cnt
         +hh_income_median*k37_hh_cnt+ hh_income_median*o51_hh_cnt+hh_income_median*o53_hh_cnt+
           hh_income_median*o54_hh_cnt - 1
         -a03_hh_cnt- a04_hh_cnt- bo7_hh_cnt- b09_hh_cnt-b08_hh_cnt-  b10_hh_cnt-f22_hh_cnt- f23_hh_cnt- g24_hh_cnt- g25_hh_cnt
         -k37_hh_cnt- o51_hh_cnt-o53_hh_cnt-o54_hh_cnt, data=train_df) 

lm1<- glm(supermarket_other_dmnd ~  hh_income_median+ non_target_hh_cnt
          +a03_hh_cnt+ a04_hh_cnt+ bo7_hh_cnt+ b09_hh_cnt+b08_hh_cnt+  b10_hh_cnt+f22_hh_cnt+ f23_hh_cnt+ g24_hh_cnt+ g25_hh_cnt
          +k37_hh_cnt+ o51_hh_cnt+o53_hh_cnt+o54_hh_cnt +
            hh_income_median*a03_hh_cnt+ hh_income_median*a04_hh_cnt+ hh_income_median*bo7_hh_cnt+ 
            hh_income_median*b09_hh_cnt+hh_income_median*b08_hh_cnt+  hh_income_median*b10_hh_cnt+
            hh_income_median*f22_hh_cnt+ hh_income_median*f23_hh_cnt+ hh_income_median*g24_hh_cnt+ 
            hh_income_median*g25_hh_cnt
          +hh_income_median*k37_hh_cnt+ hh_income_median*o51_hh_cnt+hh_income_median*o53_hh_cnt+
            hh_income_median*o54_hh_cnt - 1, data=train_df, family=Gamma(link=identity)) 

lm1<- lm(supermarket_other_dmnd ~  hh_income_median+ non_target_hh_cnt
          +a03_hh_cnt+ a04_hh_cnt+ bo7_hh_cnt+ b09_hh_cnt+b08_hh_cnt+  b10_hh_cnt+f22_hh_cnt+ f23_hh_cnt+ g24_hh_cnt+ g25_hh_cnt
          +k37_hh_cnt+ o51_hh_cnt+o53_hh_cnt+o54_hh_cnt +
             hh_income_median*a03_hh_cnt+ hh_income_median*a04_hh_cnt+ hh_income_median*bo7_hh_cnt+ 
            hh_income_median*b09_hh_cnt+hh_income_median*b08_hh_cnt+  hh_income_median*b10_hh_cnt+
            hh_income_median*f22_hh_cnt+ hh_income_median*f23_hh_cnt+ hh_income_median*g24_hh_cnt+ 
            hh_income_median*g25_hh_cnt
          +hh_income_median*k37_hh_cnt+ hh_income_median*o51_hh_cnt+hh_income_median*o53_hh_cnt+
            hh_income_median*o54_hh_cnt - 1, data=train_df) 

lm1<- lm(supermarket_other_dmnd ~  hh_income_median+ non_target_hh_cnt
         +
           hh_income_median*a03_hh_cnt+ hh_income_median*a04_hh_cnt+ hh_income_median*bo7_hh_cnt+ 
           hh_income_median*b09_hh_cnt+hh_income_median*b08_hh_cnt+  hh_income_median*b10_hh_cnt+
           hh_income_median*f22_hh_cnt+ hh_income_median*f23_hh_cnt+ hh_income_median*g24_hh_cnt+ 
           hh_income_median*g25_hh_cnt
         +hh_income_median*k37_hh_cnt+ hh_income_median*o51_hh_cnt+hh_income_median*o53_hh_cnt+
           hh_income_median*o54_hh_cnt - 1
         -a03_hh_cnt- a04_hh_cnt- bo7_hh_cnt- b09_hh_cnt-b08_hh_cnt-  b10_hh_cnt-f22_hh_cnt- f23_hh_cnt- g24_hh_cnt- g25_hh_cnt
         -k37_hh_cnt- o51_hh_cnt-o53_hh_cnt-o54_hh_cnt, data=train_df) 

lm1<- lm(away_food_dol ~  hh_income_median+ non_target_hh_cnt
         +a03_hh_cnt+ a04_hh_cnt+ bo7_hh_cnt+ b09_hh_cnt+b08_hh_cnt+  b10_hh_cnt+f22_hh_cnt+ f23_hh_cnt+ g24_hh_cnt+ g25_hh_cnt
         +k37_hh_cnt+ o51_hh_cnt+o53_hh_cnt+o54_hh_cnt +
           hh_income_median*a03_hh_cnt+ hh_income_median*a04_hh_cnt+ hh_income_median*bo7_hh_cnt+ 
           hh_income_median*b09_hh_cnt+hh_income_median*b08_hh_cnt+  hh_income_median*b10_hh_cnt+
           hh_income_median*f22_hh_cnt+ hh_income_median*f23_hh_cnt+ hh_income_median*g24_hh_cnt+ 
           hh_income_median*g25_hh_cnt
         +hh_income_median*k37_hh_cnt+ hh_income_median*o51_hh_cnt+hh_income_median*o53_hh_cnt+
           hh_income_median*o54_hh_cnt - 1, data=train_df) 

lm1<- lm(away_food_dol ~  avgincome_circle+ non_target_hh_cnt+ avgincome_circle* non_target_hh_cnt
         +
           avgincome_circle*a03_hh_cnt+ avgincome_circle*a04_hh_cnt+ avgincome_circle*bo7_hh_cnt+ 
           avgincome_circle*b09_hh_cnt+avgincome_circle*b08_hh_cnt+  avgincome_circle*b10_hh_cnt+
           avgincome_circle*f22_hh_cnt+ avgincome_circle*f23_hh_cnt+ avgincome_circle*g24_hh_cnt+ 
           avgincome_circle*g25_hh_cnt
         +avgincome_circle*k37_hh_cnt+ avgincome_circle*o51_hh_cnt+avgincome_circle*o53_hh_cnt+
           avgincome_circle*o54_hh_cnt - 1-a03_hh_cnt- a04_hh_cnt- bo7_hh_cnt- b09_hh_cnt-b08_hh_cnt-  b10_hh_cnt-f22_hh_cnt- f23_hh_cnt- g24_hh_cnt- g25_hh_cnt
         -k37_hh_cnt- o51_hh_cnt-o53_hh_cnt-o54_hh_cnt - non_target_hh_cnt, data=train_df) 

#Spatial Model

lm2<- lm(away_food_dol ~  avgincome_norm+ non_target_hh_cnt+ avgincome_norm* non_target_hh_cnt
         +
           avgincome_norm*a03_hh_cnt+ avgincome_norm*a04_hh_cnt+ avgincome_norm*bo7_hh_cnt+ 
           avgincome_norm*b09_hh_cnt+avgincome_norm*b08_hh_cnt+  avgincome_norm*b10_hh_cnt+
           avgincome_norm*f22_hh_cnt+ avgincome_norm*f23_hh_cnt+ avgincome_norm*g24_hh_cnt+ 
           avgincome_norm*g25_hh_cnt
         +avgincome_norm*k37_hh_cnt+ avgincome_norm*o51_hh_cnt+avgincome_norm*o53_hh_cnt+
           avgincome_norm*o54_hh_cnt - 1
         , data=train_df) 


summary(lm2)


avgincome_norm+ non_target_hh_cnt
+
  avgincome_norm*a03_hh_cnt+ avgincome_norm*a04_hh_cnt+ avgincome_norm*bo7_hh_cnt+ 
  avgincome_norm*b09_hh_cnt+avgincome_norm*b08_hh_cnt+  avgincome_norm*b10_hh_cnt+
  avgincome_norm*f22_hh_cnt+ avgincome_norm*f23_hh_cnt+ avgincome_norm*g24_hh_cnt+ 
  avgincome_norm*g25_hh_cnt
+avgincome_norm*k37_hh_cnt+ avgincome_norm*o51_hh_cnt+avgincome_norm*o53_hh_cnt+
  avgincome_norm*o54_hh_cnt - 1


