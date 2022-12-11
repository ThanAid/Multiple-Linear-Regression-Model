# Read file
file1<- read.table("vehicles.txt",header=TRUE)
attach(file1)
head(file1)
mydata<-subset(file1, select = -c(car)) # Drop car name
head(mydata)

# Correlation between variables
cormat <- cor(mydata)
head(cormat)

# Plotting correlation matrix
library(corrplot)
testcor=cor.mtest(mydata)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cormat, method="color", col=col(200),  
         type="upper",  
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = testcor$p, sig.level = 1, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

#Creating general Linear Model
mod1 <- lm(mpg~cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb)
mod1
summary(mod1)
plot(mod1)

# multicollinearity check
library(car)
vif(mod1) # VIF > 5 is a sign of multicollinearity

# Influence Index Plot
infIndexPlot(mod1, vars=c("Cook", "Studentized", "Bonf", "hat"),
             id=TRUE, grid=TRUE, main="Diagnostic Plots")

# More informative plots
library(olsrr)
ols_plot_cooksd_bar(mod1)
ols_plot_dffits(mod1)
ols_plot_dfbetas(mod1)

#############################AIC criterion######################################

# backward step
mod_back <- step(mod1, direction = 'backward', test='F', k = 2, trace=1)
mod_back$anova
print('Best Model based on the minimisation of the AIC criterion is (Backward step):')
mod_back
summary(mod_back)
ols_step_all_possible(mod_back)

# forward step
#define intercept-only model
intercept_only <- lm(mpg ~ 1, data=mydata)
#define model with all predictors
all <- lm(mpg ~ ., data=mydata)
#perform forward stepwise regression
mod_for <- step(intercept_only, direction='forward', test='F', scope=formula(all), trace=1)
mod_for$anova
print('Best Model based on the minimisation of the AIC criterion is (forward step):')
#view results of forward stepwise regression
mod_for$anova
#view final model
mod_for$coefficients
# summary of forward model
summary(mod_for)
ols_step_all_possible(mod_for)

# Both step
mod_both <- step(intercept_only, direction='both', test='F', scope=formula(all), trace=1)
mod_both$anova
print('Best Model based on the minimisation of the AIC criterion is (both step):')
mod_both
summary(mod_both)

###############################Comparison of models#############################

AIC(mod_back)
AIC(mod_for)
AIC(mod_both)

############################Final choice is forward model######################
summary(mod_for)
plot(mod_for)
# Influence Index Plot
infIndexPlot(mod_for, vars=c("Cook", "Studentized", "Bonf", "hat"),
             id=TRUE, grid=TRUE, main="Diagnostic Plots")

vif(mod_for) # VIF > 5 is a sign of multicollinearity

# Plots 

avPlots(mod_for)
crPlots(mod_for)


# Scatters of the variables according to mpg
x1 <- mydata$wt
x2 <- mydata$cyl
x3 <- mydata$hp
y <- mydata$mpg
plot(x1,y, xlab = 'wt', ylab = 'mpg')
plot(log(x1),y, xlab = 'log(wt)', ylab = 'mpg')
plot(x3,y, xlab = 'hp', ylab = 'mpg')
plot(log(x3),y, xlab = 'log(hp)', ylab = 'mpg')
plot(x2,y, xlab = 'cyl', ylab = 'mpg')

mod_opt1 <- lm(mpg ~ log(wt) + log(hp) + cyl)
summary(mod_opt1)
confint(mod_opt1, level=0.95)
# multicollinearity check
library(car)
vif(mod_opt1) # VIF > 5 is a sign of multicollinearity
ols_step_all_possible(mod_opt1)

# cyl variable contains 0 in conf d so we can ignore it
mod_opt <- lm(mpg ~ log(wt) + log(hp))
summary(mod_opt)
confint(mod_opt, level=0.95)
plot(mod_opt)
ols_step_all_possible(mod_opt)

################ Plots for optimized model ##################################### 
avPlots(mod_opt)
crPlots(mod_opt)

# multicollinearity check
library(car)
vif(mod_opt) # VIF > 5 is a sign of multicollinearity

# Influence Index Plot
infIndexPlot(mod_opt, vars=c("Cook", "Studentized", "Bonf", "hat"),
             id=TRUE, grid=TRUE, main="Diagnostic Plots")

# More informative plots
library(olsrr)
ols_plot_cooksd_bar(mod_opt)
ols_plot_dffits(mod_opt)
ols_plot_dfbetas(mod_opt)

################################################################################


################################ Predictions ###################################
# mazda rx7 fb
wt_rx7 <- 2.194
hp_rx7 <- 113
new_data <- data.frame(wt=wt_rx7, hp=hp_rx7)
predict(mod_opt,new_data, interval = 'prediction', level =.95) # real value 9.9 l/100km

#https://www.ultimatespecs.com/car-specs/Mazda/554/Mazda-RX-7-FB2-.html