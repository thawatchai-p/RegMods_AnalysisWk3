## Install packages and Load the library and data
install.packages("devtools")
devtools::install_github("jhudsl/collegeIncome")
library(collegeIncome)
data(college)

## Install the matahari R package
devtools::install_github("jhudsl/matahari")
library(matahari)

## Begin the documentation of your analysis
dance_start(value = FALSE, contents = FALSE)

## Load other necessary library
library(plyr)
library(dplyr)
library(ggplot2)

## Explore the college data
str(college) # There were 173 observations of 19 varibles

## Tidy up the college data
df25 <- data.frame(Major = college$major_category, Earning = college$p25th, P.Ranks = "25th")
df50 <- data.frame(Major = college$major_category, Earning = college$median, P.Ranks = "50th")
df75 <- data.frame(Major = college$major_category, Earning = college$p75th, P.Ranks = "75th")
df.all <- rbind(df25, df50, df75)

## Visualize the data characteristic
df.a <- df.all %>% group_by(Major, P.Ranks) %>% summarise(Mean = mean(Earning), SD = sd(Earning))
df.b <- summarySE(df.all, measurevar = "Earning", groupvars=c("Major","P.Ranks"))

## Plot the average of earning based on percentile ranks.
g0 <- ggplot(data = df.a, aes(x = reorder(Major, Mean), y = Mean, fill = P.Ranks))
g0 + geom_col(aes(fill = P.Ranks), position = "dodge") + labs(x= "Major Category", y="Average Earning") + coord_flip() + labs(title = "Average Earnings vs Major Catergory") + theme(plot.title = element_text(hjust = 0.5))

## Verify the interaction plot for a two-way anova. Square points represent means for groups, and error bars indicate standard errors of the mean.
pd = position_dodge(0.2)
g1 <- ggplot(df.b, aes(x = reorder(Major, Earning), y = Earning, color = P.Ranks))
g1 + geom_errorbar(aes(ymin = Earning-se, ymax = Earning+se), width = 0.2, size=0.7, position=pd) +
        geom_point(shape=15, size=4, position=pd) + theme_bw() +
        labs(x = "Major Category") +
        theme(axis.title.y = element_text(vjust = 1.8),
              axis.title.x = element_text(vjust= -0.5),
              axis.title = element_text(face = "bold"),
              axis.text.x = element_text(angle = 45, size = 10, hjust = 1))

## Create the model for each percentile earning
fit.25 <- lm(p25th ~ factor(major_category), data=college)
fit.50 <- lm(median ~ factor(major_category), data=college)
fit.75 <- lm(p75th ~ factor(major_category), data=college)
anova(fit.25)
anova(fit.50)
anova(fit.75)
## All model indicated that all model indicated the p-Value > 0.05, 
## it was fail to reject the null hypothesis. 
## Therefore, the major category was insignificant.

## Check the assumptions of the model
par(mfrow = c(3,1))
hist(residuals(fit.25), breaks = 15, col = "salmon")
hist(residuals(fit.50), breaks = 15, col = "forestgreen")
hist(residuals(fit.75), breaks = 15, col = "cornflowerblue")
## A histogram of residuals from a linear model. 
## The distribution of these residuals should be approximately normal.

## Perform the residual analysis of each model
par(mfrow = c(3,1))
plot(fitted(fit.25), resid(fit.25), pch = 16, col="salmon")
abline(h=0, col="black", lwd=2)
title("Residuals from 25th percentile ~ major category")
plot(fitted(fit.50), resid(fit.50), pch = 16, col="forestgreen")
abline(h=0, col="black", lwd=2)
title("Residuals from median ~ major category")
plot(fitted(fit.75), resid(fit.75), pch = 16, col="cornflowerblue")
abline(h=0, col="black", lwd=2)
title("Residuals from 75th percentile ~ major category")
## The residuals analysis result from each model indicated 
## the values were closed to zero, and randomly dispersed, 
## which indicated the models were unbiased and homoscedastic. 
## Therefore, a linear regression models were appropriated.

dance_save("./college_major_analysis.rds")
