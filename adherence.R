##########################################################################################
# Experimental Statistical Analysis for Functional Health Literacy on Treatment Adherence 
# among Type 2 Diabetic Clients' Survey Data
#
# Alfeo Sabay
#
#########################################################################################

library(plyr)
library(pastecs)
library(vcd)
library(car)
library(dplyr)
library(ggplot2)
library(tidyr)
library(xtable)
library(pwr)
library(corrplot)
library(effects)

#######################################################################################
# Data loading and preparation section
#######################################################################################

rdata <- read.csv("data/adherence_recoded-2.csv")
summary(rdata)
demog <- read.csv("data/Demographics.csv")
ddata <- subset(x=demog, select=-c(X))


# select data columns of interest
sel_data <- subset(x=rdata, select = c(nvs, Regimen, Belief, Recall, Access, Total.Score))
sel_data <- rename(sel_data, NVS=nvs, BMQ=Total.Score)
# add demographic columns
dt <- cbind(sel_data, ddata, deparse.level = 0)
dt <- rename(dt, Other_Medications=Other.Medications)

# create Literacy_Level Ordinal Category Variable (dplyr style)
dt <- dt %>%
  mutate(Literacy_level = case_when((NVS >= 0 & NVS <=1)~"limited",
                                    (NVS >= 2 & NVS <=3)~"possibly",
                                    (NVS >= 4 & NVS <=6)~"adequate") )

# make Literacy_level ordinal factor
dt$Literacy_level <- factor(dt$Literacy_level, order=TRUE, 
                            levels=c("limited","possibly", "adequate"))


# make Gender a factor variable
dt$Gender <- trimws(dt$Gender) # get rid of whitespace
dt$Gender <- factor(dt$Gender)

# convert Other_Medications to lower case
dt$Other_Medications <- tolower(dt$Other_Medications)

# make Other_Medications a factor variable
dt$Other_Medications <- trimws(dt$Other_Medications)
dt$Other_Medications <- factor(dt$Other_Medications)

# recode belief, regimen, recall, access to binary
dt <- dt%>%
  mutate(Belief=if_else(Belief > 0, 1, 0),
         Recall=if_else(Recall > 0, 1, 0),
         Regimen=if_else(Regimen > 0, 1, 0),
         Access=if_else(Access > 0, 1, 0))

freq_theme <- theme(legend.position="none",text = element_text(colour="Dark Blue"),
                    axis.text = element_text(size=10,color="Black"),
                    axis.title = element_text(size = rel(1.5)))

ggplot(dt, aes(x=dt$BMQ, fill=5, color="red")) + geom_histogram(bins=11) + 
  labs(title="Adherence Original Scale: 0 = Adherent", x="BMQ", y="Frequency") +
  freq_theme



#hist(dt$BMQ, main="Adherence Original Scale: 0 = Adherent", xlab = "BMQ")

# convert BMQ to binary (0=adherent, 1=non-adherent)
dt <- dt%>%
  mutate(BMQ=if_else(BMQ>0, "Non-Adherent","Adherent"))
  #mutate(BMQ=if_else(BMQ>0, 1,0))

#dt$BMQ <- factor(dt$BMQ, order=TRUE, levels = c("Adherent","Non-Adherent") )
dt$BMQ <- factor(dt$BMQ)

# invert
# dt$BMQ <- 1-dt$BMQ 

# split data into fit and test
## 80% of the sample size
smp_size <- floor(0.8 * nrow(dt))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(dt)), size = smp_size)

dt_fit <- dt[train_ind, ]
dt_test <- dt[-train_ind, ]


##########################################################################################
# Descriptive Statistics
##########################################################################################
summary(dt)

# plots

# NVS
ggplot(dt, aes(x=dt$NVS, fill=5, color="red")) + geom_histogram(bins=7) + 
  labs(title="NVS", x="NVS", y="Frequency") +
  freq_theme

# combined
ggplot(dt, aes(x=dt$BMQ, fill=5)) + geom_bar() + 
  labs(title="BMQ", x="BMQ", y="Frequency") + freq_theme

# by Literacy
ggplot(dt, aes(x=dt$BMQ, fill=5)) + geom_bar() + facet_grid(~dt$Literacy_level) + 
    labs(title="BMQ Groups by Literacy Levels", x="BMQ", y="Frequency") + freq_theme

# by gender
ggplot(dt, aes(x=dt$Gender, fill=5)) + geom_bar() + facet_grid(~dt$BMQ) + 
  labs(title="BMQ Groups by Gender", x="Gender", y="Frequency") + freq_theme

# by Age
ggplot(dt, aes(x=dt$Age, fill=5)) + geom_histogram(binwidth = 1) + facet_grid(~dt$BMQ) + 
  labs(title="BMQ Groups by Age", x="Age", y="Frequency") + freq_theme

# by Other Medications
ggplot(dt, aes(x=dt$Other_Medications,  fill=5)) + geom_bar() + facet_grid(~dt$BMQ) + 
  labs(title="BMQ Groups by Other Medications", x="Other Medications", y="Frequency") + freq_theme


# power analysis alpha = .05, Power = .80, Effect Size = 


#myvars <- c("NVS", "BMQ", "Age", "Literacy_level", "Gender")

# Numeric variable of interest stats
#stat.desc(dt[myvars])

# Chi-Square Test for Independence - Null Hypothesis -> Variables are independent?
v_table <- xtabs(~Literacy_level+BMQ, data=dt)
chisq.test(v_table)

# Check correlation patterns
#scatterplotMatrix(~BMQ+NVS+Regimen+Belief+Recall+Access+Age+Gender+Literacy_level+Other_Medications,
#                  data=dt, spread=FALSE, 
#                  main="Scatter Plot Matrix for Adherence Variables")

adcor <- cor(dt)

# logistic regression model
fit.full <- glm(BMQ~Literacy_level+Gender+Age+Other_Medications, 
                family=binomial(link="logit"), data = dt)

summary(fit.full)
plot(allEffects(fit.full))

# refit with reduced predictors (less Other_Medications, Gender)
fit.reduced <- glm(BMQ~Literacy_level+Age, 
                family=binomial(link="logit"), data = dt)

summary(fit.reduced)
plot(allEffects(fit.reduced))
# Chi-squared analysis of variance to see if the reduced model is just as good as the full model

anova(fit.reduced, fit.full, test="Chisq")

# examine the impact of coefficients of the reduced model, 
coef(fit.reduced)
# exp to scale up to odds scale, because it's difficult to interpret the response which is log(odds) at Y=1
exp(coef(fit.reduced))

# coefficients with confidence intervals
ci <- exp(confint(fit.reduced))
ci

# perform test prediction by varying literacy level to get probabilities
test_data <- data.frame(Literacy_level=c("limited", "possibly", "adequate"), Age=c(mean(dt$Age)))
test_data$Literacy_level <- factor(test_data$Literacy_level, order=TRUE, 
                            levels=c("limited","possibly", "adequate"))

test_data$prob <- predict(fit.reduced, newdata=test_data, type="response")
test_data

# perform test prediction by varying Age to get probabilities
test_data <- data.frame(Literacy_level=c("limited", "limited", "limited"), Age=c(50,60,70))
test_data$Literacy_level <- factor(test_data$Literacy_level, order=TRUE, 
                                   levels=c("limited","possibly", "adequate"))

test_data$prob <- predict(fit.reduced, newdata=test_data, type="response")
test_data

# perform test prediction by varying Age to get probabilities
test_data <- data.frame(Literacy_level=c("possibly", "possibly", "possibly"), Age=c(50,60,70))
test_data$Literacy_level <- factor(test_data$Literacy_level, order=TRUE, 
                                   levels=c("limited","possibly", "adequate"))
test_data$prob <- predict(fit.reduced, newdata=test_data, type="response")
test_data

# perform test prediction by varying Age to get probabilities
test_data <- data.frame(Literacy_level=c("adequate", "adequate", "adequate"), Age=c(50,60,70))
test_data$Literacy_level <- factor(test_data$Literacy_level, order=TRUE, 
                                   levels=c("limited","possibly", "adequate"))

test_data$prob <- predict(fit.reduced, newdata=test_data, type="response")
test_data

# test with 20% of survey data - set aside earlier
rpt_vars <- c("BMQ", "Literacy_level", "Age", "prob")
dt_test$Age <- mean(dt_test$Age)
dt_test$prob <- predict(fit.reduced, newdata=dt_test, type="response")
dt_test[rpt_vars]


# power plot
library(pwr)

vw <- seq(6, 300, 1)
vpw <- length(vw)

p <- NULL
for (i in 1:vpw){
  result <- pwr.f2.test(u=4, v = (vw[i]-5), f2=.48,sig.level = .05)
  p[i] <- result$power
}

plot(vw,p, type = "l", lwd=2, col="red",
     ylab = "Power",
     xlab = "Sample Size",
     main = "Power VS Sample Size Plot with Effect Size = .48 and Alpha = .05")


dt %>%
  group_by(BMQ) %>%
  summarise(
    n = n(),
    total = sum(n)
  )

dt %>%count(NVS)
