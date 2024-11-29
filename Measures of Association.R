#### One common task in epidemiology is to quantify the strength of association between exposures (‘risk factors’) i.e., variables whose association with the outcome is to be estimated, and disease outcomes.

#### Notations
# Before getting into measures of association, it is important to understand the notation used in epidemiology to convey exposure and disease data: 2 x 2 table.

# Look at the data below on smoking and hypertension:
# This is a 100-person study on smoking and hypertension, where "Y" indicates Yes and "N" indicates No. 


# creating a data frame
cema1 <- data.frame(
  Participant = c(1:100), 
  Smoking = c("Y","Y","Y","Y","N","N","N","N","N","N", "Y","Y","Y","Y","N","N","N","N","N","N", "Y","Y","Y","Y","N","N","N","N","N","N", "Y","Y","Y","Y","N","N","N","N","N","N", "Y","Y","Y","Y","N","N","N","N","N","N", "Y","Y","Y","Y","N","N","N","N","N","N", "Y","Y","Y","Y","N","N","N","N","N","N", "Y","Y","Y","Y","N","N","N","N","N","N", "Y","Y","Y","Y","N","N","N","N","N","N", "Y","Y","Y","Y","N","N","N","N","N","N"),
  Hypertension = c("Y","N","Y","Y","N","Y","N","N","Y","N", "Y","N","Y","Y","N","Y","N","N","Y","N", "Y","N","Y","Y","N","Y","N","N","Y","N", "Y","N","Y","Y","N","Y","N","N","Y","N", "Y","N","Y","Y","N","Y","N","N","Y","N", "Y","N","Y","Y","N","Y","N","N","Y","N", "Y","N","Y","Y","N","Y","N","N","Y","N", "Y","N","Y","Y","N","Y","N","N","Y","N", "Y","N","Y","Y","N","Y","N","N","Y","N", "Y","N","Y","Y","N","Y","N","N","Y","N"),
  stringsAsFactors = FALSE
)
# print the data frame
print(cema1)

####Explore this data first:
# a) What's the exposure and what's the outcome?
# b) How many smokers are there?
# c) How many participants are have hypertension?
# d) Create a 2x2 table
#### A few basic concepts?
# a) What's the P(D+)
# b) What's the Prevalence of hypertension among the participants?
# c) What is the risk of hypertension among smokers?
# d) What is the rate of hypertension among non-smokers?
# e) Does smoking contribute to or reduce hypertension in this population?
# f) AR - What is the rate of hypertension among smokers that is due to smoking?
# g) AFe - What is the proportion of disease among smokers that is due to the smoking, assuming the relationship is causal?
# h) PAR - What is the increase in risk of hypertension in the entire population that is attributable to the smoking?
# i) AFp - What is the proportion of hypertension in the whole population that is attributable to smoking, and would be avoided if the smoking were to be banned in this population?

#############################################################################################
#############################################################################################

#### Now let's turn to R

#install.packages("epiR")
install.packages("epiR")
library(epiR)

table(cema1$Hypertension, cema1$Smoking)

####Relevel factors for ease

cema1$Smoking <- factor(cema1$Smoking, levels = c("Y", "N"))
cema1$Hypertension <- factor(cema1$Hypertension, levels = c("Y", "N"))

####View our 2x2 table 

table(cema1$Hypertension, cema1$Smoking)


###Create our 2x2 
cema1.1 <- c(30,20,10,40)


# View the data in the usual 2 by 2 table format:
cema1.2 <- matrix(cema1.1, nrow = 2, byrow = TRUE, 
                  dimnames = list(c("Smoking (Yes)", "Smoking (No)"),
                                  c("Hypertension (Yes)", "Hypertension (No)")))
#############################################################################################
#############################################################################################

#### Calculate the outcome 
# A) prevalence ratio 
# B) odds ratio 
# C) attributable prevalence in the exposed 
# D) attributable fraction in the exposed 
# E) attributable prevalence in the population, and 
# F) the attributable fraction in the population 

## Note that we use the term prevalence risk ratio and prevalence odds ratio (instead of incidence risk ratio and incidence odds ratio) because we’re dealing with data from a cross-sectional study — the frequency of disease is expressed as a prevalence, not an incidence.


epi.2by2(dat = cema1.2, method = "cross.sectional", conf.level = 0.95, 
         interpret = FALSE, outcome = "as.columns")


#### Change the part "interpret" = TRUE

epi.2by2(dat = cema1.2, method = "cross.sectional", conf.level = 0.95, 
         interpret = TRUE, outcome = "as.columns")

#Any questions?


################################################################################################################################################################################################################################################################################################################################################################################

#Most of the times, we have a dataframe that has one row per observation

# We will use the data from Hosmer and Lemeshow (2000),  collected at Baystate Medical Center, Springfield, Massachusetts USA during 1986, and available in the MASS package in R. The birthwt data frame has 189 rows and 10 columns.


# Load package
library(MASS)

# We are interested in birthwt = Risk Factors Associated with Low Infant Birth Weight
#This data frame contains the following columns:
# 1) low = indicator of birth weight less than 2.5 kg.
# 2) age = mother’s age in years.
# 3) lwt = mother’s weight in pounds at last menstrual period.
# 4)race = mother’s race (1 = white, 2 = black, 3 = other).
# 5) smoke = smoking status during pregnancy.
# 6) ptl = number of previous premature labours.
# 7) ht = history of hypertension.
# 8) ui = presence of uterine irritability.
# 9) ftv = number of physician visits during the first trimester.
# 10) bwt = birth weight in grams.

############################################################################################
#Load the data

cema2 <- birthwt; head(cema2)

#We’re interested in the association between smoke (the mother’s smoking status during pregnancy) and low (delivery of a baby less than 2.5 kg bodyweight).

#Our data needs to be presented to epi.2by2 in the correct format: Outcome positives in the first column, outcome negatives in the second column, exposure positives in the first row and exposure negatives in the second row.

#Check
cema2.1 <- table(cema2$smoke, cema2$low, dnn = c("Smoking", "Low.BW")); cema2.1

#The data is not in the correct format

##Two ways to solve this problem

# A) Switch the order of the rows and columns in the output table

cema2.1 <- cema2.1[2:1,2:1]; cema2.1


# B) As in the first example, set the exposure variable and the outcome variable as a factor and to define the levels of each factor using levels = c()

cema2$low <- factor(cema2$low, levels = c(1,0))
cema2$smoke <- factor(cema2$smoke, levels = c(1,0))

#Check
cema2.1 <- table(cema2$smoke, cema2$low, dnn = c("Smoking", "Low.BW")); cema2.1

## Continue with the calculations as before

epi.2by2(dat = cema2.1, method = "cohort.count", conf.level = 0.95, 
         units = 100, interpret = FALSE, outcome = "as.columns")

############################################################################################

#Measures of association for multiple variables writing the results to a data frame

cema3 <- birthwt; head(cema3)


#We are interested in the odds ratio for age, smoke and race

cema3$flow <- factor(cema3$low, levels = c(1,0))

cema3$fage <- ifelse(cema3$age > 23, 0,1)
cema3$fage <- factor(cema3$fage, levels = c(1,0))
cema3$fsmoke <- factor(cema3$smoke, levels = c(1,0))

####Change reference level for race to white

# Race is coded 1 = white, 2 = black and 3 = other.

cema3$frace <- ifelse(cema3$race == 1, 0, 1)
cema3$frace <- factor(cema3$frace, levels = c(1,0))


# Create empty vectors to collect results:
rfactor <- ref <- or.p <- or.l <- or.u <- c() 

#The candidate risk factors are in columns 12 to 14 of data frame cema3:
for(i in 12:14){
  cema3.1 <- table(cema3[,i], cema3$flow)
  results1 <- epi.2by2(dat = cema3.1, method = "cohort.count", 
                       conf.level = 0.95, units = 100, interpret = FALSE, outcome = "as.columns")
  
  trfactor <- as.character(names(cema3)[i]) 
  rfactor <- c(rfactor, trfactor) 
  
  tref <- as.character(paste("Reference: ", trfactor, " - ", levels(cema3[,i])[2], sep = ""))
  ref <- c(ref, tref)
  
  tor.p <- as.numeric(results1$massoc.detail$OR.strata.wald[1])
  or.p <- c(or.p, tor.p)
  
  tor.l <- as.numeric(results1$massoc.detail$OR.strata.wald[2])
  or.l <- c(or.l, tor.l)
  
  tor.u <- as.numeric(results1$massoc.detail$OR.strata.wald[3])
  or.u <- c(or.u, tor.u)
}

cema3.2 <- data.frame(yat = 1:3, ylab = rfactor, ref, or.p, or.l, or.u)
cema3.2


### Plot these

library(ggplot2); library(scales)

x.at <- c(0.25,0.5,1,2,4,8,16,32)

ggplot(data = cema3.2, aes(x = or.p, y = yat)) +
  theme_bw() +
  geom_point() + 
  geom_errorbarh(aes(xmax = or.l, xmin = or.u, height = 0.2)) + 
  scale_x_continuous(trans = log2_trans(), breaks = x.at, limits = c(0.25,8), 
                     name = "Odds ratio") + 
  scale_y_continuous(breaks = cema3.2$yat, labels = cema3.2$ylab, 
                     name = "Risk factor") + 
  geom_vline(xintercept = 1, lwd = 1) + 
  annotate("text", x = 0.25, y = cema3.2$yat, label = cema3.2$ref, hjust = 0, size = 3) +
  coord_fixed(ratio = 0.75 / 1) + 
  theme(axis.title.y = element_text(vjust = 0))

############################################################################################
