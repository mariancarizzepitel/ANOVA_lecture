library(tidyverse)

mdata <- read_csv("drugData.csv")
mdata$Group <- as.factor(mdata$Group) # for t-tests and ANOVA, do not forget to turn into factor

# DIFFERENT WAYS TO GIVE GROUP-BASED DESCRIPTIVES -------------------------------------------------------------------
# Approach 1 : psych 
psych::describeBy(x=mdata$Arousal, group=mdata$Group)
## descriptive statistics... x = "take this column of Arousal", and group them by "Group" (instead, could be gender)

# Approach 2 : tidyverse
mdata_grouped <- group_by(mdata, Group)
mdata_grouped %>% summarise(M=mean(Arousal,na.rm=TRUE), 
                            SD=sd(Arousal,na.rm=TRUE)) # M=mean(__) means create new column named "M" with mean, same with SD = sd 
#--------------------------------------------------------------------------------------------------------------------


# RUN T-TEST --------------------------------------------------------------------------------------------------------
# check assumption: homogeneity of variance 
car::leveneTest(mdata$Arousal, group=mdata$Group, center="median") #Result: non-signficant, thus equal variance 

# subset data
exp.group.rows <- mdata %>% filter(Group==0) # give me subset of mdata only for group 0s
control.group.rows <- mdata %>% filter(Group==1) # give me subset of data only for group 1s 

# run t-test 
t.test(x=exp.group.rows$Arousal, 
       y=control.group.rows$Arousal, 
       var.equal=TRUE) # var.equal=FALSE IF variance not equal bw groups
#-------------------------------------------------------------------------------------------------------------------


# CALCULATING D-VALUE-----------------------------------------------------------------------------------------------
library(MBESS)

# Calculating d-value when you have raw data 
smd(Group.1=exp.group.rows$Arousal, Group.2=control.group.rows$Arousal) # Result: d value of .62 

# Calculating d-value with descriptive stats (checking articles)
smd(Mean.1=3.2,s.1=.8,Mean.2=2.45,s.2=.91,n.1=10,n.2=10) # Result: d value of .88
ci.smd(smd=0.8753837,n.1=10,n.2=10) # confidence intervals upper and lower limits (-.06,1.8)
#-------------------------------------------------------------------------------------------------------------------


# ONE-WAY ANOVA-----------------------------------------------------------------------------------------------------
library(apaTables)

#  turn variable into factor 
analyticdata.viagra <- read_csv("Viagra.csv")
analyticdata.viagra$dose <- as.factor(analyticdata.viagra$dose) # necessary in ANOVA 
levels(analyticdata.viagra$dose) <- list("Placebo"=1, 
                                         "Low Dose"=2, 
                                         "High Dose"=3)

#  conduct Levene's test 
car::leveneTest(analyticdata.viagra$libido,
                group=analyticdata.viagra$dose, center="median") # in this case, we can assume homogeneity of variance

options(contrasts = c("contr.sum", "contr.poly")) # necessary to get numbers identical to SPSS analysis 

#  run one-way ANOVA
oneway.results <- lm(libido~dose,data=analyticdata.viagra) #linear model (get the line of best fit with slope + intercept)
car::Anova(oneway.results,type=3) # actual ANOVA function 
apa.aov.table(oneway.results) # tradition to use 90% CI in this case 

# -------------------------------------------------------------------------------------------------------------------

# CRF ANOVA - TWO-WAY ANOVA ---------------------------------------------------------------------------------------------------------------
library(phia)

# load data
analyticdata.CRF <- read_csv("crfData.csv")

# using factors
analyticdata.CRF$anxiety <- as.factor(analyticdata.CRF$anxiety)
analyticdata.CRF$preparation <- as.factor(analyticdata.CRF$preparation)
levels(analyticdata.CRF$anxiety) <- list("Low Anxiety"=1,
                                         "High Anxiety"=2)
levels(analyticdata.CRF$preparation) <- list("Low Preparation"=1,
                                             "Medium Preparation"=2,
                                             "High Preparation"=3)
# setting contrasts
options(contrasts = c("contr.sum", "contr.poly"))

# run the analysis
crf.lm <- lm(mark ~ anxiety * preparation, data=analyticdata.CRF)

# get table 
apa.aov.table(crf.lm, filename="Table1.doc", table.number=1) # shows us that there is an interaction, next: look at simple main effects

# get the marginal means 
apa.2way.table(iv1=preparation, iv2=anxiety, dv=mark, data=analyticdata.CRF,
               show.marginal.means = TRUE, filename="Table2.doc")

# examine simple main effects 
testInteractions(crf.lm, fixed="anxiety", 
                 across="preparation", 
                 adjustment="none") # hold anxiety constant and see the effect of preparation
## Results: "Simple main effect for low anxiety was significant and simple main effect for high anxiety was non-significant" 
## Interpretation: No main effect of preparation for high anxiety. Simple main effect of preparation for low anxiety was significant." 
## Next step: Paired comparisons... 

# CI for partial eta squared
get.ci.partial.eta.squared(F.value=7.1411, df1=2, df2=24, conf.level=.90) # F value taken from first simple main effect table row (low anxiety) 
get.ci.partial.eta.squared(F.value=0.9733, df1=2, df2=24, conf.level=.90) # F value..... second row (high anxiety)

# paired comparisons 
testInteractions(crf.lm, fixed="anxiety",
                 pairwise="preparation",
                 adjustment="bonferroni")
