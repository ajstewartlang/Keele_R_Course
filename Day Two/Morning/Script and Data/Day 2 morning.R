library (lme4) #needed for linear mixed models
library (lmerTest) #gives us p-values for parameters in linear mixed models 
library (emmeans) #allows us to run pairwise comparisons/contrsasts
library (readr) #needed to read in Excel and csv data
library (ggplot2) #needed for ggplots
library (ordinal) #needed for CLMM - cumulative link mixed models for ordinal data
library (yarrr) #pirateplots
library (plyr) #for some data wrangling

#Bodo Winter examples - linear model
genderheightdata <- read_delim("genderheightdata.txt", 
                               "\t", escape_double = FALSE, trim_ws = TRUE)

ourmodel <- lm(Height ~ Gender, genderheightdata)
summary (ourmodel)

#plots
scatter <- ggplot (genderheightdata, aes (group=1, Gender, Height)) + geom_point()
scatter + geom_smooth (method="lm")

ageheightdata <- read_delim("ageheightdata.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)

ourmodel <- lm (Height ~ Age, ageheightdata)
summary (ourmodel)

#plots
scatter <- ggplot (ageheightdata, aes (group=1, Age, Height)) + geom_point()
scatter + geom_smooth (method="lm")

politeness_data <- read_csv("politeness_data.csv")

politeness.model <- lmer (frequency~attitude + (1|subject) + (1|scenario), data=politeness_data)
summary (politeness.model)

politeness.model <- lmer (frequency~attitude + gender + (1|subject) + (1|scenario), data=politeness_data)
summary (politeness.model)

politeness.null <- lmer (frequency~ gender + (1|subject) + (1|scenario), data=politeness_data)

#comparing experimental model and null model
anova (politeness.model, politeness.null)

politeness.gender <- lmer (frequency~attitude + gender + (1|subject) + (1|scenario), data=politeness_data)
politeness.nogender <- lmer (frequency~attitude + (1|subject) + (1|scenario), data=politeness_data)
anova (politeness.gender, politeness.nogender)

#plots to visually examine model fit
plot (fitted (politeness.nogender), residuals (politeness.nogender), xlab="Fitted Values", ylab="Residuals", main="Without Gender",
              col="red")
abline (h=0, lty=2)
lines (smooth.spline(fitted(politeness.nogender), residuals (politeness.nogender)))

plot (fitted (politeness.gender), residuals (politeness.gender), xlab="Fitted Values", ylab="Residuals", main="With Gender",
      col="red")
abline (h=0, lty=2)
lines (smooth.spline(fitted(politeness.gender), residuals (politeness.gender)))

#generate the coefficients for our random effects
coef (politeness.model)

politeness.model <- lmer (frequency~attitude + gender + (1+attitude|subject) + (1+attitude|scenario), data=politeness_data, REML=FALSE)
coef (politeness.model)
summary (politeness.model)

politeness.null <- lmer (frequency~gender + (1+attitude|subject) + (1+attitude|scenario), data=politeness_data, REML=FALSE)
anova (politeness.model, politeness.null)

#Linear mixed model (LMM) for 1-factor (repeated measures) with three levels
DV <- read_csv("DV_1factor.csv", 
                       col_types = cols(Condition = col_factor(levels = c("Neutral", 
                                                                          "Positive", "Negative"))))
DV$Condition <- relevel (DV$Condition, ref=3)

model.null <- lmer (Gaze ~ (1+Condition|Subject) + (1+Condition|Item), data=DV, REML=TRUE)
model.full <- lmer (Gaze ~ Condition + (1+Condition|Subject) + (1+Condition|Item), data=DV, REML=TRUE)
anova (model.null, model.full)
summary (model.full)

#GLMM for binomial data - 1-factor with(repeated measuers) with 3 levels 
RO <- read_csv("RO.csv", col_types = cols(Condition = col_factor(levels = c("Neutral", 
                                                                            "Positive", "Negative"))))

model.full <- glmer (DV ~ Condition + (1+Condition|Subject) + (1+Condition|Item), data=RO, family=binomial)

model.interceptonly <- glmer (DV ~ Condition + (1|Subject) + (1|Item) , data=RO, family=binomial)
model.null <- glmer (DV ~  (1|Subject) + (1|Item), data=RO, family=binomial)
anova (model.interceptonly, model.null)

#LMM for 2x2 repeated measures factorial design
DV <- read_csv("DV.csv")

DV$Sentence <- as.factor (DV$Sentence)
DV$Context <- as.factor (DV$Context)

contrasts (DV$Sentence) <- matrix (c(.5, -.5))
contrasts (DV$Context) <- matrix (c(.5,-.5))

model.full <- lmer (RT~Context*Sentence + (1+Context*Sentence|Subject) + (1+Context*Sentence|Item), data=DV, REML=TRUE)
model.null <- lmer (RT~(1+Context*Sentence|Subject) + (1+Context*Sentence|Item), data=DV, REML=TRUE)
anova (model.full, model.null)
summary (model.full)

emmeans (model.full, pairwise~Context*Sentence, adjust="none")

#CLMM for ordinal data
Main <- read_csv("Main.csv")

Main$Subject <- as.factor (Main$Subject)
Main$Image <- as.factor (Main$Image)
Main$SportType <- as.factor (Main$SportType)
Main$VideoCondition <- as.factor (Main$VideoCondition)

#VideoCondition 2=congruent, 3=Incongruent, 4=neutral
Main$VideoCondition <- revalue (Main$VideoCondition, c("2"="Congruent", "3"="Incongruent", "4"="Neutral"))

pirateplot(formula = ratings ~ VideoCondition, data=Main, avg.line.fun = mean, xlab = "Condition", ylab = "Rating", main = "Rating Scores by Condition", jitter.val = 0.09, inf.method = "se", theme=1)

Main$ratings <- as.ordered (Main$ratings)
model.clm.null <- clmm (ratings ~ 1 + (1 + VideoCondition|Subject) + (1 + VideoCondition|SportType) + (1 + VideoCondition|Image), data=Main)
model.clm4 <- clmm (ratings ~ VideoCondition + (1 + VideoCondition|Subject) + (1 + VideoCondition|SportType) + (1 + VideoCondition|Image), data=Main)
anova (model.clm.null, model.clm4)

emmeans (model.clm4, pairwise ~ VideoCondition, adjust="none")

#Build and examine normality of residuals with data untransformed
model.full <- lmer (RT~Context*Sentence + (1+Context*Sentence|Subject) + (1+Context*Sentence|Item), data=DV, REML=TRUE)
qqnorm (residuals(model.full))
summary (model.full)

#Build and examine normality of residuals with data log transformed 
model.full <- lmer (log(RT)~Context*Sentence + (1+Context*Sentence|Subject) + (1+Context*Sentence|Item), data=DV, REML=TRUE)
qqnorm (residuals(model.full))
summary (model.full)

#Build and examine normality of residuals with GLMM under the Gamma distribution - 
#simplified random effects structure needed to converge
model.full <- glmer (RT~Context*Sentence + (1+Context|Subject) + (1+Context|Item), data=DV, family=Gamma)
qqnorm (residuals(model.full))
summary (model.full)

