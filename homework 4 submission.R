## Homework 4 Submission ## 
mistletoe <- read.csv("mistletoes.csv")
head(mistletoe)
library(performance)
library(marginaleffects)
library(MASS)
library(pROC)
## Question 1:

## 1a) Seedling density is increased beneath trees experiencing mistletoe infection.
mod<-glm.nb(Seedlings~Treatment, data = mistletoe)
summary(mod)

mae(mod, predictions(mod), na.rm = TRUE) # 145.841

#I selected a negative binomial distribution because the predictor is either parasitized or not indicating only 2 possibilities. Negative binomial was chosen over the binomial distribution because the data is count data that is overdispersed according to the histogram below. We have a couple seedling observations that are over 2,000 but the majority are under 500. 
hist(mistletoe$Seedlings)

## 1b)
plot_predictions(mod, condition = "Treatment")
predicitons(mod, newdata = data.frame(Treatment - c("parasitized", "unparasitized"))

#According to our model seedling recruitment was higher under trees that had been parasitized by mistletoe with an expected density of 308 seedlings as opposed to an expected 13 seedlings under unparasitized trees.There was a statistically significant value (p<0.05) between our groups of parasitized and unparasitized trees. The unparasitized group also had a negative estimate value indicating the relationship we see in the "plot_predictions" plot. 

#Intercept tells me the number of seedlings on the log odds scale. A negative significant effect.
## 1c) 
mod2<-glm.nb(Seedlings~Treatment*as.factor(Year), data = mistletoe)
summary(mod2)

predictions(mod2, newdata = data.frame(Year = c("2011","2012"),
                                       Treatment =c("parasitized", "unparasitized")))

plot_predictions(mod2, condition = c("Year", "Treatment"))

#Intercept is slope - Estimate: The change associated with year

# There was a nearly significant effect of year on the amount of seedlings beneath parasitized trees and unparasitized trees. This variation does not seem to have an impact on our model but since it is nearly significant could prompt us to include more variables to better explain the variation that we do see. 

## Question 2:
treemortality <- read.csv("treemortality.csv")
head(treemortality)
## 2a)
mod3<-glm(mortality~thinning, family = binomial(link = "logit"), data = treemortality)
summary(mod3)

plogis(0.9933)
plogis(0.9933-1.8559)
#With thinning treatments we saw a decrease in tree mortality from 72% without thinning to 29% within thinning. This lends support to the hypothesis that thinning did decrease tree mortality. Model fit was decent according to our AUC metric of 0.710. 

preds<-predict(mod3, type = "response")
roc <- roc(treemortality$mortality
                 ~ preds, plot = TRUE,
                 print.auc = TRUE)
#The model is doing better than random chance. 

## 2b)
#No they shouldn't need to incorporate tree size because of their additional sampling that randomized the likelihood of sampling small and large stems should remove the potential confounding effect that tree size had.  

## 2c) 

mod4<-glm(mortality~thinning+roaddist+slope, family = binomial(link = "logit"), data = treemortality)
summary(mod4)

predictions(mod4, newdata = data.frame(roaddist = mean(treemortality$roaddist), 
                                       slope = mean(treemortality$slope),
                                       thinning = c(0,1)))



plot_predictions(mod4, condition = c("roaddist","thinning"))
plot_predictions(mod4, condition = c("slope","thinning"))


#When holding the distance to road and slope at their averages tree mortality decreased from 52% in unthinned patches to 30% in thinned patches. This decrease was only a 22% change where our model with only thinning showed a decrease of approximately 50% which. Our results indicate that we overestimated the impact of thinning on tree mortality and that distance to road and slope are influencing the effect of the thinning treatments (and consequently tree mortality), so they should be included in our model. 
