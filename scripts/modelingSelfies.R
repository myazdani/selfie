#modeling selfies 
#logistic regression on gender
setwd("~/Documents/projects/selfie/scripts/")

selfies.rek = read.csv("../meta/selfies_rekognized_full.csv", header = TRUE, stringsAsFactors = FALSE)
selfies.rek$combined_gender_guess = as.factor(selfies.rek$combined_gender_guess)
levels(selfies.rek$combined_gender_guess) = c("Female", "Male")
levels(selfies.rek$combined_gender_guess) = c(1, 0)
selfies.rek$city = as.factor(selfies.rek$city)
gender.logit = glm(combined_gender_guess ~ log(1+abs(tan(pose_yaw))) + log(1+abs(tan(pose_roll))) + log(1+abs(tan(pose_pitch))) + 
                     log(sqrt(boundingbox_tl_y**2 + boundingbox_tl_x**2)) + 
                     atan2(boundingbox_tl_y,boundingbox_tl_x) + log(1+boundingbox_size_height), data = selfies.rek, family = "binomial")
confint(gender.logit)

#testing if model is better than an empty model (gives p value)
with(gender.logit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

#odds ratio
exp(coef(gender.logit))
#odds ratio and 95% confidence intervel
exp(cbind(OR = coef(gender.logit), confint(gender.logit)))

selfies.raw.tags = read.csv("../meta/tags.csv", header = TRUE, stringsAsFactors = FALSE)

ids = unique(selfies.raw.tags$id)

selfies.raw.tags.DT = data.table(selfies.raw.tags)
setkey(selfies.raw.tags.DT, id)
median.age = sapply(ids, FUN = function(x) median(selfies.raw.tags.DT[x]$age))
mean.age = sapply(ids, FUN = function(x) mean(selfies.raw.tags.DT[x]$age))
sd.age = sapply(ids, FUN = function(x) sd(selfies.raw.tags.DT[x]$age))
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

gender.agreement.level = function(x) {
  ux <- unique(x)
  length(which(x == ux[which.max(tabulate(match(x, ux)))] ))/ length(x) 
}

mode.gender = sapply(ids, FUN = function(x) Mode(selfies.raw.tags.DT[x]$gender))
gender.conf = sapply(ids, FUN = function(x) gender.agreement.level(selfies.raw.tags.DT[x]$gender))

cities = sapply(ids, FUN = function(x) selfies.raw.tags.DT[x]$city[1])
selfies.tags = data.frame(id = ids, median.age = median.age, mean.age = mean.age, sd.age= sd.age, mode.gender = mode.gender, gender.conf = gender.conf, city = cities)
levels(selfies.tags$city) = c("Bangkok", "Berlin", "Moscow", "New York", "Sao Paulo", "Tokyo")
levels(selfies.tags$mode.gender) = c("Female", "Male")


gender.mt = glm(mode.gender ~ city + log(median.age) , data = selfies.tags, family = "binomial") 

#testing if model is better than an empty model (gives p value)
with(gender.mt, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
exp(cbind(OR = coef(gender.mt), confint(gender.mt)))
