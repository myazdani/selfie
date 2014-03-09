#modeling selfies 
#multinomial logistic regression on cities
library(nnet)
setwd("~/Documents/projects/selfie/scripts/")

selfies.rek = read.csv("../meta/selfies_rekognized_full.csv", header = TRUE, stringsAsFactors = FALSE)
selfies.rek$combined_gender_guess = as.factor(selfies.rek$combined_gender_guess)
levels(selfies.rek$combined_gender_guess) = c("Female", "Male")
selfies.rek$city = as.factor(selfies.rek$city)
#gender.logit = glm(combined_gender_guess ~ log(1+abs(tan(pose_yaw))) + log(1+abs(tan(pose_roll))) + log(1+abs(tan(pose_pitch))) + 
#                     log(sqrt(boundingbox_tl_y**2 + boundingbox_tl_x**2)) + 
#                     atan2(boundingbox_tl_y,boundingbox_tl_x) + log(1+boundingbox_size_height), data = selfies.rek, family = "binomial")

#ml$prog2 <- relevel(ml$prog, ref = "academic")
#city.softmax <- multinom(prog2 ~ ses + write, data = selfies.rek)
cities.to.compare = levels(selfies.rek$city)

#for (i in c(1:length(cities.to.compare))){
for (i in 1){
  print(paste("Results for", cities.to.compare[i]))
  selfies.rek$city = relevel(selfies.rek$city, ref = cities.to.compare[i])
  
  #city.softmax = multinom(city ~ log(sqrt(boundingbox_tl_y**2 + boundingbox_tl_x**2)) + 
   #                         atan2(boundingbox_tl_y,boundingbox_tl_x) + log(1+boundingbox_size_height), data = selfies.rek, trace = FALSE)

  city.softmax = multinom(city ~ log(boundingbox_size_height)  + combined_gender_guess, data = selfies.rek, trace = FALSE)
  
  
  print(exp(coef(city.softmax)))
  
}

dses = data.frame(combined_gender_guess = c("Female", "Male"), boundingbox_size_height = mean(log(1+selfies.rek$boundingbox_size_height)))
predict(city.softmax, newdata=dses, "probs")

dbox = data.frame(combined_gender_guess = rep(c("Female", "Male"), each = 137-23+1), boundingbox_size_height = rep(log(1+c(23:137)), 2))
pp.box = cbind(dbox, predict(city.softmax, newdata = dbox, type = "probs", se = TRUE))

by(pp.box[,c(3:7)], pp.box$combined_gender_guess, colMeans)

lpp = melt(pp.box, id.vars = c("combined_gender_guess", "boundingbox_size_height"), value.name = "probability")

ggplot(lpp, aes(x = exp(boundingbox_size_height), y = probability, colour = combined_gender_guess)) + geom_line() + facet_grid(variable ~ ., scales = "free") + xlab("Boundingbox size height")
