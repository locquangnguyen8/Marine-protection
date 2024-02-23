# Load data and package
data1<-read.csv("D:/Dropbox/Research/@_Team/@_AISDL_Fandom/BMF collaborative projects/2023/Datasets/Marine and coastal ecosystems and climate change/Survey_Fonsecaetal_07122022.csv",header = TRUE,stringsAsFactors = TRUE)
library(bayesvl)
library(cowplot)
library(ggplot2)

data1$SupportforOcean <- data1$Q12_6

data1$Benefits_ClimatechangeReduction <- data1$Q8_6
data1$KnowledgeTowardClimateChange <- data1$Q3
data1$EmotionTowardClimateChange <- data1$Q4



keeps <- c("SupportforOcean","Benefits_ClimatechangeReduction","KnowledgeTowardClimateChange","EmotionTowardClimateChange")
data1 <- data1[keeps]
data1<-na.omit(data1)



# Model construction: Model 1
model1<-bayesvl()
model1<-bvl_addNode(model1,"SupportforOcean","norm")
model1<-bvl_addNode(model1,"Benefits_ClimatechangeReduction","norm")
model1<-bvl_addNode(model1,"KnowledgeTowardClimateChange","norm")

model1<-bvl_addNode(model1,"Benefits_Knowledge","trans")


model1<-bvl_addArc(model1,"Benefits_ClimatechangeReduction","Benefits_Knowledge","*")
model1<-bvl_addArc(model1,"KnowledgeTowardClimateChange","Benefits_Knowledge","*")

model1<-bvl_addArc(model1,"Benefits_ClimatechangeReduction","SupportforOcean","slope")
model1<-bvl_addArc(model1,"KnowledgeTowardClimateChange","SupportforOcean","slope")
model1<-bvl_addArc(model1,"Benefits_Knowledge","SupportforOcean","slope")

bvl_bnPlot(model1)

# Generate Stan code
model_string1<- bvl_model2Stan(model1)
cat(model_string1) 

# Model Fit
model1<-bvl_modelFit(model1, data1, warmup = 2000, iter = 5000, chains = 4,cores = 4)
summary(model1)

bvl_plotTrace(model1)
bvl_plotGelmans(model1,row = 2, col = 2)
bvl_plotAcfs(model1,row = 2,col = 2)


loo1<-bvl_stanLoo(model1)
plot(loo1)

# Model construction: Model 2
model2<-bayesvl()
model2<-bvl_addNode(model2,"SupportforOcean","norm")
model2<-bvl_addNode(model2,"Benefits_ClimatechangeReduction","norm")
model2<-bvl_addNode(model2,"EmotionTowardClimateChange","norm")

model2<-bvl_addNode(model2,"Benefits_Emotion","trans")


model2<-bvl_addArc(model2,"Benefits_ClimatechangeReduction","Benefits_Emotion","*")
model2<-bvl_addArc(model2,"EmotionTowardClimateChange","Benefits_Emotion","*")

model2<-bvl_addArc(model2,"Benefits_ClimatechangeReduction","SupportforOcean","slope")
model2<-bvl_addArc(model2,"EmotionTowardClimateChange","SupportforOcean","slope")
model2<-bvl_addArc(model2,"Benefits_Emotion","SupportforOcean","slope")

bvl_bnPlot(model2)

# Generate Stan code
model_string2<- bvl_model2Stan(model2)
cat(model_string2) 

# Model Fit
model2<-bvl_modelFit(model2, data1, warmup = 2000, iter = 5000, chains = 4,cores = 4)
summary(model2)

bvl_plotTrace(model2)
bvl_plotGelmans(model2,row = 2, col = 2)
bvl_plotAcfs(model2,row = 2,col = 2)


loo2<-bvl_stanLoo(model2)
plot(loo2)

# Model construction: Model 2
model3<-bayesvl()
model3<-bvl_addNode(model3,"SupportforOcean","norm")
model3<-bvl_addNode(model3,"Benefits_ClimatechangeReduction","norm")
model3<-bvl_addNode(model3,"KnowledgeTowardClimateChange","norm")
model3<-bvl_addNode(model3,"EmotionTowardClimateChange","norm")

model3<-bvl_addNode(model3,"Benefits_Emotion","trans")


model3<-bvl_addArc(model3,"Benefits_ClimatechangeReduction","Benefits_Emotion","*")
model3<-bvl_addArc(model3,"EmotionTowardClimateChange","Benefits_Emotion","*")

model3<-bvl_addNode(model3,"Benefits_Knowledge","trans")


model3<-bvl_addArc(model3,"Benefits_ClimatechangeReduction","Benefits_Knowledge","*")
model3<-bvl_addArc(model3,"KnowledgeTowardClimateChange","Benefits_Knowledge","*")


model3<-bvl_addArc(model3,"Benefits_ClimatechangeReduction","SupportforOcean","slope")
model3<-bvl_addArc(model3,"EmotionTowardClimateChange","SupportforOcean","slope")
model3<-bvl_addArc(model3,"Benefits_Emotion","SupportforOcean","slope")
model3<-bvl_addArc(model3,"KnowledgeTowardClimateChange","SupportforOcean","slope")
model3<-bvl_addArc(model3,"Benefits_Knowledge","SupportforOcean","slope")

bvl_bnPlot(model3)

# Generate Stan code
model_string2<- bvl_model2Stan(model3)
cat(model_string2) 

# Model Fit
model3<-bvl_modelFit(model3, data1, warmup = 2000, iter = 5000, chains = 4,cores = 4)
summary(model3)

bvl_plotTrace(model3)
bvl_plotGelmans(model3,row = 2, col = 3)
bvl_plotAcfs(model3,row = 2,col = 3)


loo3<-bvl_stanLoo(model3)
plot(loo3)

# Model construction: Model 4
model4<-bayesvl()
model4<-bvl_addNode(model4,"SupportforOcean","norm")
model4<-bvl_addNode(model4,"Benefits_ClimatechangeReduction","norm")
model4<-bvl_addNode(model4,"KnowledgeTowardClimateChange","norm")
model4<-bvl_addNode(model4,"EmotionTowardClimateChange","norm")

model4<-bvl_addNode(model4,"Benefits_Emotion","trans")


model4<-bvl_addArc(model4,"Benefits_ClimatechangeReduction","Benefits_Emotion","*")
model4<-bvl_addArc(model4,"EmotionTowardClimateChange","Benefits_Emotion","*")

model4<-bvl_addNode(model4,"Benefits_Knowledge","trans")


model4<-bvl_addArc(model4,"Benefits_ClimatechangeReduction","Benefits_Knowledge","*")
model4<-bvl_addArc(model4,"KnowledgeTowardClimateChange","Benefits_Knowledge","*")

model4<-bvl_addNode(model4,"Benefits_Knowledge_Emotion","trans")


model4<-bvl_addArc(model4,"Benefits_Knowledge","Benefits_Knowledge_Emotion","*")
model4<-bvl_addArc(model4,"EmotionTowardClimateChange","Benefits_Knowledge_Emotion","*")

model4<-bvl_addArc(model4,"Benefits_ClimatechangeReduction","SupportforOcean","slope")
model4<-bvl_addArc(model4,"EmotionTowardClimateChange","SupportforOcean","slope")
model4<-bvl_addArc(model4,"Benefits_Emotion","SupportforOcean","slope")
model4<-bvl_addArc(model4,"KnowledgeTowardClimateChange","SupportforOcean","slope")
model4<-bvl_addArc(model4,"Benefits_Knowledge","SupportforOcean","slope")
model4<-bvl_addArc(model4,"Benefits_Knowledge_Emotion","SupportforOcean","slope")

bvl_bnPlot(model4)

# Generate Stan code
model_string2<- bvl_model2Stan(model4)
cat(model_string2) 

# Model Fit
model4<-bvl_modelFit(model4, data1, warmup = 2000, iter = 5000, chains = 4,cores = 4)
summary(model4)

loo4<-bvl_stanLoo(model4)
plot(loo4)

bvl_plotTrace(model4)
bvl_plotGelmans(model4,row = 3, col = 3)
bvl_plotAcfs(model4,row = 3,col = 3)

bvl_plotIntervals(model4,c("b_Benefits_ClimatechangeReduction_SupportforOcean","b_EmotionTowardClimateChange_SupportforOcean","b_Benefits_Emotion_SupportforOcean",
                           "b_KnowledgeTowardClimateChange_SupportforOcean","b_Benefits_Knowledge_SupportforOcean","b_Benefits_Knowledge_Emotion_SupportforOcean"))+theme_bw()

bvl_plotDensity(model4,c("b_Benefits_ClimatechangeReduction_SupportforOcean","b_EmotionTowardClimateChange_SupportforOcean","b_Benefits_Emotion_SupportforOcean",
                           "b_KnowledgeTowardClimateChange_SupportforOcean","b_Benefits_Knowledge_SupportforOcean","b_Benefits_Knowledge_Emotion_SupportforOcean"))+theme_bw()

bvl_plotParams(model4,row = 3,col = 3,credMass = 0.89)

# Weight comparison
library("loo")

log_lik_1 <- extract_log_lik(model1@stanfit, parameter_name = "log_lik_SupportforOcean", merge_chains = FALSE)
r_eff1 <- relative_eff(exp(log_lik_1))
loo_1 <- loo(log_lik_1, r_eff = r_eff1, cores = 2)

log_lik_2 <- extract_log_lik(model2@stanfit, parameter_name = "log_lik_SupportforOcean", merge_chains = FALSE)
r_eff2 <- relative_eff(exp(log_lik_2))
loo_2 <- loo(log_lik_2, r_eff = r_eff2, cores = 2)

log_lik_3 <- extract_log_lik(model3@stanfit, parameter_name = "log_lik_SupportforOcean", merge_chains = FALSE)
r_eff3 <- relative_eff(exp(log_lik_3))
loo_3 <- loo(log_lik_3, r_eff = r_eff3, cores = 2)


log_lik_4 <- extract_log_lik(model4@stanfit, parameter_name = "log_lik_SupportforOcean", merge_chains = FALSE)
r_eff4 <- relative_eff(exp(log_lik_4))
loo_4 <- loo(log_lik_4, r_eff = r_eff4, cores = 2)

loo_list <- list(model1=loo_1, model2=loo_2, model3=loo_3, model4=loo_4)

stacking_wts <-loo_model_weights(loo_list) # stacking weight
pbma_BB_wts<-loo_model_weights(loo_list, method = "pseudobma") # pseudo-BMA+ weight
pbma_wts <-loo_model_weights(loo_list, method = "pseudobma", BB = FALSE) # pseudo-BMA weight

(waic1 <- waic(log_lik_1))
(waic2 <- waic(log_lik_2))
(waic3 <- waic(log_lik_3))
(waic4 <- waic(log_lik_4))


waics <- c(
  waic1$estimates["elpd_waic", 1],
  waic2$estimates["elpd_waic", 1],
  waic3$estimates["elpd_waic", 1],
  waic4$estimates["elpd_waic", 1]
)

waic_wts <- exp(waics) / sum(exp(waics))  # WAIC weight

round(cbind(waic_wts, pbma_wts, pbma_BB_wts, stacking_wts),3 )