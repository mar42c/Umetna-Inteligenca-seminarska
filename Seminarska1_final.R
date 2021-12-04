#########################################################
#  Seminarska naloga 1 pri predmetu Umetna Inteligenca  #
#########################################################


########################################
#   Dodajanje ucne in testne mnozice   #
########################################
trainDt <-  read.table(file="ucnaSem1.txt", sep=",", header=T, stringsAsFactors = T)

# Popravimo datum
trainDt$datum <- as.Date(trainDt$datum, "%Y-%m-%d")

summary(trainDt)

# Dodajanje atributa vikend
dan <- weekdays(trainDt$datum)

sobota <- dan == "Saturday"
nedelja <- dan == "Sunday"
vikend <- sobota|nedelja
vikend

trainDt$vikend <- vikend

trainDt$vikend <- as.factor(trainDt$vikend)

# Dodajanje atributa letni cas

getSeason <- function(DATES) {
  WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Zimski solsticij
  SE <- as.Date("2012-3-21",  format = "%Y-%m-%d") # Spomladansko enakonocje
  SS <- as.Date("2012-6-21",  format = "%Y-%m-%d") # Poletni solsticij
  FE <- as.Date("2012-9-23",  format = "%Y-%m-%d") # Jesensko enakonocje
  
  # Konverzija vseh datumov v leto 2012 (lahko bi bilo katerokoli leto)
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Zima",
          ifelse (d >= SE & d < SS, "Pomlad",
                  ifelse (d >= SS & d < FE, "Poletje", "Jesen")))
}

letni_cas <- getSeason(trainDt$datum)
trainDt$letni_cas <- letni_cas
# Letni cas je char zato ga se faktoriziramo
trainDt$letni_cas <- as.factor(trainDt$letni_cas)

# Dodajanje atributa smeri vetra glede na smer neba
# Motivacija: Smer neba je trenutno podana v stopinjah, ampak nam poda smer neba veliko vec
# informacije (1 in 359 sta zelo razlicni vrednosti ampak gre za veter iz skoraj iste smeri)
# Smer vetra bomo kategorizirali v razrede:S, SV, V, JV, J, JZ, Z, SZ

getWindDirection <- function(d) {
  meje <- c(22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5) #Priprava mej
  
  ifelse (d >= meje[8] | d < meje[1], "S",
    ifelse (d >= meje[1] & d < meje[2], "SV",
        ifelse (d >= meje[2] & d < meje[3], "V",
            ifelse (d >= meje[3] & d < meje[4], "JV",
                 ifelse (d >= meje[4] & d < meje[5], "J",
                     ifelse (d >= meje[5] & d < meje[6], "JZ",
                         ifelse (d >= meje[6] & d < meje[7], "Z", "SZ")))))))
}

smer_vetra1 <- getWindDirection(trainDt$smer_vetra)
smer_vetra1 <- as.factor(smer_vetra1)

trainDt$smer_vetra1 <- smer_vetra1

# Spreminjanje atributa padavine
# Motivacija: vrednost -1 pomeni prsenje, vse ostale vrednosti pa mm dezja, 
# kar je zoperno, saj je po pomenu prsenje blizje malo padavinam po absolutni 
# vrednosti atributa pa je blizje suhemu vremenu.
# Resitev: kategorizacija atributa 

kategorizirajPadavine <- function(p) {
  meje <- c(0.5, 4, 8) #Priprava mej
  
  ifelse (p == 0, "Brez padavin",
    ifelse (p == -1, "Prsenje",
       ifelse (p > 0 & p <= meje[1], "Rahel dez",
           ifelse (p > meje[1] & p <= meje[2], "Zmeren dez",
               ifelse (p > meje[2] & p <= meje[3], "Mocan dez", "Zelo mocan dez")))))
}

padavine_k <- kategorizirajPadavine(trainDt$padavine)
trainDt$padavine_k <- as.factor(padavine_k)
summary(trainDt$padavine_k)

# dodajanje testne mnozice

testDt <-  read.table(file="testnaSem1.txt", sep=",", header=T, stringsAsFactors = T)
#Popravimo datum
testDt$datum <- as.Date(testDt$datum, "%Y-%m-%d")
# Dodajanje atributa vikend
dan1 <- weekdays(testDt$datum)
sobota1 <- dan1 == "Saturday"
nedelja1 <- dan1 == "Sunday"
vikend1 <- sobota1|nedelja1
vikend1
testDt$vikend <- vikend1
typeof(dt$vikend)

#Dodajanje atributa letni cas
letni_cas <- getSeason(testDt$datum)
testDt$letni_cas <- letni_cas
testDt$letni_cas <- as.factor(testDt$letni_cas)

# Dodajanje atributa smer vetra
smer_vetra1 <- getWindDirection(testDt$smer_vetra)
smer_vetra1 <- as.factor(smer_vetra1)

testDt$smer_vetra1 <- smer_vetra1

# Klasificiranje atributa padavine
padavine_k <- kategorizirajPadavine(testDt$padavine)
testDt$padavine_k <- as.factor(padavine_k)
summary(testDt$padavine_k)



##############################################
# -------- Vizualizacija podatkov ---------- #
##############################################

# Vizualizacija namembnosti
# tukaj vizualiziramo koliko *merjenj porabe* smo naredili v stavbah, ne koliko stavb imamo
# za posamezno namembnost
plot(dt$namembnost, xlab="Tip zgradbe", ylab="Frekvenca glede na tip", main="Frekvenca po namembnosti")


# Odstotek izobrazevalnih v primerjavi z drugimi
tab <- table(dt$namembnost == "izobrazevalna")
names(tab) <- c("Ostalo", "Izobrazevalna")
tab

pie(tab, main="Odstotek izobrazevalnih zgradb v primerjavi z drugimi")


# Vizualizacija porabe
hist(trainDt$poraba, xlab="Poraba v kWh", ylab="Frekvenca", main="Histogram porabe elektrike")

# Povprecna poraba
mean(trainDt$poraba)

# Povprecna poraba glede na namembnost
povpPorabaNam <- aggregate(as.double(poraba) ~ namembnost, data=trainDt, mean)
povpPorabaNam
plot(povpPorabaNam, ylab="Povprecna poraba v kWh", main="Povprecna poraba glede na namembnost.")
# Skupno najvec porabijo izobrazevalne zgradbe, vendar povprecno pa najvec porabijo poslovne

#Povprecna poraba po letnem casu
meanPorabaLet <- aggregate(as.double(poraba) ~ letni_cas, data=trainDt, mean)
meanPorabaLet
plot(meanPorabaLet, xlab="Letni cas", ylab="Povprecna poraba v kWh", main="Povprecna poraba glede na letni cas")
#Opazanja: v letnih casih z ekstremnimi temperaturami (poletje, zima) imamo vecjo porabo (hlajenje in segrevanje)

# Povprecna poraba glede na povrsino
meanPorabaPovr <- aggregate(as.double(poraba) ~ povrsina, data=trainDt, mean)
meanPorabaPovr
plot(meanPorabaPovr, xlab="Povrsina", ylab="Povprecna poraba v kWh", main="Povprecna poraba glede na povrsino")

# Visoka korelacija porabe s povrsino???
cor(trainDt$poraba, trainDt$povrsina)
# Oprazanja: stavbe z vecjo povrsino naceloma porabijo povprecno vec energije

# Povprecna poraba glede na vikend ali cez teden

names(summary(trainDt$namembnost)) -> namembnosti

rm(jeVikend, niVikend)
jeVikend <- c()
niVikend <- c()
for (ime in namembnosti){
  v1 <- trainDt$vikend == TRUE & trainDt$namembnost == ime
  v2 <- trainDt$vikend == FALSE & trainDt$namembnost == ime
  
  jeVikend <- c(jeVikend, mean(trainDt[trainDt$poraba[v1], "poraba"]))
  niVikend <- c(niVikend, mean(trainDt[trainDt$poraba[v2], "poraba"]))
}

df1 <- data.frame(namembnosti, jeVikend, niVikend)
summary(df1)

barplot(t(as.matrix(df1[, 2:3])), 
        beside = TRUE,
        names.arg = df1$namembnosti,
        legend.text = T,
        ylim = c(0, 270),
        ylab = "Poraba v KW")

# Ugotovitve: Vidimo, da se poraba v izobrazevalnih ustanovah ter v poslovnih
# zgradbah cez vikend ne spremeni bistveno


# Nadpovprecna ali podpovprecna poraba
nadpovp <- table(trainDt$poraba > mean(trainDt$poraba))
names(nadpovp) <- c("Podpovprecna poraba", "Nadpovprecna poraba")
pie(nadpovp, main="Razmerje")



###############################################
#    Ocenjevanje atributov - klasifikacija    #
###############################################

# Ocenjujemo s pomocjo paketa CORElearn
library(CORElearn)

sort(attrEval(namembnost ~ ., trainDt, "InfGain"), decreasing = TRUE)

odl <- CoreModel(namembnost ~ ., trainDt, model="tree", selectionEstimator="InfGain")
plot(odl, trainDt)

sort(attrEval(namembnost ~ ., trainDt, "Gini"), decreasing = TRUE)

sort(attrEval(namembnost ~ ., trainDt, "GainRatio"), decreasing = TRUE)

sort(attrEval(namembnost ~ ., trainDt, "MDL"), decreasing = TRUE)

odl <- CoreModel(namembnost ~ ., trainDt, model="tree", selectionEstimator="MDL")
plot(odl, trainDt)

sort(attrEval(namembnost ~ ., trainDt, "InfGain", binaryEvaluation=T), decreasing = T)

sort(attrEval(namembnost ~ ., trainDt, "Gini", binaryEvaluation=T), decreasing = T)

source("wrapper.R")
library(rpart)

myTrainFunc <- function(formula, traindata)
{
  rpart(formula, traindata)	
}

myPredictFunc <- function(model, testdata)
{
  predict(model, testdata, type="class")
}

myEvalFunc <- function(predicted, observed, trained)
{
  # vracamo napako modela, saj wrapper minimizira vrednost ocene
  1.0 - mean(observed == predicted)	
}

set.seed(0)
wrapper(namembnost ~ regija+povrsina+leto_izgradnje+temp_zraka+temp_rosisca+oblacnost+padavine_k+pritisk+smer_vetra1+hitrost_vetra+poraba+vikend+letni_cas, trainDt, myTrainFunc, myPredictFunc, myEvalFunc, cvfolds=10)
# best model: estimated error =  0.007502428 selected feature subset =  namembnost ~ povrsina + leto_izgradnje + temp_zraka 
# selected feature subset =  namembnost ~ povrsina + leto_izgradnje + temp_zraka

myPredictFuncProb <- function(model, testdata)
{
  predict(model, testdata, type="prob")
}

myEvalFuncBrier <- function(predicted, observed, trained)
{
  obsMat <- model.matrix(~observed-1)
  sum((obsMat - predicted) ^ 2) / nrow(predicted)	
}

set.seed(0)
wrapper(namembnost ~ regija+povrsina+leto_izgradnje+temp_zraka+temp_rosisca+oblacnost+padavine_k+pritisk+smer_vetra1+hitrost_vetra+poraba+vikend+letni_cas, trainDt, myTrainFunc, myPredictFuncProb, myEvalFuncBrier, cvfolds=10)
# best model: estimated error =  0.01229021 selected feature subset =  namembnost ~ povrsina + leto_izgradnje + temp_zraka 
# selected feature subset =  namembnost ~ povrsina + leto_izgradnje + temp_zraka


#########################################
#  Sestavljanje in ocenjevanje modelov  #
#########################################
CA <- function(observed, predicted)
{
  mean(observed == predicted)
}

# Modeli zgrajeni z atributi, ki jih je podal wrapper z nakljucnim seed-om 
modelDT <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine_k + pritisk + smer_vetra1 + hitrost_vetra + vikend , trainDt, model="tree")
# 0.4922659
modelNB <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine_k + pritisk + smer_vetra1 + hitrost_vetra + vikend , trainDt, model="bayes")
# 0.4330686
modelKNN <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine_k + pritisk + smer_vetra1 + hitrost_vetra + vikend , trainDt, model="knn", kInNN = 5)
# 0.4815635

# Modeli zgrajeni z atributi, ki jih je podal wrapper z seed = 0
modelDT <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka, trainDt, model="tree")
# 0.4922659
modelNB <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka, trainDt, model="bayes")
# 0.4508779
modelKNN <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka, trainDt, model="knn", kInNN = 5)
#0.4320652


predDT <- predict(modelDT, testDt, type = "class")
caDT <- CA(testDt$namembnost, predDT)
caDT

predNB <- predict(modelNB, testDt, type="class")
caNB <- CA(testDt$namembnost, predNB)
caNB

predKNN <- predict(modelKNN, testDt, type="class")
caKNN <- CA(testDt$namembnost, predKNN)
caKNN


dtVzhod <- trainDt[trainDt$regija == "vzhodna",]
dtZahod <- trainDt[trainDt$regija == "zahodna",]

# Vzhodna regija
modelDTVzhod <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka, dtVzhod, model="tree")
# 0.4708612
modelNBVzhod <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka, dtVzhod, model="bayes")
# 0.5094482
modelKNNVzhod <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka, dtVzhod, model="knn", kInNN = 5)
# 0.4947324


predDTVzhod <- predict(modelDTVzhod, testDt, type = "class")
caDTVzhod <- CA(testDt$namembnost, predDTVzhod)
caDTVzhod

predNBVzhod <- predict(modelNBVzhod, testDt, type="class")
caNBVzhod <- CA(testDt$namembnost, predNBVzhod)
caNBVzhod

predKNNVzhod <- predict(modelKNNVzhod, testDt, type="class")
caKNNVzhod <- CA(testDt$namembnost, predKNNVzhod)
caKNNVzhod

# Zahodna regija
modelDTZahod <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka, dtZahod, model="tree")
# 0.290301
modelNBZahod <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka, dtZahod, model="bayes")
# 0.4109532
modelKNNZahod <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka, dtZahod, model="knn", kInNN = 5)
# 0.3460702

predDTZahod <- predict(modelDTZahod, testDt, type = "class")
caDTZahod <- CA(testDt$namembnost, predDTZahod)
caDTZahod

predNBZahod <- predict(modelNBZahod, testDt, type="class")
caNBZahod <- CA(testDt$namembnost, predNBZahod)
caNBZahod

predKNNZahod <- predict(modelKNNZahod, testDt, type="class")
caKNNZahod <- CA(testDt$namembnost, predKNNZahod)
caKNNZahod
#
# Zahodna regija ima bistveno slabše rezultate kot vzhodna
#


###########################################
#    Ocenjevanje atributov - regresija    #
###########################################
mae <- function(obs, pred)
{
  mean(abs(obs - pred))
}

# srednja kvadratna napaka
mse <- function(obs, pred)
{
  mean((obs - pred)^2)
}

# relativna srednja absolutna napaka
rmae <- function(obs, pred, mean.val) 
{  
  sum(abs(obs - pred)) / sum(abs(obs - mean.val))
}

# relativna srednja kvadratna napaka
rmse <- function(obs, pred, mean.val) 
{  
  sum((obs - pred)^2)/sum((obs - mean.val)^2)
}

library(CORElearn)
sort(attrEval(poraba ~ ., trainDt, "MSEofMean"), decreasing = TRUE)
sort(attrEval(poraba ~ ., trainDt, "RReliefFexpRank"), decreasing = TRUE)

source("wrapper.R")
myTrainFuncReg <- function(formula, traindata)
{
  train.kknn(formula, traindata, ks=5)
}

myPredictFuncReg <- function(model, testdata)
{
  predict(model, testdata)
}

myEvalFuncRMSE <- function(predicted, observed, trained)
{
  sum((observed - predicted)^2)/sum((observed - mean(trained))^2)	
}


# tukaj sem dal po vrsti (ta smiselne) kot mi jih je dal RReliefFexpRank
set.seed(0)
wrapper(poraba ~ povrsina + leto_izgradnje + namembnost + regija + padavine_k + letni_cas + vikend + temp_zraka + pritisk + hitrost_vetra + temp_rosisca + oblacnost, trainDt, myTrainFuncReg, myPredictFuncReg, myEvalFuncRMSE, cvfolds=10)
# rezultat wrapperja
# poraba ~ povrsina + temp_zraka + leto_izgradnje + letni_cas + vikend + namembnost + regija


#########################################
#  Sestavljanje in ocenjevanje modelov  #
#########################################

# k najblizji
library(kknn)
modelReduced <- train.kknn(poraba ~ povrsina + temp_zraka + leto_izgradnje + letni_cas + vikend + namembnost + regija, trainDt, ks=5)
predicted <- predict(modelReduced, testDt)
mae(testDt$poraba, predicted)   # 126.7343
mse(testDt$poraba, predicted)   # 86195.75
rmae(testDt$poraba, predicted, mean(dt$poraba)) # 0.7933414
rmse(testDt$poraba, predicted, mean(dt$poraba)) # 1.90684

# vzhodna regija
modelReduced <- train.kknn(poraba ~ povrsina + temp_zraka + leto_izgradnje + letni_cas + vikend + namembnost + regija, dtVzhod, ks=5)
predictedV <- predict(modelReduced, testDt)
mae(testDt$poraba, predictedV)   # 152.3002
mse(testDt$poraba, predictedV)   # 111519
rmae(testDt$poraba, predictedV, mean(dtVzhod$poraba)) # 0.8322077
rmse(testDt$poraba, predictedV, mean(dtVzhod$poraba)) # 2.169097



#svm
library(e1071)
svm.model <- svm(poraba ~ povrsina + temp_zraka + leto_izgradnje + letni_cas + vikend + namembnost + regija, trainDt)
predicted <- predict(svm.model, testDt)
mae(testDt$poraba, predicted) # 112.2171
mse(testDt$poraba, predicted) # 46412.86
rmae(testDt$poraba, predicted, mean(dt$poraba)) # 0.7024651
rmse(testDt$poraba, predicted, mean(dt$poraba)) # 1.026755

# vhodna regija
svm.model <- svm(poraba ~ povrsina + temp_zraka + leto_izgradnje + letni_cas + vikend + namembnost + regija, dtVzhod)
predictedV <- predict(svm.model, testDt)
mae(testDt$poraba, predictedV) # 140.4451
mse(testDt$poraba, predictedV) # 47498.04
rmae(testDt$poraba, predictedV, mean(dtVzhod$poraba)) # 0.7674285
rmse(testDt$poraba, predictedV, mean(dtVzhod$poraba)) # 0.9238594



# nakljucni gozd
library(randomForest)
set.seed(0) # Pomembno
rf.model <- randomForest(poraba ~ povrsina + temp_zraka + leto_izgradnje + letni_cas + vikend + namembnost + regija, trainDt)
predicted <- predict(rf.model, testDt)
mae(testDt$poraba, predicted) # 92.4099
mse(testDt$poraba, predicted) # 26302.14
rmae(testDt$poraba, predicted, mean(dt$poraba)) # 0.5784746
rmse(testDt$poraba, predicted, mean(dt$poraba)) # 0.5818612

# vzhodna regija
set.seed(0) # Pomembno
rf.model <- randomForest(poraba ~ povrsina + temp_zraka + leto_izgradnje + letni_cas + vikend + namembnost + regija, dtVzhod)
predictedV <- predict(rf.model, testDt)
mae(testDt$poraba, predictedV) # 150.0842
mse(testDt$poraba, predictedV) # 58171.93
rmae(testDt$poraba, predictedV, mean(dtVzhod$poraba)) # 0.820099
rmse(testDt$poraba, predictedV, mean(dtVzhod$poraba)) # 1.131472


############################
#   Kombiniranje modelov   #
############################

#
# Glasovanje
#

pred <- data.frame(predDT, predNB, predKNN)
predV <- data.frame(predDTVzhod, predNBVzhod, predKNNVzhod)

head(pred)
head(predV)

voting <- function(predictions)
{
  res <- vector()
  
  for (i in 1 : nrow(predictions))  	
  {
    vec <- unlist(predictions[i,])
    res[i] <- names(which.max(table(vec)))
  }
  
  res
}

predClass <- voting(pred)
predClassV <- voting(predV)
head(predClass)
head(predClassV)

predicted <- factor(predClass, levels=levels(trainDt$namembnost))
predictedV <- factor(predClass, levels=levels(dtVzhod$namembnost))
head(predicted)
head(predictedV)

CA(testDt$namembnost, predicted) # 0.5207
CA(testDt$namembnost, predictedV) # 0.5025084


#
# Utezeno glasovanje
#

predDT.prob <- predict(modelDT, testDt, type="prob")
predNB.prob <- predict(modelNB, testDt, type="prob")
predKNN.prob <- predict(modelKNN, testDt, type="prob")

predDTVzhod.prob <- predict(modelDTVzhod, testDt, type="prob")
predNBVzhod.prob <- predict(modelNBVzhod, testDt, type="prob")
predKNNVzhod.prob <- predict(modelKNNVzhod, testDt, type="prob")

predProb <- predDT.prob + predNB.prob + predKNN.prob
predProbV <- predDTVzhod.prob + predNBVzhod.prob + predKNNVzhod.prob

head(predProb)

head(max.col(predProb))

predClass <- colnames(predProb)[max.col(predProb)]
predicted <- factor(predClass, levels(trainDt$namembnost))
head(predicted)

predClassV <- colnames(predProbV)[max.col(predProbV)]
predicted <- factor(predClassV, levels(dtVzhod$namembnost))

CA(testDt$namembnost, predicted) # 0.5025084
CA(testDt$namembnost, predictedV) # 0.5025084

#
# Boosting
#

# install.packages("adabag")
library(adabag)

bm <- boosting(namembnost ~ ., trainDt, mfinal=100) 
predictions <- predict(bm, testDt)

bmV <- boosting(namembnost ~ ., dtVzhod, mfinal=100) 
predictionsV <- predict(bmV, testDt)

names(predictions)

predicted <- predictions$class
predictedV <- predictionsV$class


CA(testDt$namembnost, predicted) # 0.4926003
CA(testDt$namembnost, predictedV) # 0.365092

