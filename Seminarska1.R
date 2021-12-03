#########################################################
#  Seminarska naloga 1 pri predmetu Umetna Inteligenca  #
#########################################################


##############################
#   Dodajanje ucne mnozice   #
##############################

dt <-  read.table(file="ucnaSem1.txt", sep=",", header=T, stringsAsFactors = T)

# Popravimo datum
dt$datum <- as.Date(dt$datum, "%Y-%m-%d")

summary(dt)

# Dodajanje atributa vikend
dan <- weekdays(dt$datum)

sobota <- dan == "Saturday"
nedelja <- dan == "Sunday"
vikend <- sobota|nedelja
vikend

dt$vikend <- vikend

dt$vikend <- as.factor(dt$vikend)

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

letni_cas <- getSeason(dt$datum)
dt$letni_cas <- letni_cas
# Letni cas je char zato ga se faktoriziramo
dt$letni_cas <- as.factor(dt$letni_cas)

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

smer_vetra1 <- getWindDirection(dt$smer_vetra)
smer_vetra1 <- as.factor(smer_vetra1)

dt$smer_vetra1 <- smer_vetra1

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

padavine_k <- kategorizirajPadavine(dt$padavine)
dt$padavine_k <- as.factor(padavine_k)
summary(dt$padavine_k)


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
hist(dt$poraba, xlab="Poraba v kWh", ylab="Frekvenca", main="Histogram porabe elektrike")

# Povprecna poraba
mean(dt$poraba)

# Povprecna poraba glede na namembnost
povpPorabaNam <- aggregate(as.double(poraba) ~ namembnost, data=dt, mean)
povpPorabaNam
plot(povpPorabaNam, ylab="Povprecna poraba v kWh", main="Povprecna poraba glede na namembnost.")
# Skupno najvec porabijo izobrazevalne zgradbe, vendar povprecno pa najvec porabijo poslovne

#Povprecna poraba po letnem casu
meanPorabaLet <- aggregate(as.double(poraba) ~ letni_cas, data=dt, mean)
meanPorabaLet
plot(meanPorabaLet, xlab="Letni cas", ylab="Povprecna poraba v kWh", main="Povprecna poraba glede na letni cas")
#Opazanja: v letnih casih z ekstremnimi temperaturami (poletje, zima) imamo vecjo porabo (hlajenje in segrevanje)

# Povprecna poraba glede na povrsino
meanPorabaPovr <- aggregate(as.double(poraba) ~ povrsina, data=dt, mean)
meanPorabaPovr
plot(meanPorabaPovr, xlab="Povrsina", ylab="Povprecna poraba v kWh", main="Povprecna poraba glede na povrsino")

# Visoka korelacija porabe s povrsino???
cor(dt$poraba, dt$povrsina)
# Oprazanja: stavbe z vecjo povrsino naceloma porabijo povprecno vec energije

# Povprecna poraba glede na vikend ali cez teden

names(summary(dt$namembnost)) -> namembnosti

rm(jeVikend, niVikend)
jeVikend <- c()
niVikend <- c()
for (ime in namembnosti){
  v1 <- dt$vikend == TRUE & dt$namembnost == ime
  v2 <- dt$vikend == FALSE & dt$namembnost == ime
  
  jeVikend <- c(jeVikend, mean(dt[dt$poraba[v1], "poraba"]))
  niVikend <- c(niVikend, mean(dt[dt$poraba[v2], "poraba"]))
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
nadpovp <- table(dt$poraba > mean(dt$poraba))
names(nadpovp) <- c("Podpovprecna poraba", "Nadpovprecna poraba")
pie(nadpovp, main="Razmerje")


###############################################
#    Ocenjevanje atributov - klasifikacija    #
###############################################

# Ocenjujemo s pomocjo paketa CORElearn
library(CORElearn)

sort(attrEval(namembnost ~ ., dt, "InfGain"), decreasing = TRUE)

odl <- CoreModel(namembnost ~ ., dt, model="tree", selectionEstimator="InfGain")
plot(odl, dt)

sort(attrEval(namembnost ~ ., dt, "Gini"), decreasing = TRUE)

sort(attrEval(namembnost ~ ., dt, "GainRatio"), decreasing = TRUE)

sort(attrEval(namembnost ~ ., dt, "MDL"), decreasing = TRUE)

odl <- CoreModel(namembnost ~ ., dt, model="tree", selectionEstimator="MDL")
plot(odl, dt)

sort(attrEval(namembnost ~ ., dt, "InfGain", binaryEvaluation=T), decreasing = T)

sort(attrEval(namembnost ~ ., dt, "Gini", binaryEvaluation=T), decreasing = T)

source("wrapper.R")
library(rpart)

myTrainFunc <- function(formula, traindata)
{
  rpart(formula, traindata)	
}

#
# Funkcija za pridobivanje napovedi modela (razredi)
#

myPredictFunc <- function(model, testdata)
{
  predict(model, testdata, type="class")
}

#
# Atribute lahko izberemo glede na klasifikacijsko tocnost modela
#

myEvalFunc <- function(predicted, observed, trained)
{
  # vracamo napako modela, saj wrapper minimizira vrednost ocene
  1.0 - mean(observed == predicted)	
}

#Z wrapperjem preverimo kar vse atribute 
# (brez tistih, iz katerih smo izpeljali druge in brez indeksnih)
set.seed(0)
wrapper(namembnost ~ regija+povrsina+leto_izgradnje+temp_zraka+temp_rosisca+oblacnost+padavine_k+pritisk+smer_vetra1+hitrost_vetra+poraba+vikend+letni_cas, dt, myTrainFunc, myPredictFunc, myEvalFunc, cvfolds=10)
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
wrapper(namembnost ~ regija+povrsina+leto_izgradnje+temp_zraka+temp_rosisca+oblacnost+padavine_k+pritisk+smer_vetra1+hitrost_vetra+poraba+vikend+letni_cas, dt, myTrainFunc, myPredictFuncProb, myEvalFuncBrier, cvfolds=10)
# best model: estimated error =  0.01229021 selected feature subset =  namembnost ~ povrsina + leto_izgradnje + temp_zraka 
# selected feature subset =  namembnost ~ povrsina + leto_izgradnje + temp_zraka


################################
#   Dodajanje testne mnozice   #
################################

test <-  read.table(file="testnaSem1.txt", sep=",", header=T, stringsAsFactors = T)
#Popravimo datum
test$datum <- as.Date(test$datum, "%Y-%m-%d")
# Dodajanje atributa vikend
dan1 <- weekdays(test$datum)
sobota1 <- dan1 == "Saturday"
nedelja1 <- dan1 == "Sunday"
vikend1 <- sobota1|nedelja1
vikend1
test$vikend <- vikend1
typeof(dt$vikend)

#Dodajanje atributa letni cas
letni_cas <- getSeason(test$datum)
test$letni_cas <- letni_cas
test$letni_cas <- as.factor(test$letni_cas)

# Dodajanje atributa smer vetra
smer_vetra1 <- getWindDirection(test$smer_vetra)
smer_vetra1 <- as.factor(smer_vetra1)

test$smer_vetra1 <- smer_vetra1

# Klasificiranje atributa padavine
padavine_k <- kategorizirajPadavine(test$padavine)
test$padavine_k <- as.factor(padavine_k)
summary(test$padavine_k)


#########################################
#  Sestavljanje in ocenjevanje modelov  #
#########################################

# Modeli zgrajeni z atributi, ki jih je podal wrapper z nakljucnim seed-om 
modelDT <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine_k + pritisk + smer_vetra1 + hitrost_vetra + vikend , dt, model="tree")
# 0.4922659
modelNB <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine_k + pritisk + smer_vetra1 + hitrost_vetra + vikend , dt, model="bayes")
# 0.4330686
modelKNN <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + padavine_k + pritisk + smer_vetra1 + hitrost_vetra + vikend , dt, model="knn", kInNN = 5)
# 0.4815635

# Modeli zgrajeni z atributi, ki jih je podal wrapper z seed = 0
modelDT <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka, dt, model="tree")
# 0.4922659
modelNB <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka, dt, model="bayes")
# 0.4508779
modelKNN <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka, dt, model="knn", kInNN = 5)
#0.4320652

CA <- function(observed, predicted)
{
  mean(observed == predicted)
}

predDT <- predict(modelDT, test, type = "class")
caDT <- CA(test$namembnost, predDT)
caDT

predNB <- predict(modelNB, test, type="class")
caNB <- CA(test$namembnost, predNB)
caNB

predKNN <- predict(modelKNN, test, type="class")
caKNN <- CA(test$namembnost, predKNN)
caKNN


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
sort(attrEval(poraba ~ ., dt, "MSEofMean"), decreasing = TRUE)
sort(attrEval(poraba ~ ., dt, "RReliefFexpRank"), decreasing = TRUE)

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
wrapper(poraba ~ povrsina + leto_izgradnje + namembnost + regija + padavine_k + letni_cas + vikend + temp_zraka + pritisk + hitrost_vetra + temp_rosisca + oblacnost, dt, myTrainFuncReg, myPredictFuncReg, myEvalFuncRMSE, cvfolds=10)
# rezultat wrapperja
# poraba ~ povrsina + temp_zraka + leto_izgradnje + letni_cas + vikend + namembnost + regija


#########################################
#  Sestavljanje in ocenjevanje modelov  #
#########################################

# k najblizji
library(kknn)
modelReduced <- train.kknn(poraba ~ povrsina + temp_zraka + leto_izgradnje + letni_cas + vikend + namembnost + regija, dt, ks=5)
predicted <- predict(modelReduced, test)
mae(test$poraba, predicted)   # 126.7343
mse(test$poraba, predicted)   # 86195.75
rmae(test$poraba, predicted, mean(dt$poraba)) # 0.7933414
rmse(test$poraba, predicted, mean(dt$poraba)) # 1.90684

#svm
library(e1071)
svm.model <- svm(poraba ~ povrsina + temp_zraka + leto_izgradnje + letni_cas + vikend + namembnost + regija, dt)
predicted <- predict(svm.model, test)
mae(test$poraba, predicted) # 112.2171
mse(test$poraba, predicted) # 46412.86
rmae(test$poraba, predicted, mean(dt$poraba)) # 0.7024651
rmse(test$poraba, predicted, mean(dt$poraba)) # 1.026755

# nakljucni gozd
library(randomForest)
set.seed(0) # Pomembno
rf.model <- randomForest(poraba ~ povrsina + temp_zraka + leto_izgradnje + letni_cas + vikend + namembnost + regija, dt)
predicted <- predict(rf.model, test)
mae(test$poraba, predicted) # 92.4099
mse(test$poraba, predicted) # 26302.14
rmae(test$poraba, predicted, mean(dt$poraba)) # 0.5784746
rmse(test$poraba, predicted, mean(dt$poraba)) # 0.5818612





