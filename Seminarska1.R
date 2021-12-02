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


# -------- Vizualizacija podatkov ----------
# Vizualizacija namembnosti
# ZA POPRAVIT: tukaj vizualiziramo koliko *merjenj porabe* smo naredili v stavbah, ne koliko stavb imamo
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
# Oprazanja: stavbe z vecjo povrsino naceloma porabijo tudi povprecno vec energije

# Povprecna poraba glede na vikend ali ne

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


# Nadpovprecna ali podpovprecna poraba
nadpovp <- table(dt$poraba > mean(dt$poraba))
names(nadpovp) <- c("Podpovprecna poraba", "Nadpovprecna poraba")
pie(nadpovp, main="Razmerje")


#################################
#    Ocenjevanje atributov      #
#################################

# Ocenjujemo s pomocjo paketa CORElearn
library(CORElearn)

sort(attrEval(namembnost ~ ., dt, "InfGain"), decreasing = TRUE)

odl <- CoreModel(namembnost ~ ., dt, model="tree", selectionEstimator="InfGain")
plot(odl, dt)

sort(attrEval(namembnost ~ povrsina+regija+leto_izgradnje, dt, "Gini"), decreasing = TRUE)

sort(attrEval(namembnost ~ ., dt, "GainRatio"), decreasing = TRUE)

sort(attrEval(namembnost ~ ., dt, "MDL"), decreasing = TRUE)

odl <- CoreModel(namembnost ~ ., dt, model="tree", selectionEstimator="MDL")
plot(odl, dt)

sort(attrEval(namembnost ~ ., dt, "InfGain", binaryEvaluation=T), decreasing = T)
sort(attrEval(namembnost ~ ., dt, "Gini", binaryEvaluation=T), decreasing = T)



library(rpart)

modelFull <- rpart(namembnost ~ ., dt)
predicted <- predict(modelFull, dt, type="class")
mean(dt$namembnost == predicted)


source("wrapper.R")

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

set.seed(0)
wrapper(namembnost ~ regija+povrsina+leto_izgradnje+temp_zraka+temp_rosisca+oblacnost+padavine_k+pritisk+smer_vetra1+hitrost_vetra+poraba+vikend+letni_cas, dt, myTrainFunc, myPredictFunc, myEvalFunc, cvfolds=10)

# testirajmo na neodvisni testni mnozici
modelWrap <- rpart(namembnost ~ povrsina + leto_izgradnje + datum + temp_zraka + temp_rosisca, dt)
predicted <- predict(modelWrap, dt, type="class")
mean(dt$namembnost == predicted)

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
wrapper(namembnost ~ ., dt, myTrainFunc, myPredictFuncProb, myEvalFuncBrier, cvfolds=10)



# ------- Dodajanje testne mnozice v R
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

# Sestavljanje modelov z razlicnimi atributi
# Smiselni atributi z visokim GainRatio
modelDT <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + pritisk + hitrost_vetra + vikend, dt, model="tree")
modelNB <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + pritisk + hitrost_vetra + vikend, dt, model="bayes")
modelKNN <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + pritisk + hitrost_vetra + vikend, dt, model="knn", kInNN = 5)


modelDT <- CoreModel(namembnost ~ povrsina + leto_izgradnje + regija, dt, model="tree")
modelNB <- CoreModel(namembnost ~ povrsina + leto_izgradnje + regija, dt, model="bayes")
modelKNN <- CoreModel(namembnost ~ povrsina + leto_izgradnje + regija, dt, model="knn", kInNN = 5)

modelDT <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + pritisk + hitrost_vetra, dt, model="tree")
modelNB <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + pritisk + hitrost_vetra, dt, model="bayes")
modelKNN <- CoreModel(namembnost ~ povrsina + leto_izgradnje + temp_zraka + temp_rosisca + oblacnost + pritisk + hitrost_vetra, dt, model="knn", kInNN = 5)

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






