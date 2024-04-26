library(readxl)

path <- "C:/Users/serig/OneDrive/vikrigt/skola/r_prog_ds23-main/kunskapskontroll/kunskaps kontrol/datainsamling.xlsx"
bilar <- read_excel(path)

head(bilar)

#OBS KODEN ÄR INTE STÄDAD, FINNS DELAR SOM GÖRS HÄR SOM INTE ÄR MED I RAPPORTEN.

#View(bilar)
#droppar länkar
bilar <- subset(bilar, select = -c(7))
head(bilar)
View(bilar)
#create dummies, k-1
#bränsletyp
bilar$Bränsle_bensin <- ifelse(bilar$Bränsle == "Bensin", 1, 0)
bilar$Bränsle_diesel <- ifelse(bilar$Bränsle == "Diesel", 1, 0)
bilar$Bränsle_hybrid <- ifelse(bilar$Bränsle == "Hybrid", 1, 0)# excludera sedan
#växellåda
bilar$Växellåda_Automat <- ifelse(bilar$Växellåda == "Automat", 1, 0)
bilar$Växellåda_Manuell <- ifelse(bilar$Växellåda == "Manuell", 1, 0)#bara för jämnförelse
#droppar bränsel och växellåda
bilar <- subset(bilar, select = -c(3:4))
attach(bilar)

lm_1_1 <- lm(bilar$pris~. -Bränsle_hybrid, data = bilar)
lm_1_2 <- lm(bilar$pris~. -Bränsle_bensin, data = bilar)
lm_1_3 <- lm(bilar$pris~. -Bränsle_diesel, data = bilar)

summary(lm_1_1)
par(mfrow = c(2, 2))
plot(lm_1_1)
summary(lm_1_2)
summary(lm_1_3)
lm_1_4 <- lm(bilar$pris~. -Växellåda_Automat, data = bilar)
summary(lm_1_4)#manuell växellåda påverkar priset negativt
#man ser oxå att det är de inverterat från automat.


#br_d = 00756 , 1.348e+04
#br_b = 0.00186 , 2.549e+04
#br_hy = 2.44e-06, -2.549e+04
#om man kollar på alla summeries, så ser man att hybrid har en negativ påverkan
#på priset jämnfört mot bensin och diesel.samt att bensin också är negativt påverkande
#jämnt emot diesel bilar
#notera dock att std.error är större än estimate vilket betyder att
#den kunde gå mot ett inverterat värde

#vi behåller lm_1_1 eftersom den ger bäst significans på dem dummy variablerna
lm_1 <- lm_1_1
rm(lm_1_1, lm_1_2, lm_1_3, lm_1_4)


#kan vara icke linjär, hitta ett poly. kan va ej normalfördelad kolla obs
#förutom en observ så ser de ut o va liknande varians
#några observationer verkar vara utanför3 sr gränsen och samma obser 76 verkar vara värd att kolla på

bilar$predict = lm_1$fitted.values
bilar$andelnära = bilar$predict/bilar$pris
plot(bilar$andelnära)
abline(h = 1, col = "red")
hist(bilar$andelnära, breaks = 20)
hist(bilar$pris, breaks = 20)
hist(bilar$predict, breaks = 20)
View(bilar)

histo_data <- function(data){
  par(mfrow = c(2, 2))
  plot(data$andelnära)
  abline(h = 1, col = "red")
  hist(data$andelnära, breaks = 20)
  hist(data$pris, breaks = 20)
  hist(data$predict, breaks = 20)
}

histo_data(bilar)
#?lm
bilar <- bilar[-76, ]
bilar <- subset(bilar, select = -c(Bränsle_hybrid))
bilar <- subset(bilar, select = -c(Växellåda_Automat))
#kolla korrelation, skippa pris, predict och andel nära
bilarcor <- bilar
bilarcor <- subset(bilarcor, select = -c(pris))
bilarcor <- subset(bilarcor, select = -c(predict))
bilarcor <- subset(bilarcor, select = -c(andelnära))
cor(bilarcor)
pairs(cor(bilarcor))
#ingen större korrelation mellan variablerna

### rmse. OBS DETTA ÄR PÅ TRÄNINGS SETTET
sqrt(mean((bilar$pris-bilar$predict)^2))
###

lm_1 <- lm(bilar$pris~. - predict - andelnära, data = bilar)#samma modell men utan outlier

summary(lm_1)
par(mfrow = c(2, 2))
plot(lm_1)

hist(lm_1$residuals, breaks = 20) # 

library(car)
vif(lm_1)
#ingen vif över 5 heller, släpper den tanken
#sen testa att log transformera 

bilar$logpris <- log(bilar$pris)



lm_2 <- lm(bilar$logpris~. - predict - andelnära - pris, data = bilar)

summary(lm_2)
par(mfrow = c(2, 2))
plot(lm_2)
hist(lm_2$residuals, breaks = 20) # 
###rmse samma som förra också på trännings data.
sqrt(mean((exp(bilar$logpris)-exp(lm_2$fitted.values))^2))
#average av 32128 kr fel i modellen med alla variabler
#log transformation hjälper modellen, undersök hur variabler kan påverka modellen


histo_data(bilar)
#kolla och hantera bil 90. lm_2 med log(y) get bättre r^2
View (bilar)
# bil 90 är en bil som modellen fittade 30% ifrån den riktiga priset 
#låter den den vara kvar eftersom jag anser att ett 30% inte är onormalt
#bil priser ändrars ganska ofta och någon kan försöka sälja sin bil till överpris vilket kan ha
#påverkat modellen

#vi testar att göra en interations variable då man kan anse att ju äldre en bil är
#ju mer mil har den gått. Konstatera dock att dem 2 variablerna
#inte har sådan jättehög korrelation
lm_3 <- lm(bilar$pris~. - predict - andelnära - pris +Modellår:Miltal, data = bilar)
summary(lm_3)
par(mfrow = c(2, 2))
plot(lm_3)
#ej signifikanta och residualerna blir horribla. vi släpper de
rm(lm_3)

library(leaps)

regfit_fc <- function(parametrar,data){
  regfit_full <- regsubsets(parametrar, data = data)
  plot(regfit_full, scale = "r2")
  plot(regfit_full, scale = "adjr2")
  plot(regfit_full, scale = "Cp")
  plot(regfit_full, scale = "bic")
  return (regfit_full) 
}
regfit_pris.model = regfit_fc(bilar$pris~. -logpris -andelnära -predict,bilar)
regfit_pris <- summary(regfit_pris.model)
regfit_logpris.model = regfit_fc(bilar$logpris~. -pris -andelnära -predict,bilar)
regfit_logpris <- summary(regfit_logpris.model)
names(regfit_pris)#scores basicly
regfit_plot <- function(regfit.sum){
  par(mfrow = c(2, 2))
  plot(regfit.sum$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
  points(which.min(regfit.sum$rss), regfit.sum$rss[which.min(regfit.sum$rss)], col = "red", cex = 2, pch = 20)
  
  plot(regfit.sum$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
  points(which.max(regfit.sum$adjr2), regfit.sum$adjr2[which.max(regfit.sum$adjr2)], col = "red", cex = 2, pch = 20)
  legend("bottomright", legend = paste("best score",round(max(regfit.sum$adjr2),3)))
  
  plot(regfit.sum$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
  points(which.min(regfit.sum$cp), regfit.sum$cp[which.min(regfit.sum$cp)], col = "red", cex = 2, pch = 20)
  legend("topright", legend = paste("best score",min(regfit.sum$cp)))
  
  plot(regfit.sum$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
  points(which.min(regfit.sum$bic), regfit.sum$bic[which.min(regfit.sum$bic)], col = "red", cex = 2, pch = 20)
  legend("topright", legend = paste("best score",round(min(regfit.sum$bic),2)))
}
regfit_plot(regfit_pris)
regfit_pris
regfit_pris$rss
regfit_plot(regfit_logpris)
regfit_logpris
regfit_logpris$bic
regfit_logpris$adjr2
regfit_logpris$rss

confint(lm_2)
confint(lm_1)


?confint
regfit_fwd <- regsubsets(bilar$logpris~. -pris -andelnära -predict, data = bilar, nvmax = 6, method = "forward")
summary(regfit_fwd)

regfit_bwd <- regsubsets(bilar$logpris~. -pris -andelnära -predict, data = bilar, nvmax = 6, method = "backward")
summary(regfit_bwd)

coef(regfit_fwd, 6)
coef(regfit_bwd, 6)

#cv regfit
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}
prisbilar_cv_df <- subset(bilar, select = c(Modellår,Miltal
                                            ,Hästkrafter,pris
                                            ,Bränsle_bensin,Bränsle_diesel
                                            ,Växellåda_Manuell))
logprisbilar_cv_df <- subset(bilar, select = c(Modellår,Miltal
                                               ,Hästkrafter,logpris
                                               ,Bränsle_bensin,Bränsle_diesel
                                               ,Växellåda_Manuell))
k <- 10
n <- nrow(bilar)
set.seed(1)
folds <- sample(rep(1:k, length = n))
folds
table(folds)
p = 6
cv.errors <- matrix(NA, k, p,
                    dimnames = list(NULL, paste(1:p)))

for (j in 1:k) {
  print(j)
  best.fit <- regsubsets(pris~.,data=prisbilar_cv_df[folds != j, ])
  for (i in 1:p) {
    pred <- predict(best.fit, prisbilar_cv_df[folds == j, ], id = i)
    cv.errors[j, i] <-
      mean((prisbilar_cv_df$pris[folds == j] - pred)^2)
  }
}

cv.errorslog <- matrix(NA, k, p,
                    dimnames = list(NULL, paste(1:p)))
for (j in 1:k) {
  best.fit <- regsubsets(logpris~.,data=logprisbilar_cv_df[folds != j, ])
  for (i in 1:p) {
    pred <- predict(best.fit, logprisbilar_cv_df[folds == j, ], id = i)
    cv.errorslog[j, i] <-
      mean((exp(logprisbilar_cv_df$logpris[folds == j]) - exp(pred))^2)
    #måste transformera här annars kommer logpris-logpred inte transformeras rätt tillbaka
    #till ett begripligt pris fel
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
sqrt(mean.cv.errors)#root till pris

mean.cv.errorslog <- apply(cv.errorslog, 2, mean)
mean.cv.errorslog
sqrt(mean.cv.errorslog)

par(mfrow = c(1, 2))
plot(sqrt(mean.cv.errors), type = "b", main = "modell pris")
points(which.min(sqrt(mean.cv.errors)), sqrt(mean.cv.errors)[which.min(sqrt(mean.cv.errors))], col = "red", cex = 2, pch = 20)
plot(sqrt(mean.cv.errorslog), type = "b", main = "modell logpris")
points(which.min(sqrt(mean.cv.errorslog)), sqrt(mean.cv.errorslog)[which.min(sqrt(mean.cv.errorslog))], col = "red", cex = 2, pch = 20)

par(mfrow = c(1, 1))
plot(sqrt(mean.cv.errorslog), type = "b",ylab = "RMSE",xlab="Number of variables",lwd=2)
points(which.min(sqrt(mean.cv.errorslog)), sqrt(mean.cv.errorslog)[which.min(sqrt(mean.cv.errorslog))], col = "red", cex = 2, pch = 20)
lines(sqrt(mean.cv.errors), col = "blue",lty=2, type="b",lwd=2)
points(which.min(sqrt(mean.cv.errors)), sqrt(mean.cv.errors)[which.min(sqrt(mean.cv.errors))], col = "purple", cex = 2, pch = 20)
legend("topright", legend = c("Logpris model", "pris model"), col = c("black", "blue"), lty = c(1, 2), lwd = c(1, 2))
legend("right", legend = c(round(sqrt(mean.cv.errorslog)[which.min(sqrt(mean.cv.errorslog))],3)
                           ,round(sqrt(mean.cv.errors)[which.min(sqrt(mean.cv.errors))],3)), col = c("red", "purple"), pch = 20)



#cv modellering ger ett rmse på 32782 och 35057 respektive
#inte långt i från ml modell 1 och 2 vars rmse är ifrån samma träningsdata
#träna om lm_1/lm_2 till dem bästa
lm_bäst <- lm(logprisbilar_cv_df$logpris~., data = logprisbilar_cv_df)

summary(lm_bäst)
par(mfrow = c(2, 2))
plot(lm_bäst)
###pred test från https://www.blocket.se/annons/1001307003
#https://www.blocket.se/annons/1001216362
new_data_log <- data.frame(
  Modellår = c(2022,2020),
  Miltal = c(3600,8801), 
  Hästkrafter = c(340,150), 
  Bränsle_bensin = c(0,0),
  Bränsle_diesel = c(0,1),
  Växellåda_Manuell = c(0,0),
  logpris = c(log(379900),log(239900))
)

pred_lm_bäst_pi <- predict.lm(lm_bäst,new_data_log, interval = "prediction", level = 0.95)
pred_lm_bäst_ci<- predict.lm(lm_bäst,new_data_log, interval = "confidence", level = 0.95)
exp(pred_lm_bäst_pi)
exp(pred_lm_bäst_ci)
exp(new_data_log$logpris)/exp(pred_lm_bäst_ci)[,1]
#notera att pi är större då epsilon är med och då är de "riktiga" priset med i intervallet
#confidence intervallet är intervallet utan epsilon




### pxweb api

install.packages('pxweb')
library(pxweb)

#d <- pxweb_interactive("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/PersBilarDrivMedel")

#https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__TK__TK1001__TK1001A/PersBilarDrivMedel/table/tableViewLayout1/
#Eftersom scb hemsida ger en query i json format för en post request trots att de står format px?
#den queryn funkar inte till pxapin eftersom den tydligen saknar variabler i queryn
#man måste då skriva in dem variabler som saknas själv
#istället har jag använd deras egna interactive
#jag har valt den data jag vill ha och sen sparat query där ifrån.
#den queryn funkar nu tillsammans med pxweb_get_data()
#datan kommer finnas i d$data så samma set som pxweb_get_data() ger den


dqueryr <- '{
  "query": [
    {
      "code": "Region",
      "selection": {
        "filter": "item",
        "values": ["00"]
      }
    },
    {
      "code": "Drivmedel",
      "selection": {
        "filter": "item",
        "values": ["100", "110", "130"]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": ["TK1001AA"]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": ["2006M01", "2006M02", "2006M03", "2006M04", "2006M05", "2006M06", "2006M07"
        , "2006M08", "2006M09", "2006M10", "2006M11", "2006M12", "2007M01", "2007M02", "2007M03"
        , "2007M04", "2007M05", "2007M06", "2007M07", "2007M08", "2007M09", "2007M10", "2007M11"
        , "2007M12", "2008M01", "2008M02", "2008M03", "2008M04", "2008M05", "2008M06", "2008M07"
        , "2008M08", "2008M09", "2008M10", "2008M11", "2008M12", "2009M01", "2009M02", "2009M03"
        , "2009M04", "2009M05", "2009M06", "2009M07", "2009M08", "2009M09", "2009M10", "2009M11"
        , "2009M12", "2010M01", "2010M02", "2010M03", "2010M04", "2010M05", "2010M06", "2010M07"
        , "2010M08", "2010M09", "2010M10", "2010M11", "2010M12", "2011M01", "2011M02", "2011M03"
        , "2011M04", "2011M05", "2011M06", "2011M07", "2011M08", "2011M09", "2011M10", "2011M11"
        , "2011M12", "2012M01", "2012M02", "2012M03", "2012M04", "2012M05", "2012M06", "2012M07"
        , "2012M08", "2012M09", "2012M10", "2012M11", "2012M12", "2013M01", "2013M02", "2013M03"
        , "2013M04", "2013M05", "2013M06", "2013M07", "2013M08", "2013M09", "2013M10", "2013M11"
        , "2013M12", "2014M01", "2014M02", "2014M03", "2014M04", "2014M05", "2014M06", "2014M07"
        , "2014M08", "2014M09", "2014M10", "2014M11", "2014M12", "2015M01", "2015M02", "2015M03"
        , "2015M04", "2015M05", "2015M06", "2015M07", "2015M08", "2015M09", "2015M10", "2015M11"
        , "2015M12", "2016M01", "2016M02", "2016M03", "2016M04", "2016M05", "2016M06", "2016M07"
        , "2016M08", "2016M09", "2016M10", "2016M11", "2016M12", "2017M01", "2017M02", "2017M03"
        , "2017M04", "2017M05", "2017M06", "2017M07", "2017M08", "2017M09", "2017M10", "2017M11"
        , "2017M12", "2018M01", "2018M02", "2018M03", "2018M04", "2018M05", "2018M06", "2018M07"
        , "2018M08", "2018M09", "2018M10", "2018M11", "2018M12", "2019M01", "2019M02", "2019M03"
        , "2019M04", "2019M05", "2019M06", "2019M07", "2019M08", "2019M09", "2019M10", "2019M11"
        , "2019M12", "2020M01", "2020M02", "2020M03", "2020M04", "2020M05", "2020M06", "2020M07"
        , "2020M08", "2020M09", "2020M10", "2020M11", "2020M12", "2021M01", "2021M02", "2021M03"
        , "2021M04", "2021M05", "2021M06", "2021M07", "2021M08", "2021M09", "2021M10", "2021M11"
        , "2021M12", "2022M01", "2022M02", "2022M03", "2022M04", "2022M05", "2022M06", "2022M07"
        , "2022M08", "2022M09", "2022M10", "2022M11", "2022M12", "2023M01", "2023M02", "2023M03"
        , "2023M04", "2023M05", "2023M06", "2023M07", "2023M08", "2023M09", "2023M10", "2023M11"
        , "2023M12", "2024M01", "2024M02", "2024M03"]
      }
    }
  ],
  "response": {
    "format": "json"
  }
}'

urlpx = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/PersBilarDrivMedel"
dinfo <- pxweb_get_data(urlpx,dqueryr)
#transformera df så vi får data för varje månad brevid varandra
#för av någon anledning kommer inte datan så som den visas på hemsidan???

pxstatdataframe <- dinfo
scbdf <- subset(pxstatdataframe,select=c("månad","Nyregistrerade personbilar"), drivmedel=='bensin')

tempdf <- subset(pxstatdataframe,select="Nyregistrerade personbilar", drivmedel=='diesel')
scbdf$diesel <- tempdf$"Nyregistrerade personbilar"

tempdf <- subset(pxstatdataframe,select="Nyregistrerade personbilar", drivmedel=='elhybrid')
scbdf$hybrid <- tempdf$"Nyregistrerade personbilar"

scbdf$date <-as.Date(paste0(substr(scbdf$månad, 1, 4), "-", substr(scbdf$månad, 6, 7), "-01"))
scbdf <- subset(scbdf, select = -c(månad))
scbdf <- setNames(scbdf, c("bensin","diesel","hybrid","datum"))
scbdf$total <- scbdf$bensin + scbdf$diesel + scbdf$hybrid




par(mfrow = c(1, 1))
plot(x=scbdf$datum,y=scbdf$total,type = "l", xlab = "datum", ylab = "nyregistrerade bilar", main = "Nyregistrerade bilar per månad",lwd = 1,ylim = c(0,65000),lty = 2)
lines(x=scbdf$datum,y=scbdf$diesel, col = "gold",lty=1,lwd=1)
lines(x=scbdf$datum,y=scbdf$hybrid, col = "darkgreen",lty=1,lwd=1)
lines(x=scbdf$datum,y=scbdf$bensin, col = "red",lty=1,lwd=1)
points(scbdf$datum[which.min(scbdf$total)], scbdf$total[which.min(scbdf$total)], col = "purple", cex = 1, pch = 4,lwd=2)
abline(v=scbdf$datum[which.min(scbdf$total)],col="purple",lty=2)
legend("topleft", legend = c("Total","Nyreg bensin bil", "Nyreg diesel bil","Nyreg hybrid bil"),
       col = c("black","red", "gold","darkgreen"), lty = c(2,1,1,1), lwd = c(2,2,2,2))
legend("topright"
       , legend = paste(scbdf$total[which.min(scbdf$total)]
                        ," "
                        ,scbdf$date[which.min(scbdf$total)])
       ,col = "purple", lty=2,pch = 4, lwd = 2,cex = 1)

