# RussiaProjectDiss
Co-authored work 

library(car)
library(coda)
library(runjags)
library('readstata13')

setwd("~/Desktop")
dat1 <- read.dta13("WV5_Data_stata_v_2015_04_18.dta", convert.factors=F)
dat1 <- read_dta('~/Desktop/WV5_Data_Russian_Federation_2006_stata_v_2015_04_18.dta')
tmp <- rep(NA, nrow(dat1))
tmp[which(dat1$V23 == "2" & dat1$V127 == "4")] <- "other"
tmp[which(dat1$V23 == "2" & dat1$V127 == "3")] <- "other"
tmp[which(dat1$V23 == "2" & dat1$V127 == "2")] <- "particular"
tmp[which(dat1$V23 == "2" & dat1$V127 == "1")] <- "particular"
tmp[which(dat1$V23 == "1" & dat1$V127 == "4")] <- "particular"
tmp[which(dat1$V23 == "1" & dat1$V127 == "3")] <- "particular"
tmp[which(dat1$V23 == "1" & dat1$V127 == "2")] <- "particular"
tmp[which(dat1$V23 == "1" & dat1$V127 == "1")] <- "generalized"
tmp <- factor(tmp, levels=c("generalized", "particular",  "other"))
dat1$trust <- tmp
table(dat1$trust)

dat1$dem <- as.numeric(dat1$V162)
dat1$dem <- recode(dat1$dem, "'1' = '1'; '2' = '2'; '3' = '3'; '4' = '4'; '5' =  '5'; '6' = '6'; '7' = '7'; '8' = '8'; '9' = '9'; '10' = '10'; else=NA")
table(dat1$dem)

dat1$educ <-as.numeric(dat1$V238)
dat1$educ <- recode(dat1$educ, "'1' = '1'; '2' = '2'; '3' = '3'; '4' = '4'; '5' =  '5'; '6' = '6'; '7' = '7'; '8' = '8'; '9' = '9'; else=NA")
table(dat1$educ)

dat1$income <-as.numeric(dat1$V253)
dat1$income <- recode(dat1$income, "'1' = '1'; '2' = '2'; '3' = '3'; '4' = '4'; '5' =  '5'; '6' = '6'; '7' = '7'; '8' = '8'; '9' = '9'; '10' = '10'; else=NA")
table(dat1$income)

dat1$age <- as.numeric(dat1$V237)

dat1$gender <- as.numeric(dat1$V235)
dat1$gender <- recode(dat1$gender, "'1' = '1'; '2' = '2'; else=NA")
table(dat1$gender)

dat1$net <- as.numeric(dat1$V228)
dat1$net <- recode(dat1$net, "'1' = '1'; '2' = '2'; else=NA")
table(dat1$net)

dat1$country <- dat1$V2
dat1$country <- recode(dat1$country,"'20' = 'Andorra';'32' = 'Argentina';'36' = 'Australia';'76' = 'Brazil';'100' = 'Bulgaria';'124' = 'Canada';'152' = 'Chile';'156' = 'China';'158' = 'Taiwan';'170' = 'Columbia';'196' = 'Cyprus';'231' = 'Ethiopia';'246' = 'Finland';'250' = 'France';'268' = 'Georgia';'276' = 'Germany';'288' = 'Ghana';'320' = 'Guatemala';'344' = 'Hong Kong';'348' = 'Hungary';'356' = 'India';'360' = 'Indonesia';'364' = 'Iran';'368' = 'Iraq';'380' = 'Italy';'392' = 'Japan';'400' = 'Jordan';'410' = 'South Korea';'458' = 'Malaysia';'466' = 'Mali';'484' = 'Mexico';'498' = 'Moldova';'504' = 'Montenegro';'528' = 'Netherlands';'554' = 'New Zealand';'578' = 'Norway';'604' = 'Peru';'616' = 'Poland';'642' = 'Romania';'643' = 'Russia';'646' = 'Rwanda';'704' = 'Vietnam';'705' = 'Slovenia';'710' = 'South Africa';'724' = 'Spain';'752' = 'Sweden';'756' = 'Switzerland';'764' = 'Thailand';'780' = 'Trinidad and Tobago';'792' = 'Turkey' ;'804' = 'Ukraine';'818' = 'Egypt';'826' = 'Great Britain';'840' = 'United States';'854' = 'Burkina Faso';'858' = 'Uruguay';'891' = 'Serbia and Montenegro';'894' = 'Zambia'; else=NA")
table(dat1$country)
wvs5 <- dat1
cpi <- read.csv('newcpi2010.csv')
wvs5$cpi <- cpi[match(tolower(as.character(wvs5$country)), tolower(as.character(cpi[,2]))), "cpi"]

wvs5$cpi[which(wvs5$country == "Andorra")] <-  NA
wvs5$cpi[which(wvs5$country == "South Korea")] <- 5.4
wvs5$cpi[which(wvs5$country == "Viet Nam")] <- 2.7
wvs5$cpi[which(wvs5$country == "South Africa")] <- 4.5
wvs5$cpi[which(wvs5$country == "Great Britain")] <- 7.6
wvs5$cpi[which(wvs5$country == "United States")] <- 7.1
wvs5$cpi[which(wvs5$country == "Serbia and Montenegro")] <- 2.8
wvs5$cpi[which(wvs5$country == "Columbia")] <- 3.5

tmp <- wvs5[complete.cases(wvs5[,c("dem", "trust", "gov", "age", "gender",
  "educ", "income", "net", "immigrants")]), ]

wvsB <- wvs5[,c("trust", "dem", "gender", "educ", "income", "age", "country", "net")]

tmp <- tmp[which(tmp$country %in% c("Russia",  "Georgia", "Poland",  "Ukraine", "Moldova")), ]

##################################
library(lme4)
mod1 <- lmer(dem ~ trust*cpi + age + gender + educ + income + net + (trust|country), data=tmp)

summary(mod1)
##################################
unc <- unique(wvs$country)
ag.cpi <- aggregate(wvs$cpi, list(wvs$country), mean)
ag.cpi <- ag.cpi[match(unc, ag.cpi[,1]), ]
ctry <- match(wvs$country, unc)
form <- as.formula("~ trust + age + gender + educ + income + net")
X <- model.matrix(form, data=wvs)

# cat(dump.format(list(
#   N = nrow(X), Nc = length(unique(tmp$country)), g0 = c(0,0), G0 = diag(2)*.1,
#   country=ctry, X=X, y = tmp$dem, cpi=ag.cpi[,2], Tau = diag(3)*.1)), file="nf.dat")

dat.list <- list(
  N = nrow(X), Nc = length(unique(wvs$country)), g0 = c(0,0), G0 = diag(2)*.1,
  country=ctry, X=X, y = wvs$dem, cpi=ag.cpi[,2], Tau = diag(3)*.1)

mod.code <- "
model{
  for(i in 1:N){
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- inprod(X[i,], b[country[i], ])
  }
  for(k in 1:8){
    b.fix[k] ~ dnorm(0,.01)
  }
  for(j in 1:Nc){
    b[j,1:3] ~ dmnorm(mu.b[j,], Tau[1:3,1:3])
    mu.b[j,1] <- g[1,1] + g[1,2]*cpi[j]
    mu.b[j,2] <- g[2,1] + g[2,2]*cpi[j]
    mu.b[j,3] <- g[3,1] + g[3,2]*cpi[j]
    for(k in 4:8){
      b[j,k] <- b.fix[k]
    }
  }
  for(m in 1:3){
    g[m,1:2] ~ dmnorm(g0[1:2], G0[1:2,1:2])
  }
  tau ~ dgamma(1,.1)
}
"

out1 <- run.jags(mod.code, data=dat.list, monitor=c("b", "g"),
  n.chains = 2, burnin=100000, sample=100000, keep.jags.files=T)


