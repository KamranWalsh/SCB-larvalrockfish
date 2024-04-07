datasheet <- read.csv("~/Downloads/ProportionSheetRFFinalKW.csv")

#View(datasheet)
library(rethinking)

# size metrics
corr <- (datasheet$Core)*(10^6) #otolith core radius
SL1 <- datasheet$SL #standard length 
age1 <- datasheet$Age # age
RG1 <- (datasheet$Recent.Growth)*(10^6) #recent growth estimates
RGdiff1 <- datasheet$DifferenceRG #residuals of recent growth estimates

# random effect
station <- datasheet$Station.No..Integer #sample station

# presence vs. absence
calpresence <- datasheet$CalPresence #presence/absence calanoid copepodite
naupliipresence <- datasheet$NaupPresence #presence/absence nauplii
calpresencescale <- scale(calpresence)
nauppresencescale <- scale(naupliipresence)

# proportion of total biomass 
cbb <- datasheet$Cal.Gut #proportion of calanoid copepodites to total gut content biomass
nbb <- datasheet$Naup.Gut #proportion of nauplii to total gut content biomass
cbio <- scale(cbb)
nbio <- scale(nbb)

# scale size metrics
zRG1 <- scale(RG1)
zRGdiff1 <- scale(RGdiff1)
zSL <- scale(SL1)
scalecor <- scale(corr)
scala <- scale(age1)

inputdata <- data.frame(cbio, nbio,
                      calpresencescale, nauppresencescale,
                       zRG1, zRGdiff1,
                        zSL, station, scalecor, scala)

data_list <- list(
  Length = inputdata$zSL,
  Core = inputdata$scalecor,
  Age = inputdata$scala,
  Cal = inputdata$cbio,
  Naup = inputdata$nbio,
  CalBinary = inputdata$calpresencescale,
  NaupBinary = inputdata$nauppresencescale,
  RG = inputdata$zRG1,
  RGResid = inputdata$zRGdiff1,
  station = inputdata$station
)

#coreage

CoreAge <- ulam(
  alist(
    Age ~ dnorm(mu,sigma),
    mu <- a[station] + bCore*Core,
    bCore ~ dnorm(0,1),
    a[station] ~ dnorm(0, a_sigma),
    a_sigma ~ dexp(1),
    sigma ~ dexp(1)
  ),
  data = data_list, chains = 4, cores = 4, log_lik = TRUE
)

precis(CoreAge, depth = 2)
plot(CoreAge, col = "black", depth = 2, xlab = "Mean +/- CI")

# prey items in proportion to total prey biomass as predictors of SL

ProportionofBioSL <- ulam(
  alist(
    Length ~ dnorm(mu,sigma),
    mu <- a[station] + bCore*Core + bCalanoid_Copepodite*Cal + bAge*Age +
    bNauplii*Naup + bCalanoid_Age*Cal*Age + bNauplii_Age*Naup*Age,
    c(bCore, bCalanoid_Copepodite, bAge, bNauplii) ~ dnorm(0,1),
    c(bCalanoid_Age, bNauplii_Age) ~ dnorm(0,1),
    a[station] ~ dnorm(0, a_sigma),
    a_sigma ~ dexp(1),
    sigma ~ dexp(1)
  ),
  data = data_list, chains = 4, cores = 4, log_lik = TRUE
)
precis(ProportionofBioSL, depth = 2)
plot(ProportionofBioSL, col = "blue", depth = 2, pars = c("bAge", "bNauplii", "bCalanoid_Copepodite", "bCore", 
                                                          "bNauplii_Age", "bCalanoid_Age", "a[1]",
                                                          "a[2]", "a[3]", "a[4]", "a[5]", "a[6]", "a[7]",
                                                          "a_sigma", "sigma"), xlab = "Mean +/- CI", main = "ProportionofBio - Standard Length")

# prey items in proportion to total prey biomass as predictors of RG

ProportionofBioRG <- ulam(
  alist(
    RG ~ dnorm(mu,sigma),
    mu <- a[station] + bCore*Core + bCalanoid_Copepodite*Cal + bAge*Age +
      bNauplii*Naup + bCalanoid_Age*Cal*Age + bNauplii_Age*Naup*Age,
    c(bCore, bCalanoid_Copepodite, bNauplii, bAge) ~ dnorm(0,1),
    c(bCalanoid_Age, bNauplii_Age) ~ dnorm(0,1),
    a[station] ~ dnorm(0, a_sigma),
    a_sigma ~ dexp(1),
    sigma ~ dexp(1)
  ),
  data = data_list, chains = 4, cores = 4, log_lik = TRUE
)
precis(ProportionofBioRG, depth = 2)
plot(ProportionofBioRG, col = "red", depth = 2, pars = c("bAge", "bNauplii", "bCalanoid_Copepodite", "bCore", 
                                                         "bNauplii_Age", "bCalanoid_Age", "a[1]",
                                                         "a[2]", "a[3]", "a[4]", "a[5]", "a[6]", "a[7]",
                                                         "a_sigma", "sigma"), xlab = "Mean +/- CI", main = "ProportionofBio - Recent Growth")

# binary presence/absence of SL

BinaryPresenceAbsenceSL <- ulam(
  alist(
    Length ~ dnorm(mu,sigma),
    mu <- a[station] + bCore*Core + bNaupPresence*NaupBinary + bAge*Age + 
      bCalPresence*CalBinary,
    c(bCore, bCalPresence, bNaupPresence, bAge) ~ dnorm(0,1),
    a[station] ~ dnorm(0, a_sigma),
    a_sigma ~ dexp(1),
    sigma ~ dexp(1)
  ),
  data = data_list, chains = 4, cores = 4, log_lik = TRUE
)
precis(BinaryPresenceAbsenceSL)
plot(BinaryPresenceAbsenceSL, col = "blue", depth = 2, xlab = "Mean +/- CI", main = "Presence/Absence - SL")

# binary presence/absence of RG

BinaryPresenceAbsenceRG <- ulam(
  alist(
    RG ~ dnorm(mu,sigma),
    mu <- a[station] + bCore*Core + bCalPresence*CalBinary + bNaupPresence*NaupBinary + bAge*Age,
    c(bCore, bCalPresence, bNaupPresence, bAge) ~ dnorm(0,1),
    a[station] ~ dnorm(0, a_sigma),
    a_sigma ~ dexp(1),
    sigma ~ dexp(1)
  ),
  data = data_list, chains = 4, cores = 4, log_lik = TRUE
)
precis(BinaryPresenceAbsenceRG)
plot(BinaryPresenceAbsenceRG, col = "blue", depth = 2, main = "Presence/Absence - RG")

# counterfactual models without random effect

ProportionSL.ME <- ulam(
  alist(
    Length ~ dnorm(mu, sigma),
    mu <- a + bCore*Core + bNaupBio*Naup + bAge*Age + 
      bCalBio*Cal + bAgeCal*Cal*Age + bAgeNaup*Naup*Age,
    c(bCore, bCalBio, bNaupBio, bAge) ~ dnorm(0,1),
    c(bAgeCal, bAgeNaup) ~ dnorm(0,1),
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = data_list, chains = 4, cores = 4, log_lik = TRUE
)
precis(ProportionSL.ME)
plot(ProportionSL.ME)

ProportionRG.ME <- ulam(
  alist(
    RG ~ dnorm(mu, sigma),
    mu <- a + bCore*Core + bNaupBio*Naup + bAge*Age + 
      bCalBio*Cal + bAgeCal*Cal*Age + bAgeNaup*Naup*Age,
    c(bCore, bCalBio, bNaupBio, bAge) ~ dnorm(0,1),
    c(bAgeCal, bAgeNaup) ~ dnorm(0,1),
    a ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ),
  data = data_list, chains = 4, cores = 4, log_lik = TRUE
)
precis(ProportionRG.ME)
plot(ProportionRG.ME)

# traceplots
#traceplot(CoreAge)
#traceplot(ProportionofBioSL)
#traceplot(ProportionofBioRG)

#counterfactuals

Core_seq <- seq( from=min(data_list$Core) , to=max(data_list$Core) , length.out=30 )
Naup_seq <- seq( from=min(data_list$Naup) , to=max(data_list$Naup), length.out=30 )
Age_seq <- seq( from=min(data_list$Age) , to=max(data_list$Age) , length.out=30 )
Cal_seq <- seq( from=min(data_list$Cal) , to=max(data_list$Cal) , length.out=30 )

mu <- link(ProportionSL.ME , data=data.frame( Core=0, Age=1.96, Naup=Naup_seq, Cal=0) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
mu2 <- link(ProportionSL.ME , data=data.frame( Core=0, Age=-1.96, Naup=Naup_seq, Cal=0) )
mu_mean2 <- apply(mu2,2,mean)
mu_PI2 <- apply(mu2,2,PI)
plot.default( NULL , xlim=range(-0.25, 1.5) , ylim=range(-2,2), ylab = "", xlab="")
title(xlab = "% Calanoid and Cyclopoid Nauplii Gut Content Biomass", cex.lab=1.25, line = 2.5, ylab = "Standard Length")
lines( Naup_seq , mu_mean , lwd=1 , col = "red")
shade( mu_PI , Naup_seq , col = rgb(1, 0, 0,0.5))
lines( Naup_seq , mu_mean2 , lwd=2, col = "blue" )
shade( mu_PI2 , Naup_seq, col= rgb(0, 0, 1,0.5) )
legend("topleft", legend=c("Older Larvae", "Younger Larvae"), 
       fill = c( rgb(1,0,0,0.5), rgb(0, 0, 1,0.5)), cex=1.15)

mu <- link(ProportionSL.ME , data=data.frame( Core=0, Age=1.96, Naup=0, Cal=Cal_seq) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
mu2 <- link(ProportionSL.ME , data=data.frame( Core=0, Age=-1.96, Naup=0, Cal=Cal_seq) )
mu_mean2 <- apply(mu2,2,mean)
mu_PI2 <- apply(mu2,2,PI)
plot.default( NULL , xlim=range(-0.25, 1.5) , ylim=range(-2,2), ylab = "", xlab = "")
title(xlab = "% Calanoid Copepodite Gut Content Biomass", cex.lab=1.25, line = 2.5, ylab = "Standard Length")
lines( Cal_seq , mu_mean , lwd=1 , col = "red")
shade( mu_PI , Cal_seq , col = rgb(1, 0, 0,0.5))
lines( Cal_seq , mu_mean2 , lwd=2, col = "blue" )
shade( mu_PI2 , Cal_seq, col= rgb(0, 0, 1,0.5) )
legend("topleft", legend=c("Older Larvae", "Younger Larvae"), 
       fill = c( rgb(1,0,0,0.5), rgb(0, 0, 1,0.5)), cex=1.15)

mu <- link(ProportionRG.ME , data=data.frame( Core=0, Age=1.96, Naup=Naup_seq, Cal=0) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
mu2 <- link(ProportionRG.ME , data=data.frame( Core=0, Age=-1.96, Naup=Naup_seq, Cal=0) )
mu_mean2 <- apply(mu2,2,mean)
mu_PI2 <- apply(mu2,2,PI)
plot.default( NULL , xlim=range(-0.25, 1.5) , ylim=range(-2,2), xlab = "", ylab = "")
title(xlab = "% Calanoid and Cyclopoid Nauplii Gut Content Biomass", cex.lab=1.25, line = 2.5, ylab = "Recent Growth")
lines( Naup_seq , mu_mean , lwd=1 , col = "red")
shade( mu_PI , Naup_seq , col = rgb(1, 0, 0,0.5))
lines( Naup_seq , mu_mean2 , lwd=2, col = "blue" )
shade( mu_PI2 , Naup_seq, col= rgb(0, 0, 1,0.5) )
legend("topleft", legend=c("Older Larvae", "Younger Larvae"), 
       fill = c( rgb(1,0,0,0.5), rgb(0, 0, 1,0.5)), cex=1.15)

mu <- link(ProportionRG.ME , data=data.frame( Core=0, Age=1.96, Naup=0, Cal=Cal_seq) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
mu2 <- link(ProportionRG.ME , data=data.frame( Core=0, Age=-1.96, Naup=0, Cal=Cal_seq) )
mu_mean2 <- apply(mu2,2,mean)
mu_PI2 <- apply(mu2,2,PI)
plot.default( NULL , xlim=range(-0.25, 1.5) , ylim=range(-2,2), xlab = "", ylab = "")
title(xlab = "% Calanoid Copepodite Gut Content Biomass", cex.lab=1.25, line = 2.5, ylab = "Recent Growth")
  lines( Cal_seq, mu_mean, lwd=1 , col = "red")
shade( mu_PI , Cal_seq , col = rgb(1, 0, 0,0.5))
lines( Cal_seq , mu_mean2 , lwd=2, col = "blue" )
shade( mu_PI2 , Cal_seq, col= rgb(0, 0, 1,0.5) )
legend("topleft", legend=c("Older Larvae", "Younger Larvae"), 
       fill = c( rgb(1,0,0,0.5), rgb(0, 0, 1,0.5)), cex=1.15)

#PPC

sim1 <- rethinking::sim(ProportionofBioSL, post = extract.samples(ProportionofBioSL), n=1000)
priorsim1 <- rethinking::sim(ProportionofBioSL, post = extract.prior(ProportionofBioSL), n=1000)
hist(data_list$Length, prob=TRUE, 12, col="grey", xlim = c(-5,5), ylim = c(0,0.6), lwd=2,
     main = "Standard Length", cex.main = 1.5, cex.axis = 1.0, xlab="Standardized Length Distribution", ylab="Density")
# title(ylab= "Density", line=2)
# title(xlab= "Standardized Length Distribution", line=2)
# title( main="mSL1", cex.main = 1.5, line=1.5)
lines(density(sim1), col=rgb(1, 0, 0,1), lwd= 3) 
lines(density(priorsim1), col=rgb(0,0,1,1), lwd=3)
legend("topright", legend=c("Observed Data", "Posterior", "Prior"), 
        fill = c("grey", rgb(1,0,0,1), rgb(0, 0, 1,1)))

sim2 <- rethinking::sim(ProportionofBioRG, post = extract.samples(ProportionofBioRG), n=1000)
priorsim2 <- rethinking::sim(ProportionofBioRG, post = extract.prior(ProportionofBioRG), n=1000)
hist(data_list$RG, prob=TRUE, 12, col="grey", xlim = c(-5,5), ylim = c(0,0.5), lwd=2,
     main = "Recent Growth", cex.main = 1.5, cex.axis = 1.0, xlab="Standardized Recent Growth Distribution", ylab="Density")
# title(ylab= "Density", line=2)
# title(xlab= "Standardized Length Distribution", line=2)
# title( main="mSL1", cex.main = 1.5, line=1.5)
lines(density(sim2), col=rgb(1, 0, 0,1), lwd= 3) 
lines(density(priorsim2), col=rgb(0,0,1,1), lwd=3)
legend("topright", legend=c("Observed Data", "Posterior", "Prior"), 
       fill = c("grey", rgb(1,0,0,1), rgb(0, 0, 1,1)))

sim3 <- rethinking::sim(CoreAge, post = extract.samples(CoreAge), n=1000)
priorsim3 <- rethinking::sim(CoreAge, post = extract.prior(CoreAge), n=1000)
hist(data_list$Age, prob=TRUE, 12, col="grey", xlim = c(-5,5), ylim = c(0,0.5), lwd=2,
     main = "Age ~ Otolith Core", cex.main = 1.5, cex.axis = 1.0, xlab="Standardized Age Distribution", ylab="Density")
# title(ylab= "Density", line=2)
# title(xlab= "Standardized Length Distribution", line=2)
# title( main="mSL1", cex.main = 1.5, line=1.5)
lines(density(sim3), col=rgb(1, 0, 0,1), lwd= 3) 
lines(density(priorsim3), col=rgb(0,0,1,1), lwd=3)
legend("topright", legend=c("Observed Data", "Posterior", "Prior"), 
       fill = c("grey", rgb(1,0,0,1), rgb(0, 0, 1,1)))

