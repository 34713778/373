# Chemical
library(readxl)
library(sm)
Chem_RD1 <- read_excel("Chem_RD1.xlsx")
Chem_RD2 <- read_excel("Chem_RD2.xlsx")
Chem_PB1 <- read_excel("Chem_PB1.xlsx")


S <- function(m,s){
  sqrt(log(1+s^2/m^2))
}

M <- function(m,s){
  log(m/(sqrt(1+s^2/m^2)))
}

qL <- function(M,S,w){
  exp(M+w*S)
}

se <- function(s,N){
  s/sqrt(N)
}

LCI <- function(m,se,w){
  m-w*se
}

CI <- function(m,s,w){
  c(m-w*s, m+w*s)
}

# RD1

# BOD
RD1_BOD <- Chem_RD1$`0085 BOD ATU mg/l`
RD1_BOD_m <- mean(RD1_BOD, na.rm = TRUE )
RD1_BOD_s <- sd(RD1_BOD, na.rm = TRUE)
RD1_BOD_M <- M(RD1_BOD_m, RD1_BOD_s)
RD1_BOD_S <- S(RD1_BOD_m, RD1_BOD_s)
RD1_BOD_w <- 0.9540
RD1_BOD_qL <- qL(RD1_BOD_M, RD1_BOD_S, RD1_BOD_w)



# Amm
RD1_Amm <- Chem_RD1$`0111 Ammonia - N mg/l`
RD1_Amm_m <- mean(RD1_Amm, na.rm = TRUE )
RD1_Amm_s <- sd(RD1_Amm, na.rm = TRUE)
RD1_Amm_M <- M(RD1_Amm_m, RD1_Amm_s)
RD1_Amm_S <- S(RD1_Amm_m, RD1_Amm_s)
RD1_Amm_n <- length(RD1_Amm[!is.na(RD1_Amm)])
RD1_Amm_w <- 0.9580
RD1_Amm_qL <- qL(RD1_Amm_M, RD1_Amm_S, RD1_Amm_w)



# Orth
RD1_Orth <- Chem_RD1$`6392 OrthoPhosph mg/l`
RD1_Orth_m <- mean(RD1_Orth, na.rm = TRUE )
RD1_Orth_s <- sd(RD1_Orth, na.rm = TRUE)
RD1_Orth_n <- length(RD1_Orth[!is.na(RD1_Orth)])
RD1_Orth_se <- se(RD1_Orth_s, RD1_Orth_n)
RD1_Orth_w <- 1.96
RD1_Orth_LCI <- LCI(RD1_Orth_m, RD1_Orth_se, RD1_Orth_w)*1000


# pH
RD1_pH <- Chem_RD1$`0061 pH PHUNITS`
RD1_pH_m <- mean(RD1_pH, na.rm = TRUE )
RD1_pH_s <- sd(RD1_pH, na.rm = TRUE)
RD1_pH_n <- length(RD1_pH[!is.na(RD1_pH)])
RD1_pH_w <- 1.2846
RD1_pH_CI <- CI(RD1_pH_m,RD1_pH_s,RD1_pH_w)



# DO
RD1_DO <- Chem_RD1$`9901 O Diss% satn %`
RD1_DO_m <- mean(RD1_DO, na.rm = TRUE )
RD1_DO_s <- sd(RD1_DO, na.rm = TRUE)
RD1_DO_n <- length(RD1_DO[!is.na(RD1_DO)])
RD1_DO_w <- 0.8011
RD1_DO_LCI <- LCI(RD1_DO_m, RD1_DO_s, RD1_DO_w)



# RD2

# BOD
RD2_BOD <- Chem_RD2$`0085 BOD ATU mg/l`
RD2_BOD_m <- mean(RD2_BOD, na.rm = TRUE )
RD2_BOD_s <- sd(RD2_BOD, na.rm = TRUE)
RD2_BOD_M <- M(RD2_BOD_m, RD2_BOD_s)
RD2_BOD_S <- S(RD2_BOD_m, RD2_BOD_s)
RD2_BOD_n <- length(RD2_BOD[!is.na(RD2_BOD)])
RD2_BOD_w <- 0.8477
RD2_BOD_qL <- qL(RD2_BOD_M, RD2_BOD_S, RD2_BOD_w)


# Amm
RD2_Amm <- Chem_RD2$`0111 Ammonia - N mg/l`
RD2_Amm_m <- mean(RD2_Amm, na.rm = TRUE )
RD2_Amm_s <- sd(RD2_Amm, na.rm = TRUE)
RD2_Amm_M <- M(RD2_Amm_m, RD2_Amm_s)
RD2_Amm_S <- S(RD2_Amm_m, RD2_Amm_s)
RD2_Amm_n <- length(RD2_Amm[!is.na(RD2_Amm)])
RD2_Amm_w <- 0.9618
RD2_Amm_qL <- qL(RD2_Amm_M, RD2_Amm_S, RD2_Amm_w)


# Orth
RD2_Orth <- Chem_RD2$`6392 OrthoPhosph mg/l`
RD2_Orth_m <- mean(RD2_Orth, na.rm = TRUE )
RD2_Orth_s <- sd(RD2_Orth, na.rm = TRUE)
RD2_Orth_n <- length(RD2_Orth[!is.na(RD2_Orth)])
RD2_Orth_se <- se(RD2_Orth_s, RD2_Orth_n)
RD2_Orth_w <- 1.96
RD2_Orth_LCI <- LCI(RD2_Orth_m, RD2_Orth_se, RD2_Orth_w)*1000


# pH
RD2_pH <- Chem_RD2$`0061 pH PHUNITS`
RD2_pH_m <- mean(RD2_pH, na.rm = TRUE )
RD2_pH_s <- sd(RD2_pH, na.rm = TRUE)
RD2_pH_n <- length(RD2_pH[!is.na(RD2_pH)])
RD2_pH_w <- 1.2846
RD2_pH_CI <- CI(RD2_pH_m,RD2_pH_s,RD2_pH_w)


# DO
RD2_DO <- Chem_RD2$`9901 O Diss% satn %`
RD2_DO_m <- mean(RD2_DO, na.rm = TRUE )
RD2_DO_s <- sd(RD2_DO, na.rm = TRUE)
RD2_DO_n <- length(RD2_DO[!is.na(RD2_DO)])
RD2_DO_w <- 0.9456
RD2_DO_LCI <- LCI(RD2_DO_m, RD2_DO_s, RD2_DO_w)



# PB1 

# BOD
PB1_BOD <- Chem_PB1$`0085 BOD ATU mg/l`
PB1_BOD_m <- mean(PB1_BOD, na.rm = TRUE )
PB1_BOD_s <- sd(PB1_BOD, na.rm = TRUE)
PB1_BOD_M <- M(PB1_BOD_m, PB1_BOD_s)
PB1_BOD_S <- S(PB1_BOD_m, PB1_BOD_s)
PB1_BOD_n <- length(PB1_BOD[!is.na(PB1_BOD)])
PB1_BOD_w <- 0.9499
PB1_BOD_qL <- qL(PB1_BOD_M, PB1_BOD_S, PB1_BOD_w)



# Amm
PB1_Amm <- Chem_PB1$`0111 Ammonia - N mg/l`
PB1_Amm_m <- mean(PB1_Amm, na.rm = TRUE )
PB1_Amm_s <- sd(PB1_Amm, na.rm = TRUE)
PB1_Amm_M <- M(PB1_Amm_m, PB1_Amm_s)
PB1_Amm_S <- S(PB1_Amm_m, PB1_Amm_s)
PB1_Amm_n <- length(PB1_Amm[!is.na(PB1_Amm)])
PB1_Amm_w <- 0.9499
PB1_Amm_qL <- qL(PB1_Amm_M, PB1_Amm_S, PB1_Amm_w)


# Orth
PB1_Orth <- Chem_PB1$`6392 OrthoPhosph mg/l`
PB1_Orth_m <- mean(PB1_Orth, na.rm = TRUE )
PB1_Orth_s <- sd(PB1_Orth, na.rm = TRUE)
PB1_Orth_n <- length(PB1_Orth[!is.na(PB1_Orth)])
PB1_Orth_se <- se(PB1_Orth_s, PB1_Orth_n)
PB1_Orth_w <- 1.96
PB1_Orth_LCI <- LCI(PB1_Orth_m, PB1_Orth_se, PB1_Orth_w)*1000


# pH
PB1_pH <- Chem_PB1$`0061 pH PHUNITS`
PB1_pH_m <- mean(PB1_pH, na.rm = TRUE )
PB1_pH_s <- sd(PB1_pH, na.rm = TRUE)
PB1_pH_n <- length(PB1_pH[!is.na(PB1_pH)])
PB1_pH_w <- 1.2756
PB1_pH_CI <- CI(PB1_pH_m,PB1_pH_s,PB1_pH_w)


# DO
PB1_DO <- Chem_PB1$`9901 O Diss% satn %`
PB1_DO_m <- mean(PB1_DO, na.rm = TRUE )
PB1_DO_s <- sd(PB1_DO, na.rm = TRUE)
PB1_DO_n <- length(PB1_DO[!is.na(PB1_DO)])
PB1_DO_w <- 0.7866
PB1_DO_LCI <- LCI(PB1_DO_m, PB1_DO_s, PB1_DO_w)




# Graph
x <- seq(0, 4, by=0.001)
std <- 0.75*x
logx <- log(x/(sqrt(1+(std^2/x^2))))
logstd <- sqrt(log(1+(std^2/x^2)))
q90 <- exp(logx+(1.6*logstd))

y <- (41*0.460836396+13*2.327818814+17.25*q90)/(41+13+17.25)

plot(x,y, xlab = "HSTW Mean Ammonia Consent (mg/L)", ylab = "RD2 q90 Concentration (mg/L)", type="l", ylim = c(0,3.5), main = "Conservative Dilution Model [NH3]", xaxt="none")
axis(1, seq(0,4, by = 0.5))
abline(a=2.5, b=0, lty = "longdash")
abline(a=1.1, b=0, lty = "longdash")
abline(a=0.6, b=0, lty = "longdash")
abline(a=0.3, b=0, lty = "longdash")


# Graph 2
RD1f_mean <- 41
RD1f_q95 <- 14
RD1f_sd <- (RD1f_mean-RD1f_q95)/2
RD1f_meanlog <- log(RD1f_mean/(sqrt(1+(RD1f_sd^2/RD1f_mean^2))))
RD1f_sdlog <- sqrt(log(1+(RD1f_sd^2/RD1f_mean^2)))

RD1c_mean <- 0.197810810810811
RD1c_sd <- RD1c_mean*0.75
RD1c_meanlog <- log(RD1c_mean/(sqrt(1+(RD1c_sd^2/RD1c_mean^2))))
RD1c_sdlog <- sqrt(log(1+(RD1c_sd^2/RD1c_mean^2)))


PB1f_mean <- 13
PB1f_q95 <- 1.2
PB1f_sd <- (PB1f_mean-PB1f_q95)/2
PB1f_meanlog <- log(PB1f_mean/(sqrt(1+(PB1f_sd^2/PB1f_mean^2))))
PB1f_sdlog <- sqrt(log(1+(PB1f_sd^2/PB1f_mean^2)))

PB1c_mean <- 0.9992
PB1c_sd <- PB1c_mean*0.75
PB1c_meanlog <- log(PB1c_mean/(sqrt(1+(PB1c_sd^2/PB1c_mean^2))))
PB1c_sdlog <- sqrt(log(1+(PB1c_sd^2/PB1c_mean^2)))


STWf_mean <- 11.5*1.5
STWf_sd <- STWf_mean*0.75
STWf_meanlog <- log(STWf_mean/(sqrt(1+(STWf_sd^2/STWf_mean^2))))
STWf_sdlog <- sqrt(log(1+(STWf_sd^2/STWf_mean^2)))



n = 1000
RD1f_dist <- exp(rnorm(n,RD1f_meanlog,RD1f_sdlog))
RD1c_dist <- exp(rnorm(n,RD1c_meanlog,RD1c_sdlog))

PB1f_dist <- exp(rnorm(n,PB1f_meanlog,PB1f_sdlog))
PB1c_dist <- exp(rnorm(n,PB1c_meanlog,PB1c_sdlog))

STWf_dist <- exp(rnorm(n,STWf_meanlog,STWf_sdlog))

# 2.96
STWc296_mean <- 2.96
STWc296_sd <- STWc296_mean*0.75
STWc296_meanlog <- log(STWc296_mean/(sqrt(1+(STWc296_sd^2/STWc296_mean^2))))
STWc296_sdlog <- sqrt(log(1+(STWc296_sd^2/STWc296_mean^2)))
STWc296_q95con <- exp(STWc296_meanlog+2*STWc296_sdlog)

STWc296_dist <- exp(rnorm(n,STWc296_meanlog,STWc296_sdlog))

RD2c296_dist <- (RD1f_dist*RD1c_dist+PB1f_dist*PB1c_dist+STWf_dist*STWc296_dist)/(RD1f_dist+PB1f_dist+STWf_dist)

h<-hist(RD2c296_dist, breaks=20, xlab="Concentration",
        main="Histogram with Normal Curve")
RD2c296_distfit<-seq(0,6,length=50)
RD2c296_distyfit<-dnorm(RD2c296_distfit,mean=mean(RD2c296_dist),sd=sd(RD2c296_dist))
RD2c296_distyfit <- RD2c296_distyfit*diff(h$mids[1:2])*length(RD2c296_dist)
lines(RD2c296_distfit, RD2c296_distyfit, col="blue", lwd=1)


# 1.48
STWc148_mean <- 1.48
STWc148_sd <- STWc148_mean*0.75
STWc148_meanlog <- log(STWc148_mean/(sqrt(1+(STWc148_sd^2/STWc148_mean^2))))
STWc148_sdlog <- sqrt(log(1+(STWc148_sd^2/STWc148_mean^2)))
STWc148_q95con <- exp(STWc148_meanlog+2*STWc148_sdlog)

STWc148_dist <- exp(rnorm(n,STWc148_meanlog,STWc148_sdlog))

RD2c148_dist <- (RD1f_dist*RD1c_dist+PB1f_dist*PB1c_dist+STWf_dist*STWc148_dist)/(RD1f_dist+PB1f_dist+STWf_dist)

h<-hist(RD2c148_dist, breaks=20, xlab="Concentration",
        main="Histogram with Normal Curve")
RD2c148_distfit<-seq(0,6,length=50)
RD2c148_distyfit<-dnorm(RD2c148_distfit,mean=mean(RD2c148_dist),sd=sd(RD2c148_dist))
RD2c148_distyfit <- RD2c148_distyfit*diff(h$mids[1:2])*length(RD2c148_dist)
lines(RD2c148_distfit, RD2c148_distyfit, col="red", lwd=1)


# 0.74
STWc074_mean <- 0.74
STWc074_sd <- STWc074_mean*0.75
STWc074_meanlog <- log(STWc074_mean/(sqrt(1+(STWc074_sd^2/STWc074_mean^2))))
STWc074_sdlog <- sqrt(log(1+(STWc074_sd^2/STWc074_mean^2)))
STWc074_q95con <- exp(STWc074_meanlog+2*STWc074_sdlog)

STWc074_dist <- exp(rnorm(n,STWc074_meanlog,STWc074_sdlog))

RD2c074_dist <- (RD1f_dist*RD1c_dist+PB1f_dist*PB1c_dist+STWf_dist*STWc074_dist)/(RD1f_dist+PB1f_dist+STWf_dist)

h<-hist(RD2c074_dist, breaks=20, xlab="Concentration",
        main="Histogram with Normal Curve")
RD2c074_distfit<-seq(0,6,length=50)
RD2c074_distyfit<-dnorm(RD2c074_distfit,mean=mean(RD2c074_dist),sd=sd(RD2c074_dist))
RD2c074_distyfit <- RD2c074_distyfit*diff(h$mids[1:2])*length(RD2c074_dist)
lines(RD2c074_distfit, RD2c074_distyfit, col="green", lwd=1)


# 0.296
STWc0296_mean <- 0.296
STWc0296_sd <- STWc0296_mean*0.75
STWc0296_meanlog <- log(STWc0296_mean/(sqrt(1+(STWc0296_sd^2/STWc0296_mean^2))))
STWc0296_sdlog <- sqrt(log(1+(STWc0296_sd^2/STWc0296_mean^2)))
STWc0296_q95con <- exp(STWc0296_meanlog+2*STWc0296_sdlog)

STWc0296_dist <- exp(rnorm(n,STWc0296_meanlog,STWc0296_sdlog))

RD2c0296_dist <- (RD1f_dist*RD1c_dist+PB1f_dist*PB1c_dist+STWf_dist*STWc0296_dist)/(RD1f_dist+PB1f_dist+STWf_dist)

h<-hist(RD2c0296_dist, breaks=20, xlab="Concentration",
        main="Histogram with Normal Curve")
RD2c0296_distfit<-seq(0,6,length=50)
RD2c0296_distyfit<-dnorm(RD2c0296_distfit,mean=mean(RD2c0296_dist),sd=sd(RD2c0296_dist))
RD2c0296_distyfit <- RD2c0296_distyfit*diff(h$mids[1:2])*length(RD2c0296_dist)
lines(RD2c0296_distfit, RD2c0296_distyfit, lwd=1)



plot(RD2c296_distfit, RD2c296_distyfit, col="red", lwd=1, type="l", ylim=c(0,max(RD2c296_distyfit,RD2c148_distyfit,RD2c074_distyfit)), xlim=c(0,4), ylab = "Frequency", xlab = "RD2 NH3 Concentration (mg/L)", main = "Combined Distribution Model (1000 trials)")
lines(RD2c148_distfit, RD2c148_distyfit, col="orange", lwd=1)
lines(RD2c074_distfit, RD2c074_distyfit, col="blue", lwd=1)
lines(RD2c0296_distfit, RD2c0296_distyfit, col="green", lwd=1)
legend(3, 200, legend=c("Year 1", "Year 2", "Year 3", "Year 4"),col=c("red", "orange", "blue", "green"), lty=1, cex=0.8)
