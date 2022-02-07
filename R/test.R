library(MASS)
set.seed(101)
my_data <- rnorm(250, mean=1, sd=0.45)
my_doto <- rnorm(250, mean=1, sd= 3)

fit <- fitdistr(my_data, densfun="log-normal") # we assume my_data ~ log-Normal
fit

coef(fit)

#######
#Donne des résultats différents
med <- mean(my_data)-sd(my_data)*sd(my_data)/2
med
median(my_data)
#######

#######
mode <- mean(my_data)-3*sd(my_data)*sd(my_data)/2
mode
mode(my_data)
#######

mean <- mean(my_data)
sd <-  sd(my_data)

ecdfhb <- exp(mean + sd)
ecdflb <- exp(mean - sd)

mytest = function(x){
  mean_x <- mean(x, na.rm=TRUE)
  (mean((x-mean_x)^4)/(sd(x, na.rm = TRUE)^4)) - 3 #return?
}
mytest(my_data)

vec <- c(1:40)
list <- list(vec)

lower <- c(-1, 0.02, 2)
upper <- c(1, 40, 50)
starting_list <- list(m = mean(my_data), s = sd(my_data), df = 3)
Param <- fitdistr(my_data, "t",
                  start = starting_list, 
                  lower = lower,
                  upper = upper)

tm <- coef(Param)[1]
ts <- coef(Param)[2]
tdf<- coef(Param)[3]

count <- 12
c <- c(1/(2*count), 1)
c

x <- my_data+1
plot(my_data)
points(x, pch = 20, col="red")

Format = function(number) { #to make stardardized output
  sprintf("%3.6f", number)
}
Format(0.37419)

#lines(my_data,my_data+0.5, lwd=2, col="green")

my_neg <- my_data[my_data < 0]
my_neg

c <- as.matrix(c("delta", "truc", "chose"), ncol = nrow, col = NULL)
for (i in 1:length(c)) {
  c[i] <- NULL
}
c <- NULL



  
max_eta-min_eta <- 2
min_eta <- 1
DELTA <- 2
deltaeta_tstudent <- NULL
cfdt <- NULL
tdist <- NULL
cfdt[1] <- 3
tdist[1] <- cfdt[1]
count <- 100

for(i in 2:count){
  deltaeta_tstudent[i] <- min_eta+(i-1)*DELTA/count
}

cfdt <- cfdt/cfdt[count]
deltaeta_tstudent_neg <- abs(deltaeta_tstudent[deltaeta_tstudent<0])
negcdft <- cfdt[deltaeta_tstudent<0]

count_pos <- 4
a <- rep(NA,(count_pos + count + count))

count_neg <- 12
xpos_exp <- 3
b <- c(rep(NA, (count_neg)), xpos_exp, rep(NA, 2*count))


All <- 3
scaled <- 0
data <- 2
c <- c(All, scaled, data)

c <- NULL



All <- 3
scaled <- 0
data <- 2
c <- list(c(All, scaled, data))
#c <- as.vector(c)

for (i in 2:length(c)) {
  c[i] <- NULL
}

plotfile <- FALSE
if(plotfile) {
  plot_index <- list()
  for (i in 1:(length(films)*2)) {
    plot_index[i] = NULL
  }
}

#plot_index <- rep(NA, 5)
plot_index <- list()
for (i in 1:length(plot_index)) {
  plot_index[i] = NULL
}
plot_index <- NULL


films <- c("film10", "film15", "film21")
for (i in 1:length(films)) {
  file <- paste(films[i], "plot_correct_scaling.txt", sep = "")
  correct_scaling <- read.table(file = file, header = TRUE)
}

cdf <- matrix(NA, ncol=3, nrow=6)
cdf[,1] <-  1
cdf[,2] <- 2
cdf[,3] <- 3
cdf_neg <- -cdf[,1]
#all_scaled_datax <- NULL
all_scaled_datax <- cdf_neg
plot_datax <- cdf[,1]


plot_index <-  list()
nr <- 2*i-1
for (i in (nr+1):8) {
  plot_index[[i]] <- c(plot_index[[i]], rep(NA, length(cdf_neg)))
}


x <- 1
mu <- 3
sigma <- 5
nu <- 0
t_general <- function(x, mu, sigma, nu) {
  1/sigma*gamma((nu+1)/2)/sqrt(nu*pi)/
    gamma(nu/2)*(1+((x-mu)^2)/nu/sigma/sigma)^(-(1+nu)/2)
}

plot(my_data, my_doto)
lines(my_data, my_doto, lwd=2,col='red')


pch1 <- c(1, 1, 1, 1, 1, 1)
pch <- rep(1, 6)
col=c("red","black","red","black","red","black")


string <-  "truc"
string <- "machin"

alfa <- rep(NA, 4)
alfa[1] <- 0.5
alfa[2] <- 0.7
alfa[3] <- 0.9
alfa[4] <- 0.99

a <- 0
for (ialfa in 1:4) {
  a <- a+1
}


x <- NULL
x[1] <- 12
for (i in 2:20) {
  x[i] <- i^2
}

eta <- c(0.2:19)
eta_sampled <- sample(eta)
for(i in 1:3) {
  eta_for <- sample(eta)
}

eta <- seq(12, 5, -0.32)
lower_vis <- min(eta)
upper_vis <- max(eta)
eta_uncleaned <- eta
eta_NA <- eta
eta_NA[log(eta)<=lower_vis | log(eta)>=upper_vis] <- NA
eta_cleaned <- eta_NA

eta_NA <- rep(NA, length(eta))


t <- t[log(eta) > lower_vis & log(eta) < upper_vis]
eta <- eta[log(eta) > lower_vis & log(eta) < upper_vis]

Film_ID <- c(1:length(eta))

testOK <- "OK"
test <- testOK

film2 <- "truc"
film <- NA
filacf <- c(film, film2)

l <- 2
alpha <- 4
sigma <- 3
truc <- expression(paste(gamma(l)(1-alpha^2)/(2*sigma^2)))


film_number <- 12
legend_markers <- NULL
legend_text <- NULL
Film_frequencies <- seq(1, 7, by = 0.2)
legend_colors <- c("red", "blue", "green", "orange", "salmon", "magenta", "purple", "brown", 
                   "olivedrab", "cyan", "sea green", "navy", "violet", "grey")
for (i in 1:film_number) {
  legend_markers=c(legend_markers,i)
  legend_text <- c(legend_text, round(Film_frequencies[i], 3))
  legend_colors <- legend_colors[1:film_number]
}

legend_markers <- seq(1, film_number, by=1)
legend_text <- round(Film_frequencies[1:film_number], 3)

x <- (1:9999)/10000
x=(1:10000)/100

Film_alfa <- seq(-5, 5, 1)
Film_alfa[Film_alfa>1]=NA

Film_epsilon <- seq(-5, 5, 1)
Film_epsilon[Film_epsilon<=0]=NA

c <- c(rep("red", 3), "black")


fit <- 3
if (fit == 3) {
  add = function(x) {
    1+x
  }
}

char <- "run "
if (char == "run ") {
  x <- 1 + 1
  x
  }


x <- "blablablalbal"
x <- "lalala"

test3 <- 3
test4 <- 4
test <- 3
if (test == 3) {
  print("ok")
}

x <- rnorm(100, 2, 3)
plot(ecdf(x))

number <- 4
Format=function(number){sprintf("%3.6f",number)}
Format(4)


path <- "/Users/rgoulanc/Desktop/"
setwd(path)
getwd()


Modelhole_all(FIT)

variogramfit <- nlsLM(fitvariogram ~ Modelhole(fitlag, C0_constant_cosinus, C1_T, C2_constant_GRW, C3_GRW),
                      start = list(C0_constant_cosinus = C0s, C1_T = C1s, 
                                   C2_constant_GRW = C2s, C3_GRW = C3s),
                      lower = c(C0l,C1l,C2l,C3l),
                      upper = c(C0m,C1m,C2m,C3m))


Modelhole_all = function(FIT) {
  #Modelhole = function()
  if (FIT == FIT0) {
    Modelhole = function(fitlag, C0_constant_cosinus, C1_T, C2_constant_GRW, C3_GRW) {
      C0_constant_cosinus*(1-cos(2*pi*averagetime*fitlag/C1_T)) +
        2*C2_constant_GRW*(1-C3_GRW^fitlag)/(1-C3_GRW^2)
    }
  }
  else if (FIT == FIT1) {
    Modelhole = function(fitlag, C0_constant_cosinus, C1_T, C2_constant_OU, C3_correlationtime) {
      C0_constant_cosinus*(1-cos(2*pi*averagetime*fitlag/C1_T)) + 
        C2_constant_OU*(C3_correlationtime/averagetime)*(1-exp(-fitlag*averagetime/C3_correlationtime))
    }
  }
  else if (FIT == FIT2) {
    Modelhole = function(fitlag, C0_constant_GRW, C1_GRW) {
      2*C0_constant_GRW*(1-C1_GRW^fitlag)/(1-C1_GRW^2) 
    }
  }
  else if (FIT == FIT3) {
    Modelhole = function(fitlag, C0_constant_OUP, C1_correlationtime) {
      C0_constant_OUP*(C1_correlationtime/averagetime)*(1-exp(-fitlag*averagetime/C1_correlationtime))
    }
  }
  else if (FIT == FIT4) {
    Modelhole = function(fitlag, C0, C1, C2) {
      C0+C1*(1-cos(C2*fitlag))
    }
  }
  else if (FIT == FIT5) {
    Modelhole = function(fitlag,C0) {
      C0 + 0*fitlag
    }
  }
}

alldatafiles_test1 <- list.files(path = ID_path, ".txt", full.names = TRUE)
alldatafiles_test1


path_Input <- "/Users/rgoulanc/desktop/Series/InputData"
setwd(path_Input)
alldatafiles_test2 <- NULL
file_number <- readline(prompt = "How many files do you want to read? ")
for (i in 1:file_number) {
alldatafiles_test2[i] <- file.choose(new=FALSE)
}

x <- 0
b <- 4
exemple = function(x) {
  if(x == 0) {
    a = function(b) {
      c <- 3*b
    }
  }
  else if (x == 2) {
    a <- 4
  }
}
exemple(2)
exemple(0)
a(b)


path_generatedFiles <- "/Users/rgoulanc/desktop/Series/OutputAll/GeneratedFiles/"
alldatafiles <- list.files(path = ID_path, ".txt", full.names = FALSE)
alldatafiles
Fichier <- list.files(path_generatedFiles, pattern = "Scalingdeltaeta.xlsx", full.names = FALSE)
Fichier

setwd(path_generatedFiles)
File <- NULL
file_numbertest <- length(alldatafiles)
for (i in 1:file_numbertest) {
  File[i] <- file.choose(new = FALSE)
}


alldatafiles <- NULL
file_number <- readline(prompt = "How many files do you want to read? (at least 5) ")
for (i in 1:file_number) {
  alldatafiles[i] <- file.choose(new = FALSE)
}
alldatafiles <- list.files

path_film1 <- "/Users/rgoulanc/desktop/Series/InputData/wC&eta(t) film1.txt"
film1 <- read.table(path_film1, header = TRUE, sep = "\t", dec = ".")
film1

film <- NULL
allfilms <- list.files(path = path_Input, ".txt", full.names = TRUE)
for (i in 1:length(allfilms)) {
  film <- read.table(allfilms[i], header = TRUE, sep = "\t", dec = ".")
}

library(dplyr)
film1 <- film1 %>% relocate(viscosity..Pa.s., viscosity.average..Pas.)
View(film1)

a <- b <- rnorm(n = 1000, mean = 0, sd = 0.5)


setwd(path_generatedFiles)
File <-  c("film1Scalingdeltaeta.xlsx", 
           "film8Scalingdeltaeta.xlsx",
           "film9Scalingdeltaeta.xlsx",
           "film10Scalingdeltaeta.xlsx")
File <- NULL
File <- list.files(path_generatedFiles, pattern = "Scalingdeltaeta.xlsx", full.names = FALSE)

File <- NULL
for (i in 1:file_number) {
  File[i] <- paste0("film", file_number, "Scalingdeltaeta.xlsx")
}

library(tidyverse)
filmNumber <-  str_extract("film10Scalingdeltaeta.xlsx", "\\d+")
filmNumber
filmNumber <- NULL

File <- NULL
for(i in 1:file_number) {
  filmNumber <- str_extract(alldatafiles[i], "\\d+")
  File[i] <- paste0("film", filmNumber, "Scalingdeltaeta.xlsx")
}
alldatafiles <- NULL
file_number <- readline(prompt = "How many files do you want to read? (at least 5) ")
for (i in 1:file_number) {
  alldatafiles[i] <- file.choose(new = FALSE)
  #alldatafiles[i] <- 
}


test <- c("a", "b")

catt = function() {
  cat("ok")
}
catt()

File <- NULL
for(i in 1:file_number) {
  filmNumber <- str_extract(alldatafiles[i], "\\d+")
  File[i] <- paste0("film", filmNumber, "Scalingdeltaeta.xlsx")
}


vec <- NULL
for (i in 1:nrow(df)) {
    vec[i] <- row(df[i])
  }
vec <- unlist(vec)

vec <- NULL
for (i in 1:nrow(df)) {
  vec <- df[i,]
}

line_sampled <- sample(nrow(df), nrow(df), replace = FALSE)
#for (i in 1:length(line_sampled)) {
#  if (line_sampled[i] == nrow(df[i,])) {
#    df_sampled <- row(df[i,])
#  }
#}

df_ <- cbind(line_sampled, df)
rownames(df_) <- df_$line_sampled
df_sampled <- df_[,2:3]
df_sampled_f <- sort(rownames(df_sampled), decreasing = FALSE)

#sum <- rep(0, nrow(delta_t))
sum <- NULL
for (i in 1:nrow(delta_t)) {  
  for (j in 2:nrow(delta_t)) {
    sum[i] <- delta_t[i,] + delta_t[j,]
  }
}

sum <- NULL
for (i in 1:nrow(delta_t)) {
  sum[i] <- sum(delta_t[1:i,])
}

line_sampled <- sample(row(delta_df), nrow(df), replace = FALSE) ## prendre la matrice d'avant avec colonne line_sampled?



alldatafiles <- NULL
file_number <- readline(prompt = "How many files do you want to read? (at least 5) ")
for (i in 1:file_number) {
  alldatafiles[i] <- file.choose(new = FALSE)
}

file_number <- length(alldatafiles)
Film_array <- NULL
for(i in 1:file_number) {
  filmNumber <- str_extract(alldatafiles[i], "\\d+")
  Film_array[i] <- paste0("film", filmNumber, "Scalingdeltaeta.xlsx")
}


Plots <- NULL
x <- 1:100
for (i in 1:5) {
  y <- rnorm(100, 2, 0.9)
  Plots[i] <- list(pdf(file = paste0(x, "~", y, plot(x, y, type = "l"))))
}

b <- rnorm(100, 50, 2)
plot <- pdf(file = "plot", plot(x, b, type = "l"))
plot
View(Plots)

setwd(path_Screenplots)
x <- 1:100
b <- rnorm(100, 50, 2)
plot(x, b, type = "l")
dev.print(device = pdf, file = "figtest2.pdf", width = 5)



setwd(path_Input)
x <- 1:100
b <- rnorm(100, 50, 2)
y <-  seq(1, 50.5, 0.5)
x <- 1:100
ploting = function() {
  cat("plots")
  plot(x, y)
  lines(x, x+2)
}
pdf(file = "les_plots.pdf", onefile = TRUE)
for (p in 1:4) {
  setwd(path_Input)
  plot(x, b, type = "l", main = p)
}
ploting()
cat("end", getwd())
dev.off()


file_to_copy <- list.files(path_generatedFiles, pattern = "Scalingdeltaeta.xlsx", full.names = FALSE)
file.copy(file.path(path_generatedFiles, file_to_copy), path_Scaling, overwrite = TRUE)


final_data <- NULL
for (i in 1:nrow(line_sampled)) {
  for (j in 1:nrow(delta_df)) {
    if (line_sampled[i,1] == rownames(delta_df[j,])) {
      row <- delta_df[j,]
      final_data <- rbind(final_data, row)
    }
  }
}
plot(final_data$delta_time, final_data$viscosity, type = "l")


############################JF DATA
 library(stringr)

f <- unlist(str_extract_all(alldata[i], "\\d+"))[2]
f <- as.numeric(f)
period <- 1/f
cat("La période d'une oscillation est de : ", period)

osc <- seq(10, 15, period)
length(osc)

start_osc <- 10
end_osc <- 15
psirad_value <- NULL
nrow <- 0
for (i in 1:nrow(data)) {
  if (start_osc <= data$time.s[i] & data$time.s[i] <= end_osc) {
    nrow <-  nrow + 1
    psirad_value[i] <- data$psi.t..rad[i]
  } 
} 
psirad_value <- na.omit(psirad_value)
psimaxR <- max(psirad_value)
cat("number of value of psi(t) (on 1 oscillation) : ", nrow, "\n", "maximum of the oscillation : ", psimaxR)


f <- unlist(str_extract_all(alldata[i], "\\d+"))[2]
f <- as.numeric(f)*10^-3
p <- 1/f
period <- 1/(f)

a <- 1
b <- 3
c <- 2
if (a <= c | c <= b) {
  cat("oeoeoe\n")
}


psirad_i <- data$psi.t..rad[1]
start <- 2
for (i in start:nrow(data)) {
  if (data$psi.t..rad[i] >= psirad_i[i-1]) {
    psirad_i[i] <- data$psi.t..rad[i-1]
    start <- which(data$psi.t..rad == psirad_i[i])
  }
  else {
    
  }
}
psirad_i <- na.omit(psirad_i)


max <- NULL
ligne <- NULL
for (i in 2:nrow(data)) {
  if (i == nrow(data)) { break }
  if ((data$psi.t..rad[i] > data$psi.t..rad[i-1]) & (data$psi.t..rad[i] > data$psi.t..rad[i+1])) {
    max <- rbind(max, as.data.frame(data[i, 1:2]))
  }
}
max <- na.omit(max)

diff_max <- diff(max$psi.t..rad)
mean_diff_max_i <- mean(diff_max)

box <- boxplot(diff_max)
outlier <- box$out

diff_max_int <- diff_max[!diff_max %in% box$out]
mean_diff_max_int <- mean(diff_max_int)
box2 <- boxplot(diff_max_int)
#x[!x %in% boxplot.stats(x)$out]

vec <- NULL
start <- 1
for (i in start:nrow(data)) {
  vec[i] <- data$psi.t..rad[i]
  if (i >= nb) {
    start <- i
    nb <- nb*2
  }
}

maximum <- NULL
nb <- nrow(data)/nrow(max)
vec <- NULL
for (i in 1:nrow(data)-(nb-1)) {
  max[i] <- unlist(max(data$psi.t..rad[i:i+nb]))
} 
max <- na.omit(max)

for (i in 1:nrow(data)) {
  
  max[i] <- max(data$psi.t..rad[:nb])
}



library(stringr)
Maximum_outlier = function {
  
}

Maximum_period = function() {
  w <- unlist(str_extract_all(alldata[3], "\\d+"))[2]
  w <- as.numeric(w)*10^-3   # mHz -> Hz
  period <- (pi/w)/2 #?
  cat("The period of one oscillation is : ", period, "\n")
}

plot(data$time.s, data$psi.t..rad, main = "angular frequency vs time", sub = alldata[3],
     xlab = "time (s)",
     ylab = "angular frequency (rad)",
     type = "p",
     xlim = c(60,110))
lines(data$time.s, data$psi.t..rad)

plot(diff_max)
lines(diff_max)

Maximum_tdt = function {
  
}

plot(data$time.s, data$psi.t..rad, main = "angular frequency vs time", sub = alldata[i],
     xlab = "time (s)",
     ylab = "angular frequency (rad)",
     type = "p",
     xlim = c(10, 20))
lines(data$time.s, data$psi.t..rad, col = "red")



#nrowdata en fonction du temps d'une oscillation
t <- 1/w
point <- nrow(data)/
Maximum_nb = function {
  point <- nrow(data)/
}

Maximum_drop = function {
  
}

Maximum_chg = function {
  chg <- NULL # changements relatifs
  for (i in 1:nrow(data)) {
    if (i == nrow(data)) { break }
    chg[i] <- (data$psi.t..rad[i+1]-data$psi.t..rad[i])/data$psi.t..rad[i]
  }
  plot(chg, ylim = c(-0.2, 0.05), xlim = c(500, 600))
  lines(chg)
  abline(h = 0, col = "blue") # on prends i-2
  #si dessus alors courbe monte
  #drop = drop de valeur dans la courbe donc un max juste avant
  chg0 <- which(chg == 0)
  chg_pos <- which(chg > 0)
  chg_neg <- which(chg < 0)
  
  diff_mean <- mean(diff(chg))
  x <- diff_mean - 4/100
  abline(h = x, col = " green")
  
  max_chg <- NULL
  for (i in 2:length(chg)) {
    if (i == length(chg)) { 
      break
    }
    if (chg[i] < chg[i-1] & chg[i] < 0 & chg[i] <= chg[i-1] + x) {
      if (i > 2) {
        max_chg[i] <- data$psi.t..rad[i-1]
      }
    }
  }
  max_chg <- as.data.frame(na.omit(max_chg))
  colnames(max_chg)[1] <-  "max"
}


chg <- NULL # changements relatifs
for (i in 1:nrow(data)) {
  if (i == nrow(data)) { break }
  chg[i] <- (data$psi.t..rad[i+1]-data$psi.t..rad[i])/data$psi.t..rad[i]
}
plot(chg, type = "p" ylim = c(-0.2, 0.05), xlim = c(0, 500))
lines(chg)
#setwd("/Users/rgoulanc/Desktop/Rebecca/FAC/M1BI-IPFB/SEM2/Stage/BERRET_BOSTOEN/R/NewData/plots")
abline(h = 0, col = "blue") # on prends i-2
#dev.print(device = pdf, file = "chg0_500.pdf", width = 300)
#si dessus alors courbe monte
#drop = drop de valeur dans la courbe donc un max juste avant
chg0 <- which(chg == 0)
chg_pos <- which(chg > 0)
chg_neg <- which(chg < 0)

diff_mean <- mean(diff(chg))
x <- diff_mean - 4/100
abline(h = x, col = " green")

max_chg <- NULL
for (i in 2:length(chg)) {
  if (i == length(chg)) { 
    break
    }
  if (chg[i] < chg[i-1] & chg[i] < 0 & chg[i+1] <= chg[i] + chg[i]*30/100) {
    if (i > 2) {
    max_chg[i] <- data$psi.t..rad[i-2]
    }
  }
}
max_chg <- as.data.frame(na.omit(max_chg))
colnames(max_chg)[1] <-  "max"

time <- NULL
for (i in 1:nrow(max_chg)) {
  for (j in 1:nrow(data)) {
    if (max_chg$max[i] == data$psi.t..rad[j]) {
      time[i] <-data$time.s[j]
    }
    #if (i == nrow(max_chg)) { break }
  }
}
time <- as.data.frame(time)
max_dat <- cbind(time, max_chg)


reduc <- diff_mean - (diff_mean*5/100)
reduc <- (diff_mean*5/100)
x <- diff_mean - 5/100

diff_mean

chg[1] + x


max_drop <- NULL
for (i in 2:nrow(data)) {
  if (i >= nrow(data)) { break }
  if (data$psi.t..rad[i+1] <= data$psi.t..rad[i] - data$psi.t..rad[i]*4/100) {
    max_drop[i] <- data$psi.t..rad[i-1]
    #eval.parent(substitute(i <- i+5))
    i %+=% 3
    print(i)
  }
  if (i > 30) { break }
}

max_drop <- na.omit(max_drop)
max_drop <- as.data.frame(max_drop)

z <- 2
for (i in 1:5) {
eval.parent(substitute(z <- z+5))
print(z)
}

k <- 5
substitute(k <- k+1)
k

inc = function(i) {
  
}

Max_point = function() {
  
}

max <- NULL
for (i in 2:nrow(data)) {
  if (data$psi.t..rad[i] > data$psi.t..rad[i-1] & data$psi.t..rad[i] > data$psi.t..rad[i+1]) {
    max <- rbind(max, as.data.frame(data[i, 1:2]))
    if (nrow(max) == 2) { break }
  }
}
max <- na.omit(max)
#hyp_point <- nrow(data)/nrow(max) # number of points for 1 oscillation
numb_point <- as.numeric(rownames(max))
numb_point <- sum(numb_point)/2 # nb de points moyens pour 1 oscillation


max_osc <- NULL
vec <- NULL
k <- 1
for (i in 1:nrow(data)) {
  if (i == nrow(data)) { break }
  if (i <= k + numb_point) {
    vec[i] <- data$psi.t..rad[i]
  }
  else if (i >= k + numb_point) {
    k <- k + numb_point
    vec <- na.omit(vec)
    max_osc[i] <- max(vec)
    vec <- NULL
  }
}
max_osc <- na.omit(max_osc)
max_osc <- as.data.frame(max_osc)


Maximum_period = function() {
  library(stringr)

  w <- unlist(str_extract_all(alldata[i], "\\d+"))[2]
  w <- as.numeric(w)*10^-3   # mHz -> Hz
  period <- (pi/w)/2 #?
  cat("The period of one oscillation is : ", period, "\n")
}

w <- NULL
library(stringr)
w <- unlist(str_extract_all(alldata[3], "\\d+"))[2]
w <- as.numeric(w)*10^-3   # mHz -> Hz
period <- (2*pi)/w 
period <- pi/(2*w) 
cat("The period of one oscillation is : ", period, "\n")













