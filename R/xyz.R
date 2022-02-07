
# X, y1, y2, y3...
#Le faire avec delta t aussi

#mettre tout dans un fichier avec le t et x constant pour tous les y 
#le t y1 2 et 3
#au lieu d'avoir des films et numéros on a des colonnes --> chaque colonne équivalent à un film

setwd(path_Input)
par(mfrow = c(2, 2))
film <- NULL
allfilms <- list.files(path = path_Input, ".txt", full.names = FALSE)
for (i in 1:length(allfilms)) {
film <- read.table(allfilms[i], header = TRUE, sep = "\t", dec = ".")
film <- film %>% relocate(viscosity..Pa.s.)
cat(allfilms[i], "\n")
cat("number of columns : ", ncol(film), "\n")
cat("number of rows (points) : ", nrow(film), rep("\n", 2))

if (ncol(film) < 2) {
cat(allfilms[i])
stop("not enought columns in the dataframe")
}

if (length(allfilms) < 1) {
stop("no file to analyse")
}

for (j in 2:ncol(film)) {
if (colnames(film[j]) == "omegaC.t..rad.s") {
break
}
plot(film[,j], film$viscosity..Pa.s., 
xlab = colnames(film[j]), 
ylab = "viscosity",
main = "viscosity as a function of other columns",
sub = allfilms[i])
lines(film[,j], film$viscosity..Pa.s.)
}
plot(film$omegaC.t..rad.s, film$viscosity..Pa.s., 
xlab = colnames(film[j]), 
ylab = "viscosity",
main = "viscosity as a function of other columns", 
sub = allfilms[i])

cat("Does the time series explain the viscosity?\n")

#Simple linear model
#suppression of NA
film <- na.omit(film)

#suppression of outliers
box <- boxplot(film$viscosity..Pa.s., main = "visualization of outliers", ylab = "viscosity",
sub = allfilms[i])
box

outlier <- box$out
if (is_empty(outlier) == FALSE) {
cat("\noutliers : ", outlier, "\n")
film <- film[-which(film$viscosity..Pa.s. %in% outlier),]
}

fit <- lm(film$viscosity..Pa.s. ~ film$time.s..max.min., film)
plot(fit, sub = allfilms[i])
coef_fit <- c(fit$coefficients)

prediction <- as.data.frame(predict(fit, interval = "confidence"))
print(summary(fit))

#if (i > 5) {
break
#}
}
par(mfrow=c(1,1))
