library(signal)
library(ggplot2)

urlFile <- c("https://raw.githubusercontent.com/JsRoberto/ECGData/master/mitdb_ecgSignals.csv",
             "https://raw.githubusercontent.com/JsRoberto/ECGData/master/fs.csv")
localFile <- c("./mitdb_ecgSignals.csv","./fs.csv")

download <- function(urlFile, localFile) {
      if (!exists(localFile)) {
            download.file(urlFile, localFile)
      }
}

mapply(download, urlFile, localFile)

dataECG <- read.csv(localFile[1], header = TRUE)
dataECG.split <- split(dataECG, dataECG$signal_case)
fs <- read.csv(localFile[2], header = TRUE)
fs <- as.numeric(fs)

dataECG.plot <- function(dataECG.split, interval.seg = 0:60, Fs = fs) {
      interval.samples <- (min(interval.seg)*Fs):(max(interval.seg)*Fs) + 1
      #dataECG.splitTEST <- mapply(function(x) x$signal_mag[interval.samples],
      #                            dataECG.split)
      dataECG.split <- dataECG.split[[1]][interval.samples,]
      
      ggplot(dataECG.split, aes(x = t, y = signal_mag, group = signal_case)) +
            geom_line() +
            facet_wrap( ~ signal_case)
      
}

dataECG.plot(dataECG.split, 12:18)

#Pré-processamento
#Filtro passa-baixa
b.lp <- c(1,rep(0,5),-2,rep(0,5),1)
a.lp <- 32*c(1,-2,1)

H.lp <- freqz(b.lp, a.lp, Fs = fs)

#Filtro passa-alta
b.hp <- c(1,rep(0,31),-1)
a.hp <- c(1,-1)

H.hp <- freqz(b.hp, a.hp, Fs = fs)

H.plot <- function(H) {
      text <- deparse(substitute(H))  # get the name of the filter for the title
      c <- substr(text,4,4)
      plot(H$f, abs(H$h), col = "red", ylim = c(0,1),
           xlab = "Normalized Frequency",
           ylab = "Magnitude",
           main = paste("Filter H", c, "(z)", sep = "")
      )
}

par(mfrow=c(1,2))
plotList <- list(H.lp,H.hp)
lapply(plotList,H.plot)

dataECG.filtered <- function() {
      
}

#Operador derivativo
b.do <- c(2,1,0,-1,-2)/8

H.do <- freqz(b.do, Fs = fs)

#Squaring
