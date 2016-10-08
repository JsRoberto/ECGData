library(signal)

urlFile <- c("https://raw.githubusercontent.com/JsRoberto/ECGData/master/mitdb_ecgSignals.csv",
             "https://raw.githubusercontent.com/JsRoberto/ECGData/master/fs.csv")
localFile <- c("./mitdb_ecgSignals.csv","./fs.csv")

download <- function(urlFile, localFile) {
      if (!exists(localFile)) {
            download.file(urlFile, localFile)
      }
}

mapply(download, urlFile, localFile)

#Obs: mudar o arquivo depois
dataECG <- read.csv(localFile[1], header = TRUE)
dataECG.split <- split(dataECG$signal_mag, dataECG$signal_case)
fs <- read.csv(localFile[2], header = TRUE)
fs <- as.numeric(fs)

dataECG.plot <- function(dataECG.split, interval.seg = 0:60, Fs = fs) {
      interval.samples <- (min(interval.seg)*Fs):(max(interval.seg)*Fs) + 1
      test <- ts(dataECG.split$`100`[interval.samples], start = min(interval.seg), end = max(interval.seg),
                 frequency = Fs)
      plot(test)
}

dataECG.plot(dataECG.split, 12:18)

#Pré-processamento
#Filtro passa-baixa
b.lp <- c(1,rep(0,5),-2,rep(0,5),1)
a.lp <- 32*c(1,-2,1)

H.lp <- freqz(b.lp, a.lp, Fs = fs)

dataECG.filtered <- function(dataECG.split, num.tf, den.tf, Fs = fs) {
      filtered.signals <- lapply(dataECG.split, filter, filt = num.tf, a = den.tf)
      dataECG.split <<- lapply(filtered.signals, ts, start = 0, end = 60, frequency = Fs)  
}

dataECG.filtered(dataECG.split, b.lp, a.lp)

dataECG.plot(dataECG.split, 12:18)

#Filtro passa-alta
b.hp <- c(-1,rep(0,15),32,-32,rep(0,14),1)
a.hp <- 32*c(1,-1)

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

dataECG.filtered(dataECG.split, b.hp, a.hp)

dataECG.plot(dataECG.split)

#Operador derivativo
b.do <- c(2,1,0,-1,-2)
a.do <- 8

dataECG.filtered(dataECG.split, b.do, a.do)

dataECG.plot(dataECG.split)

do.signals <- dataECG.split

#Squaring
dataECG.split <- lapply(dataECG.split, function(x) x^2)
#---obs.: verificar se start e end são desconfigurados

dataECG.plot(dataECG.split)

#Janela de integração móvel
b.mwi <- rep(1,54)
a.mwi <- 54

dataECG.filtered(dataECG.split, b.mwi, a.mwi)

dataECG.plot(dataECG.split)

mwi.signals <- dataECG.split

#-----------------------------------------------------------------------------------------
#Estágio de decisão
peak.detection <- function(dataECG.split, samples, Fs = fs) {
      peak.valuesAUX <- list()
      #Solução achada (mas é ruim!): retirar a ultima amostra do sinal
      dataECG.split <- lapply(dataECG.split, function(x) x[-length(x)])
      dataECG.intervals <- lapply(dataECG.split, split, 
                                  f = gl(length(dataECG.split$`100`)/samples, samples))
      for (i in 1:length(dataECG.intervals)) {
            peak.valuesAUX[[i]] <- sapply(dataECG.intervals[[i]], max)
      }
      peak.values <<- peak.valuesAUX
}
###OBS.: por enquanto, não estou guardando o vetor de indices dos picos detectados,
###conforme a necessidade, revejo este tópico

start.THR <- function(peak.values) {
      initial.thr <- apply(as.data.frame(peak.values), 2, median, na.rm = TRUE)
      initial.THR <<- 0.35*initial.thr
}

PKI <- function(peaks.vector) {
      if (length(peaks.vector) <= 1) {
            peaki <- peaks.vector
      } else {
            peaki <- 0.125*peaks.vector[length(peaks.vector)] + 0.875*PKI(peaks.vector[-length(peaks.vector)])
      }
      peaki
}

THR <- function(noise.peaks, signal.peaks) {
      NPKI <- PKI(noise.peaks)
      SPKI <- PKI(signal.peaks)
      THR1 <<- NPKI + 0.25*(SPKI - NPKI)
      THR2 <<- THR1/2
}

###O vetor para inicializar as variáveis auxiliares será feito conforme a necessidade.

peak.classification <- function(dataECG.split, peak.values, initial.THR, Fs = fs) {
      ##Primeira parte: usar o parâmetro inicial initial.THR para classificar
      ##---os primeiros picos de peak.values, até o segundo pico R.
      
}
