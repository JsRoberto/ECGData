panel.grid(h=-1, v=-1)
panel.xyplot(x, y, type = "l", lwd = 1)
panel.abline(h=mean(y), lwd=1, lty=2, col="navy")
}
xyplot(signal_mag ~ t | signal_case, data = Ecg.signalsAUX,
       layout=c(4,2),
       panel=panel.smoother,
       main="Ecg Signals", xlab="time (s)",ylab="Volts")
}
dataECGplot(Ecg.signalSplit, 25:35)
.libPaths("C:/Users/Jos?Roberto/AppData/Roaming/SPB_16.6/R/win-library/3.2")
library(signal)
library(ggplot2)
library(lattice)
library(gridExtra)
Url <- c("https://raw.githubusercontent.com/JsRoberto/ECGData/master/mitdb_ecgSignals.csv",
         "https://raw.githubusercontent.com/JsRoberto/ECGData/master/fs.csv")
Local <- c("mitdb_ecgSignals.csv","fs.csv")
download <- function(Local, Url) {
      if (!file.exists(Local)) {
            download.file(Url, Local)
      }
}
mapply(download, Local, Url)
Ecg.signals <- read.csv("mitdb_ecgSignals.csv", stringsAsFactors = FALSE)
Ecg.signalSplit <- split(Ecg.signals, Ecg.signals$signal_case)
Fs <- read.csv("fs.csv")
Fs <- as.numeric(Fs)
dataECGplot <- function(Ecg.signalSplit, interval_seg = 0:60, Fs = Fs) {
      Ecg.signalsAUX <- data.frame()
      signal_mag <- vector()
      interval <- 1+((Fs*min(interval_seg)):(Fs*max(interval_seg)))
      for (i in 1:length(Ecg.signalSplit)) {
            signal_mag <- c(signal_mag,
                            Ecg.signalSplit[[i]]$signal_mag[interval])
            Ecg.signalSplit[[i]] <- Ecg.signalSplit[[i]][interval,]
            Ecg.signalsAUX <- rbind(Ecg.signalsAUX,Ecg.signalSplit[[i]])
      }
      Ecg.signalsAUX$signal_mag <- signal_mag
      panel.smoother <- function(x, y) {
            panel.grid(h=-1, v=-1)
            panel.xyplot(x, y, type = "l", lwd = 1)
            panel.abline(h=mean(y), lwd=1, lty=2, col="navy")
      }
      xyplot(signal_mag ~ t | signal_case, data = Ecg.signalsAUX,
             layout=c(4,2),
             panel=panel.smoother,
             main="Ecg Signals", xlab="time (s)",ylab="Volts")
}
dataECGplot(Ecg.signalSplit, 25:35)
dataECGplot <- function(Ecg.signalSplit, interval_seg = 0:60, fs = Fs) {
      Ecg.signalsAUX <- data.frame()
      signal_mag <- vector()
      interval <- 1+((fs*min(interval_seg)):(fs*max(interval_seg)))
      for (i in 1:length(Ecg.signalSplit)) {
            signal_mag <- c(signal_mag,
                            Ecg.signalSplit[[i]]$signal_mag[interval])
            Ecg.signalSplit[[i]] <- Ecg.signalSplit[[i]][interval,]
            Ecg.signalsAUX <- rbind(Ecg.signalsAUX,Ecg.signalSplit[[i]])
      }
      Ecg.signalsAUX$signal_mag <- signal_mag
      panel.smoother <- function(x, y) {
            panel.grid(h=-1, v=-1)
            panel.xyplot(x, y, type = "l", lwd = 1)
            panel.abline(h=mean(y), lwd=1, lty=2, col="navy")
      }
      xyplot(signal_mag ~ t | signal_case, data = Ecg.signalsAUX,
             layout=c(4,2),
             panel=panel.smoother,
             main="Ecg Signals", xlab="time (s)",ylab="Volts")
}
dataECGplot(Ecg.signalSplit, 25:35)
.libPaths("C:/Users/Jos?Roberto/AppData/Roaming/SPB_16.6/R/win-library/3.2")
library(signal)
library(ggplot2)
library(lattice)
library(gridExtra)
Url <- c("https://raw.githubusercontent.com/JsRoberto/ECGData/master/mitdb_ecgSignals.csv",
         "https://raw.githubusercontent.com/JsRoberto/ECGData/master/fs.csv")
Local <- c("mitdb_ecgSignals.csv","fs.csv")
download <- function(Local, Url) {
      if (!file.exists(Local)) {
            download.file(Url, Local)
      }
}
mapply(download, Local, Url)
Ecg.signals <- read.csv("mitdb_ecgSignals.csv", stringsAsFactors = FALSE)
Ecg.signalSplit <- split(Ecg.signals, Ecg.signals$signal_case)
Fs <- read.csv("fs.csv")
Fs <- as.numeric(Fs)
dataECGplot <- function(Ecg.signalSplit, interval_seg = 0:60, fs = Fs) {
      Ecg.signalsAUX <- data.frame()
      signal_mag <- vector()
      interval <- 1+((fs*min(interval_seg)):(fs*max(interval_seg)))
      for (i in 1:length(Ecg.signalSplit)) {
            signal_mag <- c(signal_mag,
                            Ecg.signalSplit[[i]]$signal_mag[interval])
            Ecg.signalSplit[[i]] <- Ecg.signalSplit[[i]][interval,]
            Ecg.signalsAUX <- rbind(Ecg.signalsAUX,Ecg.signalSplit[[i]])
      }
      Ecg.signalsAUX$signal_mag <- signal_mag
      panel.smoother <- function(x, y) {
            panel.grid(h=-1, v=-1)
            panel.xyplot(x, y, type = "l", lwd = 1)
            panel.abline(h=mean(y), lwd=1, lty=2, col="navy")
      }
      xyplot(signal_mag ~ t | signal_case, data = Ecg.signalsAUX,
             layout=c(4,2),
             panel=panel.smoother,
             main="Ecg Signals", xlab="time (s)",ylab="Volts")
}
Ecg.signalSplit <- split(Ecg.signals$signal_mag,Ecg.signals$signal_case)
N_lp <- c(1,rep(0,5),-2,rep(0,5),1)
D_lp <- 32*c(1,-2,1)
H_lpz <- freqz(N_lp,D_lp,Fs = Fs)#Fs = 360 Hz admite Fc = 20 Hz e X samples or
#XX ms of delay
fz_plot <- function(filter_freqz, filter_type = "lp", Fs = Fs){
      df <- data.frame(w = rep(0,length(filter_freqz$f)),
                       mag = rep(0,length(filter_freqz$h)),
                       mag_dB = rep(0,length(filter_freqz$h)),
                       phase_degrees = rep(0,length(filter_freqz$h)))
      df$w <- filter_freqz$f/(Fs/2)
      df$mag <- abs(filter_freqz$h)
      df$mag_dB <- 20*log10(abs(filter_freqz$h))
      df$phase_degrees <- (180/pi)*Arg(filter_freqz$h)
      vec_aux <- df$mag_dB
      vec_aux <- vec_aux[-1]
      vec_aux <- vec_aux + 3
      vec_aux <- vec_aux^2
      wc_sample <- numeric()
      for (i in 1:(dim(df)[1]/2)) {
            if (df$mag_dB[-1][i] == df$mag_dB[-1][vec_aux == min(vec_aux, na.rm = TRUE)]) {
                  wc_sample <- i + 1
            }
      }
      interval <- wc_sample:512 #default case: low pass filter "lp"
      type <- "Low"
      if (filter_type == "hp"){
            interval <- 1:wc_sample
            type <- "High"
      }
      p1 <- ggplot(data=df, aes(x=w, y=mag)) +
            geom_line(color = "navy", size = 1) +
            geom_line(data = df[interval,], color = "red", size = 1.3) +
            labs(x="",y="Magnitude")
      p2 <- ggplot(data=df, aes(x=w, y=mag_dB)) +
            geom_line(color = "navy", size = 1) +
            geom_line(data = df[interval,], color = "red", size = 1.3) +
            labs(x="",y="Magnitude (dB)")
      p3 <- ggplot(data=df, aes(x=w, y=phase_degrees)) +
            geom_line(color = "navy", size = 1) +
            labs(x="",y="Phase (degrees)")
      labs_title <- labs(title=paste(type,"Pass Filter - Frequency Response"))
      labs_x <- labs(x=expression(paste("Normalized Frequency (x ",pi,
                                        " rad/sample)")))
      grid.arrange(p1 + labs_title,p2,p3 + labs_x,nrow=3)
}
fz_plot(H_lpz)
fz_plot <- function(filter_freqz, filter_type = "lp", fs = Fs){
      df <- data.frame(w = rep(0,length(filter_freqz$f)),
                       mag = rep(0,length(filter_freqz$h)),
                       mag_dB = rep(0,length(filter_freqz$h)),
                       phase_degrees = rep(0,length(filter_freqz$h)))
      df$w <- filter_freqz$f/(fs/2)
      df$mag <- abs(filter_freqz$h)
      df$mag_dB <- 20*log10(abs(filter_freqz$h))
      df$phase_degrees <- (180/pi)*Arg(filter_freqz$h)
      vec_aux <- df$mag_dB
      vec_aux <- vec_aux[-1]
      vec_aux <- vec_aux + 3
      vec_aux <- vec_aux^2
      wc_sample <- numeric()
      for (i in 1:(dim(df)[1]/2)) {
            if (df$mag_dB[-1][i] == df$mag_dB[-1][vec_aux == min(vec_aux, na.rm = TRUE)]) {
                  wc_sample <- i + 1
            }
      }
      interval <- wc_sample:512 #default case: low pass filter "lp"
      type <- "Low"
      if (filter_type == "hp"){
            interval <- 1:wc_sample
            type <- "High"
      }
      p1 <- ggplot(data=df, aes(x=w, y=mag)) +
            geom_line(color = "navy", size = 1) +
            geom_line(data = df[interval,], color = "red", size = 1.3) +
            labs(x="",y="Magnitude")
      p2 <- ggplot(data=df, aes(x=w, y=mag_dB)) +
            geom_line(color = "navy", size = 1) +
            geom_line(data = df[interval,], color = "red", size = 1.3) +
            labs(x="",y="Magnitude (dB)")
      p3 <- ggplot(data=df, aes(x=w, y=phase_degrees)) +
            geom_line(color = "navy", size = 1) +
            labs(x="",y="Phase (degrees)")
      labs_title <- labs(title=paste(type,"Pass Filter - Frequency Response"))
      labs_x <- labs(x=expression(paste("Normalized Frequency (x ",pi,
                                        " rad/sample)")))
      grid.arrange(p1 + labs_title,p2,p3 + labs_x,nrow=3)
}
fz_plot(H_lpz)
filter_ecgSignals <- function(data_ecg,H_Num,H_Den){
      x <- sapply(data_ecg, filter, filt = H_Num, a = H_Den)
      x <- as.data.frame(x)
      x_norm <<- x
      #x_norm <- sapply(x, function(x) {
      #        x <- x/max(abs(x))
      #})
      #x_norm <<- as.data.frame(x_norm)
}
filter_ecgSignals(Ecg.signalSplit,N_lp,D_lp)
update.filtSignal <- function(Ecg.signals, x_norm){
      Ecg.signalSplit <<- split(Ecg.signals,Ecg.signals$signal_case)
      for (i in 1:length(Ecg.signalSplit)) {
            Ecg.signalSplit[[i]]$signal_mag <<- x_norm[[i]]
      }
}
update.filtSignal(Ecg.signals,x_norm)
N_hp <- c(-1,rep(0,15),32,-32,rep(0,14),1)
D_hp <- 32*c(1,-1)
H_hpz <- freqz(N_hp,D_hp,Fs = Fs)#Fs = 360 Hz admite Fc = 9 Hz e X samples or
#XX ms of delay
fz_plot(H_hpz, "hp")
filter_ecgSignals(x_norm,N_hp,D_hp)
update.filtSignal(Ecg.signals,x_norm)
#Bloco 3 - Derivative operator
N_do <- c(2,1,0,-1,-2)
D_do <- 8
filter_ecgSignals(x_norm,N_do,D_do)
update.filtSignal(Ecg.signals,x_norm)
dt.signal <- Ecg.signalSplit
x_norm <- sapply(x_norm, function(x) x^2)
x_norm <- as.data.frame(x_norm)
filter_ecgSignals(x_norm,1,1)
update.filtSignal(Ecg.signals,x_norm)
N_if <- rep(1,54)
D_if <- 54
filter_ecgSignals(x_norm,N_if,D_if)
update.filtSignal(Ecg.signals,x_norm)
mwi.signal <- Ecg.signalSplit
peakDetection <- function(updated.dataSplit, samples, Fs) {
      peak.values <- data.frame()
      peak.index <- list()
      k <- 0:(Fs*60/samples)*samples
      for (i in 1:(Fs*60/samples)) {
            if (i == Fs*60/samples) {
                  k[i+1] <- k[i+1] + 1
            }
            peak.intervalAUX <- lapply(updated.dataSplit,
                                       function(x) x$signal_mag[(k[i]+1):k[i+1]])
            #peak.intervalAUX <- as.data.frame(peak.intervalAUX)
            peak.valuesAUX <- lapply(peak.intervalAUX,max)
            peak.indexAUX <- mapply(function(peak,intmag) (k[i]:k[1+i])[intmag==peak],
                                    peak.valuesAUX,peak.intervalAUX)
            maximum <- 0
            for (l in 1:length(updated.dataSplit)){
                  if (length(peak.indexAUX[[l]]) > maximum) {
                        maximum <- length(peak.indexAUX[[l]])
                  }
            }
            if (maximum > 1) {
                  for (aux in 1:length(updated.dataSplit)) {
                        peak.valuesAUX[[aux]] <- c(rep(peak.valuesAUX[[aux]],length(peak.indexAUX[[aux]])),
                                                   rep(NA, maximum - length(peak.indexAUX[[aux]])))
                        peak.indexAUX[[aux]] <- c(peak.indexAUX[[aux]],
                                                  rep(NA,maximum - length(peak.indexAUX[[aux]])))
                  }
            }
            peak.valuesAUX <- as.data.frame(peak.valuesAUX)
            peak.values <- rbind(peak.values,peak.valuesAUX)
            for (j in 1:length(updated.dataSplit)) {
                  if (length(peak.index) < j) {
                        peak.index[[j]] <- list()
                  }
                  peak.index[[j]][[i]] <- peak.indexAUX[[j]]
            }
      }
      peakValues <<- as.data.frame(peak.values,
                                   row.names = 1:dim(peak.values)[1])
      peakIndex <<- peak.index
}
initializingVariables <- function() {
      #As variáveis abaixo são importantes para a atualização correta dos vetores que
      #indicam os falsos positivos (indices e quantidades; além disso,
      #um vetor indice de auxílio para obtenção dos outros) (esses valores são
      #obtidos com base na métrica do tempo entre picos R - 200 ms e 360 ms são
      #os pontos de referência) de todos os 10 sinais analisados.
      if (!exists("index.falsePos")) {
            index.falsePos <<- list()
      }
      if (!exists("num.falsePos")) {
            num.falsePos <<- list()
      }
      noise.peaks <<- list()
      signal.peaks <<- list()
      index.Rpeak <<- list()
      index <<- 1
      #As variaveis seguinda servem para a posterior atualização dos dataframes
      #para incluir uma coluna de signal_Rpeaks
      df.UPDATED <<- list()
      idx <<- 1
}
PKI <- function(vector.peaks) {
      if (length(vector.peaks)==1) {
            PEAKI <- vector.peaks[1]
      } else {
            PEAKI <- 0.125*vector.peaks[length(vector.peaks)] + 0.875*PKI(vector.peaks[-length(vector.peaks)])
      }
      PEAKI
}
THR <- function(SPKI,NPKI) {
      THR1 <<- NPKI + 0.25*(SPKI - NPKI)
      THR2 <<- 0.5*THR1
}
transf <- function(lista) {
      transformed <- vector()
      for (k in 1:length(lista)) {
            transformed <- c(transformed,lista[[k]])
      }
      transformed
}
classifying.peaks <- function(originalValues, peakValues, peakIndex, initial.THR, Fs, signal) {
      #O bloco abaixo indentifica os dois primeiros picos R de "peakValues", de acordo com as
      #condições iniciais disponiveis em "initial.THR"
      aux <- j <- 0
      auxNAsup <- auxNAinf <- 0
      for (i in 1:length(peakValues)) {
            if (!is.na(peakValues[i])){
                  if (peakValues[i] > initial.THR) {
                        if (aux < 1) {
                              aux <- aux + 1
                              if (aux == 1) {
                                    j <- i
                              }
                        } else break
                  }
            } else {
                  if (j == 0) {
                        auxNAinf <- c(i,auxNAinf)
                  } else {
                        auxNAsup <- c(i,auxNAsup)
                  }
            }
      }
      #O bloco abaixo inicializa os vetores de classificação (noise.peaks e signal.peaks),
      #vetores de indices dos picos Rs do sinal de picos "peakValues" (index.Speaks) e um vetor que
      #armazena a distancia entre amostras para esses picos R (rr.intervals).
      #Além disso, os valores iniciais são usados para definir os parâmetros
      #iniciais de classificação - futuramente, a cada novo sinal classificado,
      #os parâmetros SPKI, NPKI, THR1 e THR2 serão atualizados.
      #PAREI AQUI!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (auxNAsup[1] == 0 & auxNAinf[1] == 0) {
            noise.peaksAUX <- peakValues[1:i][-i][-j]
      } else {
            if (auxNAsup[1] != 0 & auxNAinf[1] != 0) {
                  noise.peaksAUX <- peakValues[1:i][-i][-auxNAsup][-j][-auxNAinf]
            } else {
                  if (auxNAsup[1] == 0) {
                        noise.peaksAUX <- peakValues[1:i][-i][-j][-auxNAinf]
                  }
                  if (auxNAinf[1] == 0) {
                        noise.peaksAUX <- peakValues[1:i][-i][-auxNAsup][-j]
                  }
            }
      }
      signal.peaksAUX <- c(peakValues[j],peakValues[i])
      index.Speaks <- c(j,i)
      index.originalALLpeaks <- transf(peakIndex)
      index.originalSpeaks <- index.originalALLpeaks[index.Speaks]
      RR.originalIntervals <- diff(index.originalSpeaks)
      SPKI <- PKI(signal.peaksAUX)
      NPKI <- PKI(noise.peaksAUX)
      THR(SPKI,NPKI)
      #As variáveis (1) index.Rpeak e (2) num.falsePos representam (1) o índice do último
      #pico detectado & (2) a quantidade de falsos positivos (obtidos com a métrica do tempo
      #entre picos R - 200 ms e 360 ms são os pontos de referência).
      indexIn <- 1
      index.RpeakAUX <- i
      num.falsePosAUX <- NULL
      #O laço for abaixo varre todos os valores de picos que ainda devem ser
      #classificados
      for (PEAKI in peakValues[(i+1):length(peakValues)]) {
            index.RpeakAUX <- index.RpeakAUX + 1
            if (!is.na(PEAKI)) {
                  if (PEAKI > THR1) {
                        lastIndex.originalSpeak <- index.originalSpeaks[length(index.originalSpeaks)]
                        lastRR.originalInterval <- index.originalALLpeaks[[index.RpeakAUX]] - lastIndex.originalSpeak
                        #As condições seguintes se aplicam apenas ao sinal derivativo
                        if (signal == "dx/dt") {
                              #As condições abaixo servem para indicar falsos posi-
                              #-tivos, ou seja, se um pico detectado como R é na verdade
                              #um pico T.
                              if (lastRR.originalInterval < 0.36*Fs) {
                                    if (lastRR.originalInterval > 0.2*Fs) {
                                          lastIndex <- index.originalALLpeaks[[index.RpeakAUX]]
                                          beforeIndex <- lastIndex.originalSpeak
                                          if (lastIndex < dim(originalValues)[1]){
                                                lastSlope <- (originalValues$signal_mag[lastIndex+1]-originalValues$signal_mag[lastIndex-1])*Fs/2
                                          } else {
                                                lastSlope <- (originalValues$signal_mag[lastIndex]-originalValues$signal_mag[lastIndex-1])*Fs/2
                                          }
                                          beforeSlope <- (originalValues$signal_mag[beforeIndex+1]-originalValues$signal_mag[beforeIndex-1])*Fs/2
                                          if (abs(lastSlope) < (abs(beforeSlope)/2)) {
                                                if (length(index.falsePos) < index) {
                                                      index.fasePos[[index]] <<- list()
                                                }
                                                index.falsePos[[index]][[indexIn]] <<- lastIndex
                                                indexIn <- indexIn + 1
                                                if (is.null(num.falsePosAUX)) {
                                                      num.falsePosAUX <- 0
                                                }
                                                num.falsePosAUX <- num.falsePosAUX + 1
                                                noise.peaksAUX <- c(noise.peaksAUX,PEAKI)
                                                NPKI <- PKI(noise.peaksAUX)
                                                THR(SPKI, NPKI)
                                                next
                                          }
                                    } else {
                                          if (length(index.falsePos) < index) {
                                                index.falsePos[[index]] <<- list()
                                          }
                                          index.falsePos[[index]][[indexIn]] <<- lastIndex <- lastIndex.originalSpeak
                                          indexIn <- indexIn + 1
                                          if (is.null(num.falsePosAUX)) {
                                                num.falsePosAUX <- 0
                                          }
                                          num.falsePosAUX <- num.falsePosAUX + 1
                                          noise.peaksAUX <- c(noise.peaksAUX,PEAKI)
                                          NPKI <- PKI(noise.peaksAUX)
                                          THR(SPKI, NPKI)
                                          next
                                    }
                              }
                        }
                        ###DETECÇÃO DE FALSOS NEGATIVOS --- ESSE ALGORITMO SE APLICA A AMBOS OS CONJUNTOS DE SINAIS
                        ###ANALISADOS, TANTO O dx/dt quanto o mwi
                        lastIndex.Speak <- index.Speaks[length(index.Speaks)]
                        if (lastRR.originalInterval > 1.66*mean(RR.originalIntervals)) {
                              peakValuesAUX <- peakValues[(lastIndex.Speak+1):(index.RpeakAUX-1)]
                              peakValuesAUX2 <- peakValuesAUX[peakValuesAUX < THR1 & peakValuesAUX > THR2]
                              new.Rpeak <- max(peakValuesAUX2, na.rm = TRUE)
                              if (!is.infinite(new.Rpeak)) {
                                    indexRm.Npeak1 <- (1:length(peakValuesAUX[!is.na(peakValuesAUX)]))[peakValuesAUX[!is.na(peakValuesAUX)]==new.Rpeak]
                                    indexRm.Npeak2 <- (1:length(peakValuesAUX))[peakValuesAUX==new.Rpeak & !is.na(peakValuesAUX)]
                                    signal.peaksAUX <- c(signal.peaksAUX,
                                                         new.Rpeak,PEAKI)
                                    noise.peaksAUX <- c(noise.peaksAUX[1:(length(noise.peaksAUX)-length(peakValuesAUX[!is.na(peakValuesAUX)]))],
                                                        noise.peaksAUX[(length(noise.peaksAUX)-length(peakValuesAUX[!is.na(peakValuesAUX)])+1):length(noise.peaksAUX)][-indexRm.Npeak1])
                                    index.Speaks <- c(index.Speaks,lastIndex.Speak + indexRm.Npeak2,index.RpeakAUX)
                              } else {
                                    signal.peaksAUX <- c(signal.peaksAUX, PEAKI)
                                    index.Speaks <- c(index.Speaks, index.RpeakAUX)
                              }
                        } else {
                              signal.peaksAUX <- c(signal.peaksAUX, PEAKI)
                              index.Speaks <- c(index.Speaks, index.RpeakAUX)
                        }###Obs.: não é posivel fazer a atualização correta do valor
                        ### de NPKI assim como fazemos do SPKI, uma vez que precisamos
                        ###voltar inumeros passos e reatualizar os valores todos
                        ###Contudo, o algoritmo usado corrige os valores de NPKI a cada pico R encontrado.
                        ###Então, por exemplo, se houver algum falso negativo no intervalo RR corrente, todos
                        ###o valores de NPKI estarão errados a partir dele até o ultimo "noise peak"
                        ###antes do pico R; mas, a partir daí, detecta-se a presença desse falso negativo,
                        ###e o valor correto de NPKI, a partir desse ultimo pico R, será obtido.
                        index.originalSpeaks <- index.originalALLpeaks[index.Speaks]
                        RR.originalIntervals <- diff(index.originalSpeaks)
                        SPKI <- PKI(signal.peaksAUX)
                  } else {
                        noise.peaksAUX <- c(noise.peaksAUX,PEAKI)
                        NPKI <- PKI(noise.peaksAUX)
                  }
                  THR(SPKI, NPKI)
            }
      }
      if (!is.null(num.falsePosAUX)) {
            num.falsePos[[index]] <<- num.falsePosAUX
      }
      index.Rpeak[[index]] <<- index.originalSpeaks
      noise.peaks[[index]] <<- noise.peaksAUX
      signal.peaks[[index]] <<- signal.peaksAUX
      index <<- index + 1
}
#Essa primeira aplicação da função peakDetection() é usada pra obter os thresholds
peakDetection(dt.signal,Fs*3,Fs)
initial.THR <- 0.35*apply(peakValues,2,median, na.rm = TRUE)
#Essa segunda aplicação da função peakDetection() é usada pra obter os valores de picos
#que serão usados para classíficação
peakDetection(dt.signal,80,Fs)
initializingVariables()
mapply(classifying.peaks, dt.signal, peakValues, peakIndex, initial.THR, Fs = 360, signal = "dx/dt")
df.updated <- function(signal.df, signal.peaks, noise.peaks, index.Rpeak) {
      df.UPDATED[[idx]] <<- signal.df
      signal_Rpeaks <- signal_Npeaks <- rep(NA,length(signal.df$signal_mag))
      signal_Rpeaks[index.Rpeak] <- signal.peaks
      #signal_Npeaks[-index.Rpeak] <- noise.peaks
      df.UPDATED[[idx]]$signal_Rpeaks <<- signal_Rpeaks
      #df.UPDATED[[idx]]$signal_Npeaks <<- signal_Npeaks
      idx <<- idx + 1
}
mapply(df.updated, dt.signal, signal.peaks, noise.peaks, index.Rpeak)
dt.signalUPD <- df.UPDATED
peakDetection(mwi.signal,Fs*3,Fs)
initial.THR <- 0.35*apply(peakValues,2,median, na.rm = TRUE)
#Essa segunda aplicação da função peakDetection() é usada pra obter os valores de picos
#que serão usados para classíficação
peakDetection(mwi.signal,80,Fs)
initializingVariables()
mapply(classifying.peaks, mwi.signal, peakValues, peakIndex, initial.THR, Fs = 360, signal = "dx/dt")
mwi.signalUPD <- df.UPDATED
dataECGplot(dt.signalUPD,25:35)
dataECGplot(mwi.signalUPD,25:35)
mapply(df.updated, mwi.signal, signal.peaks, noise.peaks, index.Rpeak)
setwd("C://Rdir")
