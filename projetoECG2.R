#-----------------------------------------------------------------------------------------
#Projeto ECG - Aplicação do algoritmo de Pan & Tompkins para detecção de complexos QRS
#-----------------------------------------------------------------------------------------

#ETAPA DE ESTABELECIMENTO
#Localizar a biblioteca e definir os pacotes não padrões a serem utilizados.
.libPaths("C:/Users/JoséRoberto/AppData/Roaming/SPB_16.6/R/win-library/3.2")
library(signal)
library(ggplot2)
library(lattice)
library(gridExtra)

#Baixar os arquivos "mitdb_ecgSignals.csv" e "fs.csv" - caso ainda não tenham sido.
Url <- c("https://raw.githubusercontent.com/JsRoberto/ECGData/master/mitdb_ecgSignals.csv",
         "https://raw.githubusercontent.com/JsRoberto/ECGData/master/fs.csv")
Local <- c("mitdb_ecgSignals.csv","fs.csv")

download <- function(Local, Url) {
      if (!file.exists(Local)) {
            download.file(Url, Local)
      }
}

mapply(download, Local, Url)

#Salvar em formatos adequados as variáveis que representam os sinais.
Ecg.signals <- read.csv("mitdb_ecgSignals.csv", stringsAsFactors = FALSE)
Ecg.signalSplit <- split(Ecg.signals, Ecg.signals$signal_case)
fs <- read.csv("fs.csv")
fs <- as.numeric(fs)

#São obtidos a média "Mean1" e o desvio padrão "Std1" dos sinais não filtrados.
Mean1 <- sapply(Ecg.signalSplit, function(x) mean(x$signal_mag, na.rm = TRUE))
Std1 <- sapply(Ecg.signalSplit, function(x) sd(x$signal_mag, na.rm = TRUE))

#A função "dataECGplot()" é a principal função de plotagem do código, responsável por 
#---mostrar a evolução do sinal em cada etapa de processamento.
#---Seus argumentos são:
#---(1) "Ecg.signalSplit" - define a lista de sinais a serem plotados;
#---(2) "interval_seg" - define o intervalo de tempo em que o plote está delimitado;
#---(3) "Fs" - define a frequência de amostragem dos sinais.
dataECGplot <- function(Ecg.signalSplit, interval_seg = 0:60, Fs = fs) {
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

#-----------------------------------------------------------------------------------------
#Etapa de pré-processamento: primeira fase do algoritmo de Pan & Tompkins.

#Bloco 1 - Filtro passa-baixa.
N_lp <- c(1,rep(0,5),-2,rep(0,5),1)
D_lp <- 32*c(1,-2,1)

H_lpz <- freqz(N_lp, D_lp, Fs = fs) #Fs = 360 Hz admite Fc = 20 Hz.

#A função "fz_plot()" pretende gerar gráficos das respostas frequenciais dos filtros: 
#---magnitude (dB e linear) e fase em função da frequência normalizada.
#---Seus argumentos são: 
#---(1) "filter_freqz" - define o filtro propriamente dito, mediante a classe "freqz";
#---(2) "filter_type" - define se o filtro é passa-baixa "lp" ou passa-alta "hp";
#---(3) "Fs" - define a frequência de amostragem do filtro.
fz_plot <- function(filter_freqz, filter_type, Fs = fs){
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
      labs_x <- labs(x=expression(paste("Normalized Frequency (x ",pi," rad/sample)")))
      
      grid.arrange(p1 + labs_title, p2, p3 + labs_x, nrow=3)
}

fz_plot(H_lpz, "lp")

#A função "filter_ecgSignals()" pretende aplicar sobre a lista de sinais "data_ecg" o filtro
#---definido por uma função de transferência com numerador "H_Num" e denominador "H_Den".
#---Além disso, o sinal filtrado resultante "x_norm" está normalizado.
filter_ecgSignals <- function(data_ecg, H_Num, H_Den) {
      x <- sapply(data_ecg, filter, filt = H_Num, a = H_Den)
      x <- as.data.frame(x)
      x_norm <- sapply(x, function(x) {
              x <- x/max(abs(x))
      })
      x_norm <<- as.data.frame(x_norm)
}

filter_ecgSignals(Ecg.signalSplit, N_lp, D_lp)

#A função "update.filtSignal()" pretende atualizar a lista de sinais "Ecg.signalSplit" pela
#---lista de valores filtrados e normalizados "x_norm".
update.filtSignal <- function(Ecg.signalSplit, x_norm) {
      for (i in 1:length(Ecg.signalSplit)) {
            Ecg.signalSplit[[i]]$signal_mag <<- x_norm[[i]]
      }
}

update.filtSignal(Ecg.signals, x_norm)

dataECGplot(Ecg.signalSplit, 25:35)

#Bloco 2 - Filtro passa-alta.
N_hp <- c(-1,rep(0,15),32,-32,rep(0,14),1)
D_hp <- 32*c(1,-1)

H_hpz <- freqz(N_hp, D_hp, Fs = fs) #Fs = 360 Hz admite Fc = 9 Hz

fz_plot(H_hpz, "hp")

filter_ecgSignals(x_norm, N_hp, D_hp)

update.filtSignal(Ecg.signals, x_norm)

dataECGplot(Ecg.signalSplit, 25:35)

#Bloco 3 - Operador derivativo.
N_do <- c(2,1,0,-1,-2)
D_do <- 8

filter_ecgSignals(x_norm, N_do, D_do)

update.filtSignal(Ecg.signals, x_norm)

dt.signal <- Ecg.signalSplit

dataECGplot(dt.signal, 25:35)

#Bloco 4 - Operador que eleva os valores dos sinais ao quadrado.
x_norm <- sapply(x_norm, function(x) x^2)
x_norm <- as.data.frame(x_norm)

filter_ecgSignals(x_norm, 1, 1)

update.filtSignal(Ecg.signals, x_norm)

dataECGplot(Ecg.signalSplit, 25:35)

#Bloco 5 - Janela de integração móvel
N_if <- rep(1,54)
D_if <- 54

filter_ecgSignals(x_norm,N_if,D_if)

update.filtSignal(Ecg.signals,x_norm)

mwi.signal <- Ecg.signalSplit

dataECGplot(mwi.signal, 25:35)

#-----------------------------------------------------------------------------------------
#Etapa de decisão [PARTE 1]: segunda fase do algoritmo de Pan & Tompkins.
#No primeiro momento, são definidas as funções importantes para a detecção e classificação
#---dos picos dos sinais.

#A função "peakDetection()" apresenta como argumentos (a) "updated.dataSplit", uma lista
#---de sinais atualizada pela função "update.filtSignal()", (b) "samples", a quantidade de 
#---amostras que terá cada segmento de um sinal, (c) "Fs", a frequência de amostragem dos 
#---sinais.
#---O objetivo desta função é obter duas listas: 
#---(1) "peakValues", que armazena os vetores de picos de cada sinal;
#---(2) "peakIndez", que armazena os vetores de índices de cada pico em "peakValues".
peakDetection <- function(updated.dataSplit, samples, Fs = fs) {
      peakValues <- data.frame()
      peakIndex <- list()
      k <- 0:(Fs*60/samples)*samples
      for (i in 1:(Fs*60/samples)) {
            if (i == Fs*60/samples) {
                  k[i+1] <- k[i+1] + 1
            }
            peak.intervalAUX <- lapply(updated.dataSplit,
                                       function(x) x$signal_mag[(k[i]+1):k[i+1]])
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

#A função "initializingVariables()" tem o objetivo de inicializar vriaveis importantes para
#---construir as listas que armazenarão vários dados importantes do processamento:
#---(1) "noise.peaks", os vetores dos picos classificados como picos de ruidos;
#---(2) "signal.peaks", os vetores dos picos classificados como picos R ou picos de sinal;
#---(3) "index.Rpeak", os vetores dos indices dos picos definidos em "signal.peaks";
#---(4) "num.falsePos", os números que indicam a quantidade de falsos positivos;
#---(5) "index.falsePos", os vetores que indicam os indices de cada falso positivo;
#---(6) "df.UPDATED", os sinais atualizados com parte das informações das listas acima;
#---(7) "index" e "idx", são índices que auxiliam na iteração da função de classificação
#-------"classifying.peaks()" e na função de atualização "df.updated()", respectivamente.
initializingVariables <- function() {
      RR.originalIntervals <<- list()
      index.falsePos <<- list()
      num.falsePos <<- list()
      signal.peaks <<- list()
      noise.peaks <<- list()
      index.Rpeak <<- list()
      index <<- 1
      df.UPDATED <<- list()
      idx <<- 1
}

#A função "PKI()" tem o objetivo de gerar os parâmetros de NPKI e SPKI utilizados por Pan 
#---& Tompkins. Se o "vector.peaks" utilizado argumento for "noise.peaks", NPKI é obtido;
#---se for "signal.peaks", então SPKI é obtido. 
PKI <- function(vector.peaks) {
      if (length(vector.peaks)==1) {
            PEAKI <- vector.peaks[1]
      } else {
            PEAKI <- 0.125*vector.peaks[length(vector.peaks)] + 0.875*PKI(vector.peaks[-length(vector.peaks)])
      }
      PEAKI
}

#A função "THR()" utiliza os parâmetros NPKI e SPKI, obtidos com a função "PKI()", para
#---gerar os parâmetros de classificação dos picos dos sinais.
THR <- function(SPKI,NPKI) {
      THR1 <<- NPKI + 0.25*(SPKI - NPKI)
      THR2 <<- 0.5*THR1
}

#A função "lst2vct()" simplesmente transforma uma lista de vetores em um único vetor.
lst2vct <- function(lst) {
      vct <- vector()
      for (k in 1:length(lst)) {
            vct <- c(vct,lst[[k]])
      }
      vct
}

#A função "classifying.peaks()", devido a sua complexidade, terá comentários explicativos
#---sobre seus blocos de funcionamento ao logo do seu código. Contudo, resumidamente, seu
#---objetivo é gerar as listas "noise.peaks", "signal.peaks", "index.Rpeaks", "num.falsePos"
#---e "index.falsePos" inicializadas anteriormente pela função "initializingVariables()".
#---Como argumentos, são utilizados:
#---(1) "originalValues", a lista dos sinais cujos picos serão classificados;
#---(2) "peakValues" e "peaksIndex", as listas geradas pela função "peakDetection()";
#---(3) "initialTHR", a lista com os parâmetros iniciais de classificação de picos;
#---(4) "Fs", frequencia de amostragem dos sinais.   
classifying.peaks <- function(originalValues, peakValues, peakIndex, initial.THR, Fs = fs) {
      #-----------------------------------------------------------------------------------
      #O bloco abaixo identifica os dois primeiros picos R de "peakValues", de acordo com
      #---as condições iniciais "initial.THR". Há, também, a necessidade de identificar os
      #---missing values ou NAs de "peakValues".  
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
      #O bloco a seguir inicializa os vetores de classificação "noise.peaksAUX" e 
      #---"signal.peaksAUX"; o vetor de índices dos picos R "index.Speaks", referente a 
      #---"peakValues"; o vetor "RR.originalIntervals", que armazena a distância entre
      #---amostras de "signal.peaksAUX"; os vetores "index.originalALLpeaks" e 
      #---"index.originalSpeaks", que representam os índices, referentes ao sinal original,
      #---de todos os picos e dos picos de sinal, respectivamente.
      #---Além disso, os valores iniciais acima são utilizados para definir os primeiros
      #---parâmetros de classificação - "SPKI", "NPKI", "THR1" e "THR2".
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
      RR.originalIntervalsAUX <- diff(index.originalSpeaks)
      SPKI <- PKI(signal.peaksAUX)
      NPKI <- PKI(noise.peaksAUX)
      THR(SPKI,NPKI)
      #As variáveis abaixa são auxiliares para varredura do vetores de picos "peakValues" 
      #---("index.RpeakAUX") e para obtenção de informações sobre falsos positivos ("indexIn"
      #---e "num.falsePosAUX").
      indexIn <- 1
      index.RpeakAUX <- i
      num.falsePosAUX <- NULL
      #-----------------------------------------------------------------------------------
      #O laço "for" abaixo varre todos os valores de picos que ainda serão classificados.
      for (PEAKI in peakValues[(i+1):length(peakValues)]) {
            index.RpeakAUX <- index.RpeakAUX + 1
            if (!is.na(PEAKI)) {
                  if (PEAKI > THR1) {
                        lastIndex.originalSpeak <- index.originalSpeaks[length(index.originalSpeaks)]
                        lastRR.originalInterval <- index.originalALLpeaks[[index.RpeakAUX]] - lastIndex.originalSpeak
                        #As condições abaixo servem para indicar falsos positivos, ou seja,
                        #---se um pico detectado como R é, na verdade, um pico T.
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
                        #As condições abaixo tratam de testar a existência de falsos 
                        #---negativos, ou seja, se entre o último pico R classificado e o 
                        #---suposto atual existe outro pico R não detectado anteriormente. 
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
                        }
                        index.originalSpeaks <- index.originalALLpeaks[index.Speaks]
                        RR.originalIntervalsAUX <- diff(index.originalSpeaks)
                        SPKI <- PKI(signal.peaksAUX)
                  } else {
                        noise.peaksAUX <- c(noise.peaksAUX,PEAKI)
                        NPKI <- PKI(noise.peaksAUX)
                  }
                  THR(SPKI, NPKI)
            }
      }
      #-----------------------------------------------------------------------------------
      #No último bloco, os vetores gerados pelo processamento são armazenados nas listas 
      #---"noise.peaks", "signal.peaks", "index.Rpeaks", "num.falsePos" e "index.falsePos".
      if (!is.null(num.falsePosAUX)) {
            num.falsePos[[index]] <<- num.falsePosAUX
      }
      index.Rpeak[[index]] <<- index.originalSpeaks
      noise.peaks[[index]] <<- noise.peaksAUX
      signal.peaks[[index]] <<- signal.peaksAUX
      RR.originalIntervals[[index]] <<- RR.originalIntervalsAUX
      index <<- index + 1
}

#-----------------------------------------------------------------------------------------
#Etapa de decisão [PARTE 2]: segunda fase do algoritmo de Pan & Tompkins.
#No segundo momento, são (1) aplicadas as funções definidas anteriormente, (2) definidas as
#---listas de sinais atualizados com dados obtidos com a função "classifying.peaks()", (3)
#---plotados os gráficos resultades da atualização.
#---Esses procedimentos serão aplicados tanto à lista de sinais "dt.signal" quanto à lista
#---"mwi.signal".

#Aplicação sobre a lista "dt.signal".
#A primeira aplicação da função "peakDetection()" é usada pra obter os parâmetros iniciais
#---de classificação "initial.THR".
peakDetection(dt.signal, fs*3, fs)
initial.THR <- 0.35*apply(peakValues, 2, median, na.rm = TRUE)

#A segunda aplicação da função "peakDetection()" é usada pra obter os valores dos picos
#---que serão usados para classificação.
peakDetection(dt.signal, 80, fs)

initializingVariables()

mapply(classifying.peaks, dt.signal, peakValues, peakIndex, initial.THR)

#A função "df.updated()" atualiza o sinal "signal.df" com imformações sobre a localização 
#---("index.Rpeak") e a magnitude ("signal.peaks") dos picos desse sinal.
df.updated <- function(signal.df, signal.peaks, index.Rpeak) {
      signal_Rpeaks <- signal_Npeaks <- rep(NA,length(signal.df$signal_mag))
      signal_Rpeaks[index.Rpeak] <- signal.peaks
      df.UPDATED[[idx]] <<- signal.df
      df.UPDATED[[idx]]$signal_Rpeaks <<- signal_Rpeaks
      idx <<- idx + 1
}

mapply(df.updated, dt.signal, signal.peaks, index.Rpeak)

dt.signalUPD <- df.UPDATED

dataECGplot(dt.signalUPD, 25:35)

#Aplicação sobre a lista "mwi.signal"
peakDetection(mwi.signal, fs*3, fs)
initial.THR <- 0.35*apply(peakValues, 2, median, na.rm = TRUE)

peakDetection(mwi.signal, 80, fs)

initializingVariables()

mapply(classifying.peaks, mwi.signal, peakValues, peakIndex, initial.THR)

mapply(df.updated, mwi.signal, signal.peaks, index.Rpeak)

mwi.signalUPD <- df.UPDATED

dataECGplot(mwi.signalUPD, 25:35)

#
