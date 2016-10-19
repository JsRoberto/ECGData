#Objetivo 1: melhorar a as fase de estabelecimento e de decisão (parte 1). Ou seja, melhor
#nomear as variáveis, melhorar as funções etc.
#
#Objetivo 2: (2.1) adaptar a função "classifying.peaks()" para atuar diretamente sobre 
#listas, isso vai evitar a necessidade de "initializingVariables()"; (2.2) difidir a mesma
#função em blocos de funções: o de processamento inicial, o do algoritmo de falsos 
#negativos e o do algoritmo de falsos positivos.
#

a <- 1:12
b <- 1:6
c <- 1:18
d < list(a,b,c)
app <- sapply(d, function(x) x <- c(x, x))

interval.samples <- 1000:2000
df.aux <<- data.frame()
a <- lapply(dataECG.split, 
            function(x) 
                  df.aux <<- rbind(df.aux, x[interval.samples,]))[[length(dataECG.split)]]

#-----------------------------------------------------------------------------------------
#Projeto ECG - Algoritmo de Pan & Tompkins para detecção de complexos QRS
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
#Fase de estabelecimento------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
#Localizar a biblioteca e definir os pacotes não padrões a serem utilizados.
.libPaths("C:/Users/JoséRoberto/AppData/Roaming/SPB_16.6/R/win-library/3.2")
library(signal)
library(ggplot2)
library(gridExtra)

#Baixar os arquivos "mitdb_ecgSignals.csv" e "fs.csv" - caso ainda não tenham sido.
urlFile <- 
      c("https://raw.githubusercontent.com/JsRoberto/ECGData/master/mitdb_ecgSignals.csv",
        "https://raw.githubusercontent.com/JsRoberto/ECGData/master/fs.csv")
localFile <- c("./mitdb_ecgSignals.csv","./fs.csv")

download <- function(urlFile, localFile) {
      if (!exists(localFile)) {
            download.file(urlFile, localFile)
      }
}

mapply(download, urlFile, localFile)

#Salvar em formatos adequados as variáveis que representam os sinais.
dataECG <- read.csv(localFile[1], header = TRUE)
dataECG.split <- split(dataECG, dataECG$signal_case)
fs <- read.csv(localFile[2], header = TRUE)
fs <- as.numeric(fs)

#São obtidos a média "mean1" e o desvio padrão "std1" dos sinais não filtrados.
mean1 <- sapply(dataECG.split, function(x) mean(x$signal_mag, na.rm = TRUE))
std1 <- sapply(dataECG.split, function(x) sd(x$signal_mag, na.rm = TRUE))

dataECG.plot <- function(dataECG, interval.seg = 0:60, Fs = fs) {
      interval.samples <- (min(interval.seg)*Fs):(max(interval.seg)*Fs) + 1
      dataECG.aux <<- data.frame()
      dataECG.aux2 <- lapply(dataECG.split,
                             function(x) dataECG.aux <<- 
                                   rbind(dataECG.aux, x[interval.samples,]))
      
      p1 <- ggplot(dataECG.aux, aes(x = t, y = signal_mag, group = signal_case)) + 
            geom_line(color = "blue3") + 
            facet_wrap( ~ signal_case, nrow = 3) + 
            labs(title = "Sinais ECG", x = "tempo (s)", y = "magnitude (Volts)")
      
      if (is.null(Ecg.signalsAUX$signal_Rpeaks)) {
            p1
      } else {
            p1 + geom_point(mapping = aes(y = signal_Rpeaks), color = "darkolivegreen3")
      }
}

freqz.plot <- function(filter.freqz, Fs = fs) {
      filter.df <- data.frame(w = filter.freqz$f/(Fs/2),
                              mag = abs(filter.freqz$h),
                              mag_dB = 20*log10(abs(filter.freqz$h)),
                              phase_degrees = (180/pi)*Arg(filter.freqz$h))
      filter.df <- na.omit(filter.df)
      cutoff.freq <- function(filter.df, interval = 1:length(filter.df$mag)) {
            vec_aux <- filer.df$mag_dB[interval]
            vec_aux <- vec_aux + 3
            vec_aux <- vec_aux^2
            wc_sample <- (1:length(vec_aux))[vec_aux==min(vec_aux)]
            wc_sample
      }
      if (round(filter.df$mag[length(filter.df$mag)]) == 1
          & round(filter.df$mag[1]) == 1) {#rejeita-faixa
            filter.type <- "rejeita-faixa"
            central.freq <- (1:length(filter.df$mag))[min(filter.df$mag)] 
            wc_sample1 <- cutoff.freq(filter.df, 1:central.freq)
            wc_sample2 <- cutoff.freq(filter.df, central.freq:length(filter.df$mag))
            interval <- wc_sample1:wc_sample2
      } else {
            if (round(filter.df$mag[length(filter.df$mag)]) == 1) {#passa-alta
                  filter.type <- "passa-alta"
                  wc_sample <- cutoff.freq(filter.df)
                  interval <- 1:wc_sample
            }
            if (round(df$mag[1]) == 1) {#passa-baixa
                  filter.type <- "passa-baixa"
                  wc_sample <- cutoff.freq(filter.df)
                  interval <- wc_sample:length(filter.df$mag)
            }
            if (round(filter.df$mag[length(filter.df$mag)]) == 0
                & round(filter.df$mag[1]) == 0) {#passa-faixa
                  filter.type <- "passa-faixa"
                  central.freq <- (1:length(filter.df$mag))[max(filter.df$mag)] 
                  wc_sample1 <- cutoff.freq(filter.df, 1:central.freq)
                  wc_sample2 <- cutoff.freq(filter.df, central.freq:length(filter.df$mag))
                  interval <- c(1:wc_sample1,
                                rep(NA, length(filter.df) + wc_sample1 - wc_sample2 + 1),
                                wc_sample2:length(filter.df$mag))
            }
      }
      
      p1 <- ggplot(data = df, aes(x = w, y = mag)) +
            geom_line(color = "blue3", size = 1) +
            geom_line(data = df[interval,], color = "red", size = 1.2) +
            labs(x = "", y = "Magnitude")
      p2 <- ggplot(data = df, aes(x = w, y = mag_dB)) +
            geom_line(color = "blue3", size = 1) +
            geom_line(data = df[interval,], color = "red", size = 1.2) +
            labs(x = "", y = "Magnitude (dB)")
      p3 <- ggplot(data = df, aes(x = w, y = phase_degrees)) +
            geom_line(color = "blue3", size = 1) +
            labs(x = "", y = "Fase (graus)")
      
      labs.title <- labs(title = paste("Filtro", filter.type,
                                        "- Resposta em Frequência"))
      labs.x <- labs(x = expression(paste("Frequência Normalizada (x ", pi,
                                          " rad/amostra)")))
      
      grid.arrange(p1 + labs.title, p2, p3 + labs.x, nrow = 3)
}

dataECG.filter <- function(dataECG.split, num.tf, den.tf) {
      x <- lapply(dataECG.split, function(x) x$signal_mag - mean(x$signal_mag))
      x <- sapply(x, filter, filt = num.tf, a = den.tf)
      x <- as.data.frame(x)
      x_norm <- sapply(x, function(x) x <- x/max(abs(x)))
      x_norm <<- as.data.frame(x_norm)
}

dataECG.update <- function(dataECG.split, x_norm) {
      dataECG.aux <- mapply(function(data, signal_mag) data$signal_mag <<- signal_mag,
                            dataECG.split, x_norm)
}

peak.detection <- function(dataECG.updated, samples, Fs = fs) {
      peak.values <- data.frame()
      peak.index <- list()
      
      dataECG.max <- lapply(dataECG.updated,
                            tapply(dataECG.updated$signal_mag[-1], #retirei o primeiro
                                   gl((dim(dataECG.updated)[1]-1)/samples, samples), max))
      #Obs.: aparentemente dataECG.idx está funcionando (verificar!!)
      idx <<- NULL
      dataECG.idx <- mapply(tapply(dataECG.updated$signal_mag[-1], #retirei o primeiro
                                   gl((dim(dataECG.updated)[1]-1)/samples, samples),
                                   function(values, peak = dataECG.max) {
                                         if (is.null(idx)) idx <<- 0 else idx <<- idx + 1
                                         ifelse(values==peak[idx], idx, NA)
                                   }),
                            dataECG.updated, dataECG.max)
      #parei aqui!!
      idx <<- NULL
      dataECG.idx <- lapply(dataECG.idx, lapply(dataECG.idx, function())) )
      
      idx <<- NULL
      aux <- mapply(mapply(function(index, peak) {
                                 if (is.null(idx)) idx <<- 0 else idx <<- idx + 1
                                 
                           },
                           dataECG.idx, as.list(dataECG.max)), dataECG.idx, dataECG.max)
      
      #################teste!!!!!!!!!!!!
      idx <<- 0
      tapply(a, gl(2,5),
             function(x) {
                   idx <<- idx + 1
                   ifelse(x>3, idx, NA)
             })
      #################teste!!!!!!!!!!!!
      k <- 0:(Fs*60/samples)*samples
      for (i in 1:(Fs*60/samples)) {
            if (i == Fs*60/samples) {
                  k[i+1] <- k[i+1] + 1
            }
            peak.intervalAUX <- lapply(updated.dataSplit,
                                       function(x) x$signal_mag[(k[i]+1):k[i+1]])
            peak.valuesAUX <- lapply(peak.intervalAUX,max)
            peak.indexAUX <- mapply(function(peak,intmag) ((k[i]+1):k[1+i])[intmag==peak],
                                    peak.valuesAUX,peak.intervalAUX)
            #Obs.:representar os NAs é realmente importante??!! Na verdade, é devido
            #à estrutura de interação adotada quando da obtenção de peak.values mediante o
            #rbind de um data.frame (isso, por sua vez, gera a mesma necessidade de repre-
            #sentação de NA em peak.index!
            maximum <- 0
            for (l in 1:length(updated.dataSplit)){
                  if (length(peak.indexAUX[[l]]) > maximum) {
                        maximum <- length(peak.indexAUX[[l]])
                  }
            }
            if (maximum > 1) {
                  for (aux in 1:length(updated.dataSplit)) {
                        peak.valuesAUX[[aux]] <- c(rep(peak.valuesAUX[[aux]],
                                                       length(peak.indexAUX[[aux]])),
                                                   rep(NA, maximum - length(peak.indexAUX
                                                                            [[aux]])))
                        peak.indexAUX[[aux]] <- c(peak.indexAUX[[aux]],
                                                  rep(NA,maximum - length(peak.indexAUX
                                                                          [[aux]])))
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


peakDetection <- function(updated.dataSplit, samples, Fs = fs) {
      peak.values <- data.frame()
      peak.index <- list()
      k <- 0:(Fs*60/samples)*samples
      for (i in 1:(Fs*60/samples)) {
            if (i == Fs*60/samples) {
                  k[i+1] <- k[i+1] + 1
            }
            peak.intervalAUX <- lapply(updated.dataSplit,
                                       function(x) x$signal_mag[(k[i]+1):k[i+1]])
            peak.valuesAUX <- lapply(peak.intervalAUX,max)
            peak.indexAUX <- mapply(function(peak,intmag) ((k[i]+1):k[1+i])[intmag==peak],
                                    peak.valuesAUX,peak.intervalAUX)
            maximum <- 0
            for (l in 1:length(updated.dataSplit)){
                  if (length(peak.indexAUX[[l]]) > maximum) {
                        maximum <- length(peak.indexAUX[[l]])
                  }
            }
            if (maximum > 1) {
                  for (aux in 1:length(updated.dataSplit)) {
                        peak.valuesAUX[[aux]] <- c(rep(peak.valuesAUX[[aux]],
                                                       length(peak.indexAUX[[aux]])),
                                                   rep(NA, maximum - length(peak.indexAUX
                                                                            [[aux]])))
                        peak.indexAUX[[aux]] <- c(peak.indexAUX[[aux]],
                                                  rep(NA,maximum - length(peak.indexAUX
                                                                          [[aux]])))
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









