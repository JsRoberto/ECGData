library(signal)
library(ggplot2)

urlFile <- c("https://raw.githubusercontent.com/JsRoberto/ECGData/master/mitdb_ecgSignals.csv",
             "https://raw.githubusercontent.com/JsRoberto/ECGData/master/fs.csv")
localFile <- c("./mitdb_ecgSignals.csv","./fs.csv")

download <- function(urlFile, localFile) {
      if (!exists(localFile)) {
            download.file(urlFile,localFile)
      }
}

mapply(download, urlFile, localFile)

dataECG <- read.csv(localFile[1], header = TRUE)
dataECG.split <- split(dataECG, dataECG$signal_case)
fs <- read.csv(localFile[2], header = TRUE)
fs <- as.numeric(fs)

