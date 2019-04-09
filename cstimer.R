
setwd("~/Github/Cubing")
library(tidyverse)
library(ggrepel)
library(gghighlight)

cube_byte <- read.csv2("./Input/ByteTimer.csv", header = T, sep = ",", 
                  stringsAsFactors = F)

cube_cs <- read.csv2("./Input/csTimerExport.csv", header= T, 
                  stringsAsFactors=F)

cube_cs$Solve <- c(1070:(1070+(nrow(cube_cs)-1))) # giving solve numbers
cube <- bind_rows(cube_byte, cube_cs)
cube$Time <- as.numeric(cube$Time)
str(cube)

cubing <- tibble::tibble(Solve = 1:nrow(cube), Time= cube$Time,
                         AO5 = rep(NA,nrow(cube)), AO12= rep(NA, nrow(cube)),
                         AO50 = rep(NA, nrow(cube)), AO100 = rep(NA, nrow(cube)),
                         AO500 = rep(NA, nrow(cube)), AO1000 = rep(NA, nrow(cube)))

for (i in 5:nrow(cube)) {
     cubing$AO5[i] <- mean(sort(cube$Time[i:(i-4)])[2:4])
}

for (i in 12:nrow(cube)) {
     cubing$AO12[i] <- mean(sort(cube$Time[i:(i-11)])[2:11])
}

for (i in 50:nrow(cube)) {
     cubing$AO50[i] <- mean(sort(cube$Time[i:(i-49)])[3:48])
}

for (i in 100:nrow(cube)) {
     cubing$AO100[i] <- mean(sort(cube$Time[i:(i-99)])[6:95])
}

for (i in 500:nrow(cube)) {
          cubing$AO500[i] <- mean(sort(cube$Time[i:(i-499)])[26:475])
}

for (i in 1000:nrow(cube)) {
          cubing$AO1000[i] <- mean(sort(cube$Time[i:(i-999)])[51:950])
}

cubing <- round(cubing, 2)

cubing_lf <- reshape2::melt(cubing, id= c("Solve", "Time")) 

gghighlight_point(cube, aes(x=Solve, y=Time), Time == min(cube$Time), label_key = Time) +
          geom_line(data= cubing_lf, aes(Solve, Time), alpha= 0.1, colour= "black") +
          geom_line() +
          scale_y_continuous("Time (s)") +
          labs(colour = "") +
          theme_light()

ggplot(cube, aes(Solve, Time)) +
          geom_line(aes(colour= "red"), show.legend = FALSE) +
          geom_point(data=cubing_lf[which.min(cubing_lf$Time),], aes(x=Solve, y=Time), size= 1) +
          geom_text_repel(data= cubing_lf[which.min(cubing_lf$Time),], label= min(cubing_lf$Time)) +
          scale_y_continuous("Time (s)") +
          labs(colour = "") +
          theme_light() 

ggplot(cubing_lf, aes(Solve, value, colour= variable)) +
          geom_line(data= cubing_lf, aes(Solve, Time), alpha= 0.1, colour= "black") +
          geom_line() +
          geom_point(data=cubing_lf[which.min(cubing_lf$Time),], aes(x=Solve, y=Time), size= 2) +
          scale_y_continuous("Time (s)") +
          labs(colour = "") +
          theme_light() +
          facet_wrap(~ variable)

ggplot(cubing_lf, aes(Time)) +
          geom_histogram() + 
          theme_light()
          
ggplot(cubing_lf, aes(Solve, Time)) +
          geom_violin() + 
          scale_y_continuous("Time (s)") +
          theme_light()
