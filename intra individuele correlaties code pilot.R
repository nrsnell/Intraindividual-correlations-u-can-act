#Intra individuele correlaties tussen vragen

#setup data frames

#set working directory
setwd("C:/Users/p279841/Google Drive/projecten/u-can-act/archief/Pilot/Data")
#setwd("C:/Users/Nick/Google Drive/projecten/u-can-act/archief/Pilot/Data")

# (variabele aanmaken) Inladen dagboek allvar + lege velden na.strings maken
diary1xdo<-read.csv2("./Dagboek studenten/1x/responses_dagboek_studenten_1x_per_week_donderdag_2017-06-26.csv", na.strings = "")

diary2xma<-read.csv2("./Dagboek studenten/2x/maandag/responses_dagboek_studenten_2x_per_week_maandag_2017-06-26.csv", na.strings = "")
diary2xdo<-read.csv2("./Dagboek studenten/2x/donderdag/responses_dagboek_studenten_2x_per_week_donderdag_2017-06-26.csv", na.strings = "")

diary5xma<-read.csv2("./Dagboek studenten/5x/maandag/responses_dagboek_studenten_5x_per_week_maandag_2017-06-26.csv", na.strings = "")
diary5xdiwovr<-read.csv2("./Dagboek studenten/5x/dinsdagwoensdagvrijdag/responses_dagboek_studenten_5x_per_week_di_wo_vr2017-06-26.csv", na.strings = "")
diary5xdo<-read.csv2("./Dagboek studenten/5x/donderdag/responses_dagboek_studenten_5x_per_week_donderdag_2017-06-26.csv", na.strings = "")


#package ggplot activeren
#install.packages (ggplot2)
#install.packages('psych')
#install.packages('plyr')
#install.packages('data.table')

require(ggplot2)
library(psych)
require(plyr)
require(data.table)

#Samenvoegen alle vragenlijsten in een dataframe

### 1x 1x 1x ###

#Column toevoegen  die aangeeft welke dag de meting heeft plaatsgevonden.
diary1xdo$day <- "do"

#Column toevoegen die aangeeft om welke variant het gaat.
diary1xdo$variant <- "1x"


### 2x 2x 2x ###
#de vragen op de juiste plaatsen neerzetten 2x variant
setnames(diary2xma, old = c("v1", "v1_timing", "v2", "v2_timing", "v3", "v3_timing", "v4", "v4_timing", "v5", "v5_timing", "v6", "v6_timing", "v7_anders_namelijk", "v7_anders_namelijk_text", "v7_anders_namelijk_text_timing", "v7_anders_namelijk_timing", "v7_hobby_sport", "v7_hobby_sport_timing", "v7_romantische_relatie", "v7_romantische_relatie_timing", "v7_thuis", "v7_thuis_timing", "v7_vriendschap", "v7_vriendschap_timing", "v7_werk", "v7_werk_timing", "v8", "v8_timing", "v9", "v9_timing", "v10", "v10_timing", "v11", "v11_timing", "v12", "v12_timing"), new = c("v8", "v8_timing", "v9", "v9_timing", "v10", "v10_timing", "v11", "v11_timing", "v12", "v12_timing", "v13", "v13_timing", "v14_anders_namelijk", "v14_anders_namelijk_text", "v14_anders_namelijk_text_timing", "v14_anders_namelijk_timing", "v14_hobby_sport", "v14_hobby_sport_timing", "v14_romantische_relatie", "v14_romantische_relatie_timing", "v14_thuis", "v14_thuis_timing", "v14_vriendschap", "v14_vriendschap_timing", "v14_werk", "v14_werk_timing", "v15", "v15_timing", "v16", "v16_timing", "v17", "v17_timing", "v18", "v18_timing", "v19", "v19_timing"))

#Column die aangeeft welke dag de meting heeft plaatsgevonden.
diary2xma$day <- "ma"
diary2xdo$day <- "do"

#Column toevoegen die aangeeft om welke variant het gaat.
diary2xma$variant <- "2x"
diary2xdo$variant <- "2x"

##zelfde doen voor 5x
#de vragen op de juiste plaatsen neerzetten
setnames(diary5xma, old = c("v1", "v1_timing", "v2", "v2_timing", "v3", "v3_timing", "v4", "v4_timing", "v5", "v5_timing", "v6", "v6_timing", "v7_anders_namelijk_timing", "v7_hobby_sport", "v7_hobby_sport_timing", "v7_romantische_relatie", "v7_romantische_relatie_timing", "v7_thuis", "v7_thuis_timing", "v7_vriendschap", "v7_vriendschap_timing", "v7_werk", "v7_werk_timing", "v8", "v8_timing", "v9", "v9_timing", "v10", "v10_timing", "v11", "v11_timing", "v12", "v12_timing"), new = c("v8", "v8_timing", "v9", "v9_timing", "v10", "v10_timing", "v11", "v11_timing", "v12", "v12_timing", "v13", "v13_timing", "v14_anders_namelijk_timing", "v14_hobby_sport", "v14_hobby_sport_timing", "v14_romantische_relatie", "v14_romantische_relatie_timing", "v14_thuis", "v14_thuis_timing", "v14_vriendschap", "v14_vriendschap_timing", "v14_werk", "v14_werk_timing", "v15", "v15_timing", "v16", "v16_timing", "v17", "v17_timing", "v18", "v18_timing", "v19", "v19_timing"))

#Column die aangeeft welke dag de meting heeft plaatsgevonden.
diary5xma$day <- "ma"
diary5xdo$day <- "do"
diary5xdiwovr$day <- "di wo vr"

#Column toevoegen die aangeeft om welke variant het gaat.
diary5xma$variant <- "5x"
diary5xdo$variant <- "5x"
diary5xdiwovr$variant <- "5x"

#Samenvoegen data frames
diaryall<-rbind.fill(diary1xdo,diary2xdo,diary2xma,diary5xma,diary5xdiwovr,diary5xdo)

#Set met alleen de juiste schaalvragen

#New dataframe: only questions which are interesting to correlate
#NA column maken
niets <-data.frame(
  na = NA)

niets [240, ] = NA

scale_diaryall<- data.frame(
  "personid" =diaryall$person_id,
  "open_from" =diaryall$open_from,
  "completed_at" =diaryall$completed_at,
  "v2"= diaryall$v2,
  "v3"= diaryall$v3,
  "v4"= diaryall$v4,
  "v5"= diaryall$v5,
  "v6"= diaryall$v6,
  "v7"= diaryall$v7,"v8"= diaryall$v8,
  "v9"= diaryall$v9,
  "v10"= diaryall$v10,
  "v11"= diaryall$v11,
  "v12"= diaryall$v12,
  "v13"= diaryall$v13,
  "v14"= niets$na,
  "v15"= diaryall$v15,
  "v16"= diaryall$v16,
  "v17"= diaryall$v17,
  "v18"= diaryall$v18,
  "v19"= diaryall$v19,
  "v20"= niets$na,
  "v21"= diaryall$v21,
  "v22"= diaryall$v22,
  "v23"= diaryall$v23)

#dataframe maken met juiste colnames

#juiste grote dataframe maken --> namcor
namcor <- data.frame(matrix(NA, nrow = 37, ncol = 23))

########juiste namen invoeren in namcor

## create factor and add everytime an extra name on it
clnam = c()
for(i in 2:23){
  clnam = c(clnam,paste("v2", i, sep = "_"))
}

##Add extra name in front
clnam = c("pp",clnam)

#replace column names of df
colnames(namcor) = clnam

#correlaties vragen op individu niveau per vraag berekenen.
#Werkende functie met ddply, inception loop toevoegen
cor_func <- function(scale_diaryall)
{
  df<-data.frame(NA) 
  for(j in 2:23) {
    x <- scale_diaryall[[paste ("v", j, sep = "")]]
    
    for(i in 2:23) {
      
      y <- scale_diaryall[[paste ("v", i, sep = "")]]
      
      
      m<-cor(x, y, use = "pairwise.complete.obs")
      df<-cbind(df, m)
      
      names(df)[names(df) == 'm'] <- paste("v", j, i, sep = "_")
      
    }    
    
  }
  return(df)
}


#ddply formule
cor_diaryall<-ddply(scale_diaryall, "personid", cor_func)

#correlaties opslaan in csv bestand
write.csv2(cor_diaryall,'correlations_scale_questions.csv')

#### Maak dataframe met gemiddelden van correlaties

means_cor_diaryall <- data.frame (NA, NA)
for(i in 3:486) {
  
  
  n<-mean(cor_diaryall[[i]], na.rm = TRUE)
  
  means_cor_diaryall<-cbind(means_cor_diaryall, n)
}    

colnames(means_cor_diaryall) <- colnames(cor_diaryall)

#df  -0.5 > x > 0.5  
means_no_na <-  means_cor_diaryall[, !is.na(means_cor_diaryall)]
means_cor0.5draft <- means_no_na[, abs(means_no_na) > 0.5]
means_cor0.6draft <- means_no_na[, abs(means_no_na) > 0.5]

means_cor0.5 <- means_cor0.5draft[, !1(means_cor0.5draft)]


################################################################################# TESTING ###############################################################
write.csv2(means_cor_diaryall,'means_correlations_scale_questions.csv')

