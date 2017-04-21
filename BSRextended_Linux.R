library(ggplot2)
#library(reshape2)
library(ggthemes)
library(gridExtra)
library(grid)
library(tikzDevice)
library(dplyr)
library(stargazer)
library(ggrepel)

############################################################
# LOAD DATA
############################################################

# Load Data from WD

setwd("/home/martin/Google/[Uni]/PoWi/[PhD]/[Thesis]/[Data]/Economic BSR")
DFEco <- read.csv("BIG_DATA_FRAME_ALL_DATA_FINAL.csv", sep = ",")
Norms <- read.csv("/home/martin/Google/[Uni]/PoWi/[PhD]/[Thesis]/[Data]/Economic BSR/norms_bsr.csv", sep = "")
BSR_GDP <- read.csv("/home/martin/Google/[Uni]/PoWi/[PhD]/[Thesis]/[Data]/Economic BSR/BSR_GDP_Update.csv", sep = "")
BSRComHege <- read.csv("BSRComHege.csv", sep = "")
BSRContiguity <- read.csv("BSRContiguity.csv", sep = "")
BSRCultur <- read.csv("BSRCultur.csv", sep = "")
BSRDistance <- read.csv("BSRDistance.csv", sep = "")
BSRHegeRela <- read.csv("BSRHegeRela.csv", sep = "")
BSRPolitySC2 <- read.csv("BSRPolitySC2.csv", sep = "")

# Plotting the norms + tikzdevice
tikz(file = "Illustrations/Norms_sum.tex")
ggplot(Norms, aes(Norms$year)) + geom_line(aes(y = Norms$ba_cum, col = "BA")) + geom_line(aes(y = Norms$cbss_cum, col = "CBSS")) + geom_line(aes(y = Norms$eu_cum, col = "EU")) + geom_line(aes(y = Norms$norden_cum, col = "Norden")) + scale_color_manual(name="Legend", values = c("BA" = "#021199", "CBSS" = "#02adc6", "EU" = "#c6026f", "Norden" = "#E88989")) + xlab("Year") + ylab("Amount of Norm containing Agreements")
dev.off()

# Just a test for the norms: NA values will be set to 0, in order to test long-term effects
# Norms2 <- Norms
# Norms2[is.na(Norms2)] <- 0

############################################################
# PREPROCESSING
############################################################

# Converting GDP values from $US 2011 to $US 2010
# BSR_GDP <- BSR_GDP/1.032

# Keeping all wanted columns
# DFEco_clear <- DFEco[c(2:8,15)]

# Adding the sources for variables
namevector1 <- c("source_trade")
DFEco_clear[,namevector1] <- 1
namevector2 <- c("source_pop")
DFEco_clear[,namevector2] <- 2
namevector3 <- c("source_gdp")
DFEco_clear[,namevector3] <- 4

# Creating single dyadic files from main DF
# Denmark
DEN_EST <- DFEco_clear %>% filter(acra == "DEN", acrb %in% "EST")
DEN_FIN <- DFEco_clear %>% filter(acra == "DEN", acrb %in% "FIN")
DEN_GFR <- DFEco_clear %>% filter(acra == "DEN", acrb %in% "GFR")
DEN_LAT <- DFEco_clear %>% filter(acra == "DEN", acrb %in% "LAT")
DEN_LIT <- DFEco_clear %>% filter(acra == "DEN", acrb %in% "LIT")
DEN_POL <- DFEco_clear %>% filter(acra == "DEN", acrb %in% "POL")
DEN_RUS <- DFEco_clear %>% filter(acra == "DEN", acrb %in% "RUS")
DEN_SWD <- DFEco_clear %>% filter(acra == "DEN", acrb %in% "SWD")
# Estonia
EST_DEN <- DFEco_clear %>% filter(acra == "EST", acrb %in% "DEN")
EST_FIN <- DFEco_clear %>% filter(acra == "EST", acrb %in% "FIN")
EST_GFR <- DFEco_clear %>% filter(acra == "EST", acrb %in% "GFR")
EST_LAT <- DFEco_clear %>% filter(acra == "EST", acrb %in% "LAT")
EST_LIT <- DFEco_clear %>% filter(acra == "EST", acrb %in% "LIT")
EST_POL <- DFEco_clear %>% filter(acra == "EST", acrb %in% "POL")
EST_RUS <- DFEco_clear %>% filter(acra == "EST", acrb %in% "RUS")
EST_SWD <- DFEco_clear %>% filter(acra == "EST", acrb %in% "SWD")
# Finland
FIN_DEN <- DFEco_clear %>% filter(acra == "FIN", acrb %in% "DEN")
FIN_EST <- DFEco_clear %>% filter(acra == "FIN", acrb %in% "EST")
FIN_GFR <- DFEco_clear %>% filter(acra == "FIN", acrb %in% "GFR")
FIN_LAT <- DFEco_clear %>% filter(acra == "FIN", acrb %in% "LAT")
FIN_LIT <- DFEco_clear %>% filter(acra == "FIN", acrb %in% "LIT")
FIN_POL <- DFEco_clear %>% filter(acra == "FIN", acrb %in% "POL")
FIN_RUS <- DFEco_clear %>% filter(acra == "FIN", acrb %in% "RUS")
FIN_SWD <- DFEco_clear %>% filter(acra == "FIN", acrb %in% "SWD")
# Germany
GFR_DEN <- DFEco_clear %>% filter(acra == "GFR", acrb %in% "DEN")
GFR_EST <- DFEco_clear %>% filter(acra == "GFR", acrb %in% "EST")
GFR_FIN <- DFEco_clear %>% filter(acra == "GFR", acrb %in% "FIN")
GFR_LAT <- DFEco_clear %>% filter(acra == "GFR", acrb %in% "LAT")
GFR_LIT <- DFEco_clear %>% filter(acra == "GFR", acrb %in% "LIT")
GFR_POL <- DFEco_clear %>% filter(acra == "GFR", acrb %in% "POL")
GFR_RUS <- DFEco_clear %>% filter(acra == "GFR", acrb %in% "RUS")
GFR_SWD <- DFEco_clear %>% filter(acra == "GFR", acrb %in% "SWD")
# Latvia
LAT_DEN <- DFEco_clear %>% filter(acra == "LAT", acrb %in% "DEN")
LAT_EST <- DFEco_clear %>% filter(acra == "LAT", acrb %in% "EST")
LAT_FIN <- DFEco_clear %>% filter(acra == "LAT", acrb %in% "FIN")
LAT_GFR <- DFEco_clear %>% filter(acra == "LAT", acrb %in% "GFR")
LAT_LIT <- DFEco_clear %>% filter(acra == "LAT", acrb %in% "LIT")
LAT_POL <- DFEco_clear %>% filter(acra == "LAT", acrb %in% "POL")
LAT_RUS <- DFEco_clear %>% filter(acra == "LAT", acrb %in% "RUS")
LAT_SWD <- DFEco_clear %>% filter(acra == "LAT", acrb %in% "SWD")
# Lithuania
LIT_DEN <- DFEco_clear %>% filter(acra == "LIT", acrb %in% "DEN")
LIT_EST <- DFEco_clear %>% filter(acra == "LIT", acrb %in% "EST")
LIT_FIN <- DFEco_clear %>% filter(acra == "LIT", acrb %in% "FIN")
LIT_GFR <- DFEco_clear %>% filter(acra == "LIT", acrb %in% "GFR")
LIT_LAT <- DFEco_clear %>% filter(acra == "LIT", acrb %in% "LAT")
LIT_POL <- DFEco_clear %>% filter(acra == "LIT", acrb %in% "POL")
LIT_RUS <- DFEco_clear %>% filter(acra == "LIT", acrb %in% "RUS")
LIT_SWD <- DFEco_clear %>% filter(acra == "LIT", acrb %in% "SWD")
# Poland
POL_DEN <- DFEco_clear %>% filter(acra == "POL", acrb %in% "DEN")
POL_EST <- DFEco_clear %>% filter(acra == "POL", acrb %in% "EST")
POL_FIN <- DFEco_clear %>% filter(acra == "POL", acrb %in% "FIN")
POL_GFR <- DFEco_clear %>% filter(acra == "POL", acrb %in% "GFR")
POL_LAT <- DFEco_clear %>% filter(acra == "POL", acrb %in% "LAT")
POL_LIT <- DFEco_clear %>% filter(acra == "POL", acrb %in% "LIT")
POL_RUS <- DFEco_clear %>% filter(acra == "POL", acrb %in% "RUS")
POL_SWD <- DFEco_clear %>% filter(acra == "POL", acrb %in% "SWD")
# Russia
RUS_DEN <- DFEco_clear %>% filter(acra == "RUS", acrb %in% "DEN")
RUS_EST <- DFEco_clear %>% filter(acra == "RUS", acrb %in% "EST")
RUS_FIN <- DFEco_clear %>% filter(acra == "RUS", acrb %in% "FIN")
RUS_GFR <- DFEco_clear %>% filter(acra == "RUS", acrb %in% "GFR")
RUS_LAT <- DFEco_clear %>% filter(acra == "RUS", acrb %in% "LAT")
RUS_LIT <- DFEco_clear %>% filter(acra == "RUS", acrb %in% "LIT")
RUS_POL <- DFEco_clear %>% filter(acra == "RUS", acrb %in% "POL")
RUS_SWD <- DFEco_clear %>% filter(acra == "RUS", acrb %in% "SWD")
# Sweden
SWD_DEN <- DFEco_clear %>% filter(acra == "SWD", acrb %in% "DEN")
SWD_EST <- DFEco_clear %>% filter(acra == "SWD", acrb %in% "EST")
SWD_FIN <- DFEco_clear %>% filter(acra == "SWD", acrb %in% "FIN")
SWD_GFR <- DFEco_clear %>% filter(acra == "SWD", acrb %in% "GFR")
SWD_LAT <- DFEco_clear %>% filter(acra == "SWD", acrb %in% "LAT")
SWD_LIT <- DFEco_clear %>% filter(acra == "SWD", acrb %in% "LIT")
SWD_POL <- DFEco_clear %>% filter(acra == "SWD", acrb %in% "POL")
SWD_RUS <- DFEco_clear %>% filter(acra == "SWD", acrb %in% "RUS")

#Exporting Data in Global Environment as .csv
list_df <- mget(ls()[])
lapply(seq_along(list_df),
       function(i) write.table(list_df[[i]],
                               paste0(names(list_df)[i], ".csv"),
                               row.names = FALSE, quote = FALSE,
                               sep = " ", dec = "."))

# Extracting population variable from expdata (Gleditsch)
pop_DEN <- expdata %>% filter(acra == "DEN", acrb %in% "ICE")
pop_DEN <- pop_DEN[c(5,14)]
pop_EST <- expdata %>% filter(acra == "EST", acrb %in% "LAT")
pop_EST <- pop_EST[c(5,14)]
pop_FIN <- expdata %>% filter(acra == "FIN", acrb %in% "SWD")
pop_FIN <- pop_FIN[c(5,14)]
pop_GFR <- expdata %>% filter(acra == "GFR", acrb %in% "FIN")
pop_GFR <- pop_GFR[c(5,14)]
pop_LAT <- expdata %>% filter(acra == "LAT", acrb %in% "LIT")
pop_LAT <- pop_LAT[c(5,14)]
pop_LIT <- expdata %>% filter(acra == "LIT", acrb %in% "UKR")
pop_LIT <- pop_LIT[c(5,14)]
pop_POL <- expdata %>% filter(acra == "POL", acrb %in% "ITA")
pop_POL <- pop_POL[c(5,14)]
pop_RUS <- expdata %>% filter(acra == "RUS", acrb %in% "FIN")
pop_RUS <- pop_RUS[c(5,14)]
pop_SWD <- expdata %>% filter(acra == "SWD", acrb %in% "NOR")
pop_SWD <- pop_SWD[c(5,14)]

# load dyadic state data from subset folder
setwd(dir = "/home/martin/Google/[Uni]/PoWi/[PhD]/[Thesis]/[Data]/Economic BSR/Subsets/")
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i], sep = " "))

# Combining GDP and dyadic subset
# Denmark
DEN_EST <- cbind(DEN_EST.csv, BSR_GDP$Denmark)
DEN_FIN <- cbind(DEN_FIN.csv, BSR_GDP$Denmark)
DEN_GFR <- cbind(DEN_GFR.csv, BSR_GDP$Denmark)
DEN_LAT <- cbind(DEN_LAT.csv, BSR_GDP$Denmark)
DEN_LIT <- cbind(DEN_LIT.csv, BSR_GDP$Denmark)
DEN_POL <- cbind(DEN_POL.csv, BSR_GDP$Denmark)
DEN_RUS <- cbind(DEN_RUS.csv, BSR_GDP$Denmark)
DEN_SWD <- cbind(DEN_SWD.csv, BSR_GDP$Denmark)
# Estonia
EST_DEN <- cbind(EST_DEN.csv, BSR_GDP$Estonia)
EST_FIN <- cbind(EST_FIN.csv, BSR_GDP$Estonia)
EST_GFR <- cbind(EST_GFR.csv, BSR_GDP$Estonia)
EST_LAT <- cbind(EST_LAT.csv, BSR_GDP$Estonia)
EST_LIT <- cbind(EST_LIT.csv, BSR_GDP$Estonia)
EST_POL <- cbind(EST_POL.csv, BSR_GDP$Estonia)
EST_RUS <- cbind(EST_RUS.csv, BSR_GDP$Estonia)
EST_SWD <- cbind(EST_SWD.csv, BSR_GDP$Estonia)
# Finland
FIN_DEN <- cbind(FIN_DEN.csv, BSR_GDP$Finland)
FIN_EST <- cbind(FIN_EST.csv, BSR_GDP$Finland)
FIN_GFR <- cbind(FIN_GFR.csv, BSR_GDP$Finland)
FIN_LAT <- cbind(FIN_LAT.csv, BSR_GDP$Finland)
FIN_LIT <- cbind(FIN_LIT.csv, BSR_GDP$Finland)
FIN_POL <- cbind(FIN_POL.csv, BSR_GDP$Finland)
FIN_RUS <- cbind(FIN_RUS.csv, BSR_GDP$Finland)
FIN_SWD <- cbind(FIN_SWD.csv, BSR_GDP$Finland)
# Germany
GFR_DEN <- cbind(GFR_DEN.csv, BSR_GDP$Germany)
GFR_EST <- cbind(GFR_EST.csv, BSR_GDP$Germany)
GFR_FIN <- cbind(GFR_FIN.csv, BSR_GDP$Germany)
GFR_LAT <- cbind(GFR_LAT.csv, BSR_GDP$Germany)
GFR_LIT <- cbind(GFR_LIT.csv, BSR_GDP$Germany)
GFR_POL <- cbind(GFR_POL.csv, BSR_GDP$Germany)
GFR_RUS <- cbind(GFR_RUS.csv, BSR_GDP$Germany)
GFR_SWD <- cbind(GFR_SWD.csv, BSR_GDP$Germany)
# Latvia
LAT_DEN <- cbind(LAT_DEN.csv, BSR_GDP$Latvia)
LAT_EST <- cbind(LAT_EST.csv, BSR_GDP$Latvia)
LAT_FIN <- cbind(LAT_FIN.csv, BSR_GDP$Latvia)
LAT_GFR <- cbind(LAT_GFR.csv, BSR_GDP$Latvia)
LAT_LIT <- cbind(LAT_LIT.csv, BSR_GDP$Latvia)
LAT_POL <- cbind(LAT_POL.csv, BSR_GDP$Latvia)
LAT_RUS <- cbind(LAT_RUS.csv, BSR_GDP$Latvia)
LAT_SWD <- cbind(LAT_SWD.csv, BSR_GDP$Latvia)
# Lithuania
LIT_DEN <- cbind(LIT_DEN.csv, BSR_GDP$Lithuania)
LIT_EST <- cbind(LIT_EST.csv, BSR_GDP$Lithuania)
LIT_FIN <- cbind(LIT_FIN.csv, BSR_GDP$Lithuania)
LIT_GFR <- cbind(LIT_GFR.csv, BSR_GDP$Lithuania)
LIT_LAT <- cbind(LIT_LAT.csv, BSR_GDP$Lithuania)
LIT_POL <- cbind(LIT_POL.csv, BSR_GDP$Lithuania)
LIT_RUS <- cbind(LIT_RUS.csv, BSR_GDP$Lithuania)
LIT_SWD <- cbind(LIT_SWD.csv, BSR_GDP$Lithuania)
# Poland
POL_DEN <- cbind(POL_DEN.csv, BSR_GDP$Poland)
POL_EST <- cbind(POL_EST.csv, BSR_GDP$Poland)
POL_FIN <- cbind(POL_FIN.csv, BSR_GDP$Poland)
POL_GFR <- cbind(POL_GFR.csv, BSR_GDP$Poland)
POL_LAT <- cbind(POL_LAT.csv, BSR_GDP$Poland)
POL_LIT <- cbind(POL_LIT.csv, BSR_GDP$Poland)
POL_RUS <- cbind(POL_RUS.csv, BSR_GDP$Poland)
POL_SWD <- cbind(POL_SWD.csv, BSR_GDP$Poland)
# Russia
RUS_DEN <- cbind(RUS_DEN.csv, BSR_GDP$Russia)
RUS_EST <- cbind(RUS_EST.csv, BSR_GDP$Russia)
RUS_FIN <- cbind(RUS_FIN.csv, BSR_GDP$Russia)
RUS_GFR <- cbind(RUS_GFR.csv, BSR_GDP$Russia)
RUS_LAT <- cbind(RUS_LAT.csv, BSR_GDP$Russia)
RUS_LIT <- cbind(RUS_LIT.csv, BSR_GDP$Russia)
RUS_POL <- cbind(RUS_POL.csv, BSR_GDP$Russia)
RUS_SWD <- cbind(RUS_SWD.csv, BSR_GDP$Russia)
# Sweden
SWD_DEN <- cbind(SWD_DEN.csv, BSR_GDP$Sweden)
SWD_EST <- cbind(SWD_EST.csv, BSR_GDP$Sweden)
SWD_FIN <- cbind(SWD_FIN.csv, BSR_GDP$Sweden)
SWD_GFR <- cbind(SWD_GFR.csv, BSR_GDP$Sweden)
SWD_LAT <- cbind(SWD_LAT.csv, BSR_GDP$Sweden)
SWD_LIT <- cbind(SWD_LIT.csv, BSR_GDP$Sweden)
SWD_POL <- cbind(SWD_POL.csv, BSR_GDP$Sweden)
SWD_RUS <- cbind(SWD_RUS.csv, BSR_GDP$Sweden)

# Renaming the columns in order to combine all data frames
colnames(DEN_EST)[12] <- "GDP"
colnames(DEN_FIN)[12] <- "GDP"
colnames(DEN_GFR)[12] <- "GDP"
colnames(DEN_LAT)[12] <- "GDP"
colnames(DEN_LIT)[12] <- "GDP"
colnames(DEN_POL)[12] <- "GDP"
colnames(DEN_RUS)[12] <- "GDP"
colnames(DEN_SWD)[12] <- "GDP"
colnames(EST_DEN)[12] <- "GDP"
colnames(EST_FIN)[12] <- "GDP"
colnames(EST_GFR)[12] <- "GDP"
colnames(EST_LAT)[12] <- "GDP"
colnames(EST_LIT)[12] <- "GDP"
colnames(EST_POL)[12] <- "GDP"
colnames(EST_RUS)[12] <- "GDP"
colnames(EST_SWD)[12] <- "GDP"
colnames(FIN_DEN)[12] <- "GDP"
colnames(FIN_EST)[12] <- "GDP"
colnames(FIN_GFR)[12] <- "GDP"
colnames(FIN_LAT)[12] <- "GDP"
colnames(FIN_LIT)[12] <- "GDP"
colnames(FIN_POL)[12] <- "GDP"
colnames(FIN_RUS)[12] <- "GDP"
colnames(FIN_SWD)[12] <- "GDP"
colnames(GFR_DEN)[12] <- "GDP"
colnames(GFR_EST)[12] <- "GDP"
colnames(GFR_FIN)[12] <- "GDP"
colnames(GFR_LAT)[12] <- "GDP"
colnames(GFR_LIT)[12] <- "GDP"
colnames(GFR_POL)[12] <- "GDP"
colnames(GFR_RUS)[12] <- "GDP"
colnames(GFR_SWD)[12] <- "GDP"
colnames(LAT_DEN)[12] <- "GDP"
colnames(LAT_EST)[12] <- "GDP"
colnames(LAT_FIN)[12] <- "GDP"
colnames(LAT_GFR)[12] <- "GDP"
colnames(LAT_LIT)[12] <- "GDP"
colnames(LAT_POL)[12] <- "GDP"
colnames(LAT_RUS)[12] <- "GDP"
colnames(LAT_SWD)[12] <- "GDP"
colnames(LIT_DEN)[12] <- "GDP"
colnames(LIT_EST)[12] <- "GDP"
colnames(LIT_FIN)[12] <- "GDP"
colnames(LIT_GFR)[12] <- "GDP"
colnames(LIT_LAT)[12] <- "GDP"
colnames(LIT_POL)[12] <- "GDP"
colnames(LIT_RUS)[12] <- "GDP"
colnames(LIT_SWD)[12] <- "GDP"
colnames(POL_DEN)[12] <- "GDP"
colnames(POL_EST)[12] <- "GDP"
colnames(POL_FIN)[12] <- "GDP"
colnames(POL_GFR)[12] <- "GDP"
colnames(POL_LAT)[12] <- "GDP"
colnames(POL_LIT)[12] <- "GDP"
colnames(POL_RUS)[12] <- "GDP"
colnames(POL_SWD)[12] <- "GDP"
colnames(RUS_DEN)[12] <- "GDP"
colnames(RUS_EST)[12] <- "GDP"
colnames(RUS_FIN)[12] <- "GDP"
colnames(RUS_GFR)[12] <- "GDP"
colnames(RUS_LAT)[12] <- "GDP"
colnames(RUS_LIT)[12] <- "GDP"
colnames(RUS_POL)[12] <- "GDP"
colnames(RUS_SWD)[12] <- "GDP"
colnames(SWD_DEN)[12] <- "GDP"
colnames(SWD_EST)[12] <- "GDP"
colnames(SWD_FIN)[12] <- "GDP"
colnames(SWD_GFR)[12] <- "GDP"
colnames(SWD_LAT)[12] <- "GDP"
colnames(SWD_LIT)[12] <- "GDP"
colnames(SWD_POL)[12] <- "GDP"
colnames(SWD_RUS)[12] <- "GDP"

# Combining PolitySC2 and dyadic subset
# Denmark
DEN_EST <- cbind(DEN_EST, BSRPolitySC2$DEN)
DEN_FIN <- cbind(DEN_FIN, BSRPolitySC2$DEN)
DEN_GFR <- cbind(DEN_GFR, BSRPolitySC2$DEN)
DEN_LAT <- cbind(DEN_LAT, BSRPolitySC2$DEN)
DEN_LIT <- cbind(DEN_LIT, BSRPolitySC2$DEN)
DEN_POL <- cbind(DEN_POL, BSRPolitySC2$DEN)
DEN_RUS <- cbind(DEN_RUS, BSRPolitySC2$DEN)
DEN_SWD <- cbind(DEN_SWD, BSRPolitySC2$DEN)
# Estonia
EST_DEN <- cbind(EST_DEN, BSRPolitySC2$EST)
EST_FIN <- cbind(EST_FIN, BSRPolitySC2$EST)
EST_GFR <- cbind(EST_GFR, BSRPolitySC2$EST)
EST_LAT <- cbind(EST_LAT, BSRPolitySC2$EST)
EST_LIT <- cbind(EST_LIT, BSRPolitySC2$EST)
EST_POL <- cbind(EST_POL, BSRPolitySC2$EST)
EST_RUS <- cbind(EST_RUS, BSRPolitySC2$EST)
EST_SWD <- cbind(EST_SWD, BSRPolitySC2$EST)
# Finland
FIN_DEN <- cbind(FIN_DEN, BSRPolitySC2$FIN)
FIN_EST <- cbind(FIN_EST, BSRPolitySC2$FIN)
FIN_GFR <- cbind(FIN_GFR, BSRPolitySC2$FIN)
FIN_LAT <- cbind(FIN_LAT, BSRPolitySC2$FIN)
FIN_LIT <- cbind(FIN_LIT, BSRPolitySC2$FIN)
FIN_POL <- cbind(FIN_POL, BSRPolitySC2$FIN)
FIN_RUS <- cbind(FIN_RUS, BSRPolitySC2$FIN)
FIN_SWD <- cbind(FIN_SWD, BSRPolitySC2$FIN)
# Germany
GFR_DEN <- cbind(GFR_DEN, BSRPolitySC2$GFR)
GFR_EST <- cbind(GFR_EST, BSRPolitySC2$GFR)
GFR_FIN <- cbind(GFR_FIN, BSRPolitySC2$GFR)
GFR_LAT <- cbind(GFR_LAT, BSRPolitySC2$GFR)
GFR_LIT <- cbind(GFR_LIT, BSRPolitySC2$GFR)
GFR_POL <- cbind(GFR_POL, BSRPolitySC2$GFR)
GFR_RUS <- cbind(GFR_RUS, BSRPolitySC2$GFR)
GFR_SWD <- cbind(GFR_SWD, BSRPolitySC2$GFR)
# Latvia
LAT_DEN <- cbind(LAT_DEN, BSRPolitySC2$LAT)
LAT_EST <- cbind(LAT_EST, BSRPolitySC2$LAT)
LAT_FIN <- cbind(LAT_FIN, BSRPolitySC2$LAT)
LAT_GFR <- cbind(LAT_GFR, BSRPolitySC2$LAT)
LAT_LIT <- cbind(LAT_LIT, BSRPolitySC2$LAT)
LAT_POL <- cbind(LAT_POL, BSRPolitySC2$LAT)
LAT_RUS <- cbind(LAT_RUS, BSRPolitySC2$LAT)
LAT_SWD <- cbind(LAT_SWD, BSRPolitySC2$LAT)
# Lithuania
LIT_DEN <- cbind(LIT_DEN, BSRPolitySC2$LIT)
LIT_EST <- cbind(LIT_EST, BSRPolitySC2$LIT)
LIT_FIN <- cbind(LIT_FIN, BSRPolitySC2$LIT)
LIT_GFR <- cbind(LIT_GFR, BSRPolitySC2$LIT)
LIT_LAT <- cbind(LIT_LAT, BSRPolitySC2$LIT)
LIT_POL <- cbind(LIT_POL, BSRPolitySC2$LIT)
LIT_RUS <- cbind(LIT_RUS, BSRPolitySC2$LIT)
LIT_SWD <- cbind(LIT_SWD, BSRPolitySC2$LIT)
# Poland
POL_DEN <- cbind(POL_DEN, BSRPolitySC2$POL)
POL_EST <- cbind(POL_EST, BSRPolitySC2$POL)
POL_FIN <- cbind(POL_FIN, BSRPolitySC2$POL)
POL_GFR <- cbind(POL_GFR, BSRPolitySC2$POL)
POL_LAT <- cbind(POL_LAT, BSRPolitySC2$POL)
POL_LIT <- cbind(POL_LIT, BSRPolitySC2$POL)
POL_RUS <- cbind(POL_RUS, BSRPolitySC2$POL)
POL_SWD <- cbind(POL_SWD, BSRPolitySC2$POL)
# Russia
RUS_DEN <- cbind(RUS_DEN, BSRPolitySC2$RUS)
RUS_EST <- cbind(RUS_EST, BSRPolitySC2$RUS)
RUS_FIN <- cbind(RUS_FIN, BSRPolitySC2$RUS)
RUS_GFR <- cbind(RUS_GFR, BSRPolitySC2$RUS)
RUS_LAT <- cbind(RUS_LAT, BSRPolitySC2$RUS)
RUS_LIT <- cbind(RUS_LIT, BSRPolitySC2$RUS)
RUS_POL <- cbind(RUS_POL, BSRPolitySC2$RUS)
RUS_SWD <- cbind(RUS_SWD, BSRPolitySC2$RUS)
# Sweden
SWD_DEN <- cbind(SWD_DEN, BSRPolitySC2$SWD)
SWD_EST <- cbind(SWD_EST, BSRPolitySC2$SWD)
SWD_FIN <- cbind(SWD_FIN, BSRPolitySC2$SWD)
SWD_GFR <- cbind(SWD_GFR, BSRPolitySC2$SWD)
SWD_LAT <- cbind(SWD_LAT, BSRPolitySC2$SWD)
SWD_LIT <- cbind(SWD_LIT, BSRPolitySC2$SWD)
SWD_POL <- cbind(SWD_POL, BSRPolitySC2$SWD)
SWD_RUS <- cbind(SWD_RUS, BSRPolitySC2$SWD)

# Renaming the columns in order to combine all data frames
colnames(DEN_EST)[17] <- "PolitySC2"
colnames(DEN_FIN)[17] <- "PolitySC2"
colnames(DEN_GFR)[17] <- "PolitySC2"
colnames(DEN_LAT)[17] <- "PolitySC2"
colnames(DEN_LIT)[17] <- "PolitySC2"
colnames(DEN_POL)[17] <- "PolitySC2"
colnames(DEN_RUS)[17] <- "PolitySC2"
colnames(DEN_SWD)[17] <- "PolitySC2"
colnames(EST_DEN)[17] <- "PolitySC2"
colnames(EST_FIN)[17] <- "PolitySC2"
colnames(EST_GFR)[17] <- "PolitySC2"
colnames(EST_LAT)[17] <- "PolitySC2"
colnames(EST_LIT)[17] <- "PolitySC2"
colnames(EST_POL)[17] <- "PolitySC2"
colnames(EST_RUS)[17] <- "PolitySC2"
colnames(EST_SWD)[17] <- "PolitySC2"
colnames(FIN_DEN)[17] <- "PolitySC2"
colnames(FIN_EST)[17] <- "PolitySC2"
colnames(FIN_GFR)[17] <- "PolitySC2"
colnames(FIN_LAT)[17] <- "PolitySC2"
colnames(FIN_LIT)[17] <- "PolitySC2"
colnames(FIN_POL)[17] <- "PolitySC2"
colnames(FIN_RUS)[17] <- "PolitySC2"
colnames(FIN_SWD)[17] <- "PolitySC2"
colnames(GFR_DEN)[17] <- "PolitySC2"
colnames(GFR_EST)[17] <- "PolitySC2"
colnames(GFR_FIN)[17] <- "PolitySC2"
colnames(GFR_LAT)[17] <- "PolitySC2"
colnames(GFR_LIT)[17] <- "PolitySC2"
colnames(GFR_POL)[17] <- "PolitySC2"
colnames(GFR_RUS)[17] <- "PolitySC2"
colnames(GFR_SWD)[17] <- "PolitySC2"
colnames(LAT_DEN)[17] <- "PolitySC2"
colnames(LAT_EST)[17] <- "PolitySC2"
colnames(LAT_FIN)[17] <- "PolitySC2"
colnames(LAT_GFR)[17] <- "PolitySC2"
colnames(LAT_LIT)[17] <- "PolitySC2"
colnames(LAT_POL)[17] <- "PolitySC2"
colnames(LAT_RUS)[17] <- "PolitySC2"
colnames(LAT_SWD)[17] <- "PolitySC2"
colnames(LIT_DEN)[17] <- "PolitySC2"
colnames(LIT_EST)[17] <- "PolitySC2"
colnames(LIT_FIN)[17] <- "PolitySC2"
colnames(LIT_GFR)[17] <- "PolitySC2"
colnames(LIT_LAT)[17] <- "PolitySC2"
colnames(LIT_POL)[17] <- "PolitySC2"
colnames(LIT_RUS)[17] <- "PolitySC2"
colnames(LIT_SWD)[17] <- "PolitySC2"
colnames(POL_DEN)[17] <- "PolitySC2"
colnames(POL_EST)[17] <- "PolitySC2"
colnames(POL_FIN)[17] <- "PolitySC2"
colnames(POL_GFR)[17] <- "PolitySC2"
colnames(POL_LAT)[17] <- "PolitySC2"
colnames(POL_LIT)[17] <- "PolitySC2"
colnames(POL_RUS)[17] <- "PolitySC2"
colnames(POL_SWD)[17] <- "PolitySC2"
colnames(RUS_DEN)[17] <- "PolitySC2"
colnames(RUS_EST)[17] <- "PolitySC2"
colnames(RUS_FIN)[17] <- "PolitySC2"
colnames(RUS_GFR)[17] <- "PolitySC2"
colnames(RUS_LAT)[17] <- "PolitySC2"
colnames(RUS_LIT)[17] <- "PolitySC2"
colnames(RUS_POL)[17] <- "PolitySC2"
colnames(RUS_SWD)[17] <- "PolitySC2"
colnames(SWD_DEN)[17] <- "PolitySC2"
colnames(SWD_EST)[17] <- "PolitySC2"
colnames(SWD_FIN)[17] <- "PolitySC2"
colnames(SWD_GFR)[17] <- "PolitySC2"
colnames(SWD_LAT)[17] <- "PolitySC2"
colnames(SWD_LIT)[17] <- "PolitySC2"
colnames(SWD_POL)[17] <- "PolitySC2"
colnames(SWD_RUS)[17] <- "PolitySC2"

# Include Distance to ubsets. this works for other matrices as well

DEN_EST <- cbind(DEN_EST, rep(BSRDistance$DEN[2], length=nrow(DEN_EST)))
colnames(DEN_EST)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
DEN_FIN <- cbind(DEN_FIN, rep(BSRDistance$DEN[3], length=nrow(DEN_EST)))
colnames(DEN_FIN)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
DEN_GFR <- cbind(DEN_GFR, rep(BSRDistance$DEN[4], length=nrow(DEN_EST)))
colnames(DEN_GFR)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
DEN_LAT <- cbind(DEN_LAT, rep(BSRDistance$DEN[5], length=nrow(DEN_EST)))
colnames(DEN_LAT)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
DEN_LIT <- cbind(DEN_LIT, rep(BSRDistance$DEN[6], length=nrow(DEN_EST)))
colnames(DEN_LIT)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
DEN_POL <- cbind(DEN_POL, rep(BSRDistance$DEN[7], length=nrow(DEN_EST)))
colnames(DEN_POL)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
DEN_RUS <- cbind(DEN_RUS, rep(BSRDistance$DEN[8], length=nrow(DEN_EST)))
colnames(DEN_RUS)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
DEN_SWD <- cbind(DEN_SWD, rep(BSRDistance$DEN[9], length=nrow(DEN_EST)))
colnames(DEN_SWD)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
# Estonia
EST_DEN <- cbind(EST_DEN, rep(BSRDistance$EST[1], length=nrow(DEN_EST)))
colnames(EST_DEN)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
EST_FIN <- cbind(EST_FIN, rep(BSRDistance$EST[3], length=nrow(DEN_EST)))
colnames(EST_FIN)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
EST_GFR <- cbind(EST_GFR, rep(BSRDistance$EST[4], length=nrow(DEN_EST)))
colnames(EST_GFR)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
EST_LAT <- cbind(EST_LAT, rep(BSRDistance$EST[5], length=nrow(DEN_EST)))
colnames(EST_LAT)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
EST_LIT <- cbind(EST_LIT, rep(BSRDistance$EST[6], length=nrow(DEN_EST)))
colnames(EST_LIT)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
EST_POL <- cbind(EST_POL, rep(BSRDistance$EST[7], length=nrow(DEN_EST)))
colnames(EST_POL)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
EST_RUS <- cbind(EST_RUS, rep(BSRDistance$EST[8], length=nrow(DEN_EST)))
colnames(EST_RUS)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
EST_SWD <- cbind(EST_SWD, rep(BSRDistance$EST[9], length=nrow(DEN_EST)))
colnames(EST_SWD)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
# Finland
FIN_DEN <- cbind(FIN_DEN, rep(BSRDistance$FIN[1], length=nrow(DEN_EST)))
colnames(FIN_DEN)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
FIN_EST <- cbind(FIN_EST, rep(BSRDistance$FIN[2], length=nrow(DEN_EST)))
colnames(FIN_EST)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
FIN_GFR <- cbind(FIN_GFR, rep(BSRDistance$FIN[4], length=nrow(DEN_EST)))
colnames(FIN_GFR)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
FIN_LAT <- cbind(FIN_LAT, rep(BSRDistance$FIN[5], length=nrow(DEN_EST)))
colnames(FIN_LAT)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
FIN_LIT <- cbind(FIN_LIT, rep(BSRDistance$FIN[6], length=nrow(DEN_EST)))
colnames(FIN_LIT)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
FIN_POL <- cbind(FIN_POL, rep(BSRDistance$FIN[7], length=nrow(DEN_EST)))
colnames(FIN_POL)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
FIN_RUS <- cbind(FIN_RUS, rep(BSRDistance$FIN[8], length=nrow(DEN_EST)))
colnames(FIN_RUS)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
FIN_SWD <- cbind(FIN_SWD, rep(BSRDistance$FIN[9], length=nrow(DEN_EST)))
colnames(FIN_SWD)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
# Germany
GFR_DEN <- cbind(GFR_DEN, rep(BSRDistance$GFR[1], length=nrow(DEN_EST)))
colnames(GFR_DEN)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
GFR_EST <- cbind(GFR_EST, rep(BSRDistance$GFR[2], length=nrow(DEN_EST)))
colnames(GFR_EST)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
GFR_FIN <- cbind(GFR_FIN, rep(BSRDistance$GFR[3], length=nrow(DEN_EST)))
colnames(GFR_FIN)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
GFR_LAT <- cbind(GFR_LAT, rep(BSRDistance$GFR[5], length=nrow(DEN_EST)))
colnames(GFR_LAT)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
GFR_LIT <- cbind(GFR_LIT, rep(BSRDistance$GFR[6], length=nrow(DEN_EST)))
colnames(GFR_LIT)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
GFR_POL <- cbind(GFR_POL, rep(BSRDistance$GFR[7], length=nrow(DEN_EST)))
colnames(GFR_POL)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
GFR_RUS <- cbind(GFR_RUS, rep(BSRDistance$GFR[8], length=nrow(DEN_EST)))
colnames(GFR_RUS)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
GFR_SWD <- cbind(GFR_SWD, rep(BSRDistance$GFR[9], length=nrow(DEN_EST)))
colnames(GFR_SWD)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
# Latvia
LAT_DEN <- cbind(LAT_DEN, rep(BSRDistance$LAT[1], length=nrow(DEN_EST)))
colnames(LAT_DEN)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
LAT_EST <- cbind(LAT_EST, rep(BSRDistance$LAT[2], length=nrow(DEN_EST)))
colnames(LAT_EST)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
LAT_FIN <- cbind(LAT_FIN, rep(BSRDistance$LAT[3], length=nrow(DEN_EST)))
colnames(LAT_FIN)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
LAT_GFR <- cbind(LAT_GFR, rep(BSRDistance$LAT[4], length=nrow(DEN_EST)))
colnames(LAT_GFR)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
LAT_LIT <- cbind(LAT_LIT, rep(BSRDistance$LAT[6], length=nrow(DEN_EST)))
colnames(LAT_LIT)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
LAT_POL <- cbind(LAT_POL, rep(BSRDistance$LAT[7], length=nrow(DEN_EST)))
colnames(LAT_POL)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
LAT_RUS <- cbind(LAT_RUS, rep(BSRDistance$LAT[8], length=nrow(DEN_EST)))
colnames(LAT_RUS)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
LAT_SWD <- cbind(LAT_SWD, rep(BSRDistance$LAT[9], length=nrow(DEN_EST)))
colnames(LAT_SWD)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
# Lithuania
LIT_DEN <- cbind(LIT_DEN, rep(BSRDistance$LIT[1], length=nrow(DEN_EST)))
colnames(LIT_DEN)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
LIT_EST <- cbind(LIT_EST, rep(BSRDistance$LIT[2], length=nrow(DEN_EST)))
colnames(LIT_EST)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
LIT_FIN <- cbind(LIT_FIN, rep(BSRDistance$LIT[3], length=nrow(DEN_EST)))
colnames(LIT_FIN)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
LIT_GFR <- cbind(LIT_GFR, rep(BSRDistance$LIT[4], length=nrow(DEN_EST)))
colnames(LIT_GFR)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
LIT_LAT <- cbind(LIT_LAT, rep(BSRDistance$LIT[5], length=nrow(DEN_EST)))
colnames(LIT_LAT)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
LIT_POL <- cbind(LIT_POL, rep(BSRDistance$LIT[7], length=nrow(DEN_EST)))
colnames(LIT_POL)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
LIT_RUS <- cbind(LIT_RUS, rep(BSRDistance$LIT[8], length=nrow(DEN_EST)))
colnames(LIT_RUS)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
LIT_SWD <- cbind(LIT_SWD, rep(BSRDistance$LIT[9], length=nrow(DEN_EST)))
colnames(LIT_SWD)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
# Poland
POL_DEN <- cbind(POL_DEN, rep(BSRDistance$POL[1], length=nrow(DEN_EST)))
colnames(POL_DEN)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
POL_EST <- cbind(POL_EST, rep(BSRDistance$POL[2], length=nrow(DEN_EST)))
colnames(POL_EST)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
POL_FIN <- cbind(POL_FIN, rep(BSRDistance$POL[3], length=nrow(DEN_EST)))
colnames(POL_FIN)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
POL_GFR <- cbind(POL_GFR, rep(BSRDistance$POL[4], length=nrow(DEN_EST)))
colnames(POL_GFR)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
POL_LAT <- cbind(POL_LAT, rep(BSRDistance$POL[5], length=nrow(DEN_EST)))
colnames(POL_LAT)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
POL_LIT <- cbind(POL_LIT, rep(BSRDistance$POL[6], length=nrow(DEN_EST)))
colnames(POL_LIT)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
POL_RUS <- cbind(POL_RUS, rep(BSRDistance$POL[8], length=nrow(DEN_EST)))
colnames(POL_RUS)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
POL_SWD <- cbind(POL_SWD, rep(BSRDistance$POL[9], length=nrow(DEN_EST)))
colnames(POL_SWD)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
# Russia
RUS_DEN <- cbind(RUS_DEN, rep(BSRDistance$RUS[1], length=nrow(DEN_EST)))
colnames(RUS_DEN)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
RUS_EST <- cbind(RUS_EST, rep(BSRDistance$RUS[2], length=nrow(DEN_EST)))
colnames(RUS_EST)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
RUS_FIN <- cbind(RUS_FIN, rep(BSRDistance$RUS[3], length=nrow(DEN_EST)))
colnames(RUS_FIN)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
RUS_GFR <- cbind(RUS_GFR, rep(BSRDistance$RUS[4], length=nrow(DEN_EST)))
colnames(RUS_GFR)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
RUS_LAT <- cbind(RUS_LAT, rep(BSRDistance$RUS[5], length=nrow(DEN_EST)))
colnames(RUS_LAT)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
RUS_LIT <- cbind(RUS_LIT, rep(BSRDistance$RUS[6], length=nrow(DEN_EST)))
colnames(RUS_LIT)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
RUS_POL <- cbind(RUS_POL, rep(BSRDistance$RUS[7], length=nrow(DEN_EST)))
colnames(RUS_POL)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
RUS_SWD <- cbind(RUS_SWD, rep(BSRDistance$RUS[9], length=nrow(DEN_EST)))
colnames(RUS_SWD)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
# Sweden
SWD_DEN <- cbind(SWD_DEN, rep(BSRDistance$SWD[1], length=nrow(DEN_EST)))
colnames(SWD_DEN)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
SWD_EST <- cbind(SWD_EST, rep(BSRDistance$SWD[2], length=nrow(DEN_EST)))
colnames(SWD_EST)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
SWD_FIN <- cbind(SWD_FIN, rep(BSRDistance$SWD[3], length=nrow(DEN_EST)))
colnames(SWD_FIN)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
SWD_GFR <- cbind(SWD_GFR, rep(BSRDistance$SWD[4], length=nrow(DEN_EST)))
colnames(SWD_GFR)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
SWD_LAT <- cbind(SWD_LAT, rep(BSRDistance$SWD[5], length=nrow(DEN_EST)))
colnames(SWD_LAT)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
SWD_LIT <- cbind(SWD_LIT, rep(BSRDistance$SWD[6], length=nrow(DEN_EST)))
colnames(SWD_LIT)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
SWD_POL <- cbind(SWD_POL, rep(BSRDistance$SWD[7], length=nrow(DEN_EST)))
colnames(SWD_POL)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")
SWD_RUS <- cbind(SWD_RUS, rep(BSRDistance$SWD[8], length=nrow(DEN_EST)))
colnames(SWD_RUS)[18:22] <- c("Norm_EU", "Norm_BA","Norm_CBSS","Norm_Norden","Distance")

BSR_ext_new2 <- rbind(DEN_EST, DEN_FIN, DEN_GFR,DEN_LAT,DEN_LIT,DEN_POL,DEN_RUS,DEN_SWD,EST_DEN,EST_FIN,EST_GFR,EST_LAT,EST_LIT,EST_POL,EST_RUS,EST_SWD,FIN_DEN,FIN_EST,FIN_GFR,FIN_LAT,FIN_LIT,FIN_POL,FIN_RUS,FIN_SWD,GFR_DEN,GFR_EST,GFR_FIN,GFR_LAT,GFR_LIT,GFR_POL,GFR_RUS,GFR_SWD,LAT_DEN,LAT_EST,LAT_FIN,LAT_GFR,LAT_LIT,LAT_POL,LAT_RUS,LAT_SWD,LIT_DEN,LIT_EST,LIT_FIN,LIT_GFR,LIT_LAT,LIT_POL,LIT_RUS,LIT_SWD,POL_DEN,POL_EST,POL_FIN,POL_GFR,POL_LAT,POL_LIT,POL_RUS,POL_SWD,RUS_DEN,RUS_EST,RUS_FIN,RUS_GFR,RUS_LAT,RUS_LIT,RUS_POL,RUS_SWD,SWD_DEN,SWD_EST,SWD_FIN,SWD_GFR,SWD_LAT,SWD_LIT,SWD_POL,SWD_RUS)


# Cleaning the global environment: We like it neat and tidy ;)
rm(list= ls()[!(ls() %in% c('DEN_EST', 'DEN_FIN', 'DEN_GFR','DEN_LAT','DEN_LIT','DEN_POL','DEN_RUS','DEN_SWD','EST_DEN','EST_FIN','EST_GFR','EST_LAT','EST_LIT','EST_POL','EST_RUS','EST_SWD','FIN_DEN','FIN_EST','FIN_GFR','FIN_LAT','FIN_LIT','FIN_POL','FIN_RUS','FIN_SWD','GFR_DEN','GFR_EST','GFR_FIN','GFR_LAT','GFR_LIT','GFR_POL','GFR_RUS','GFR_SWD','LAT_DEN','LAT_EST','LAT_FIN','LAT_GFR','LAT_LIT','LAT_POL','LAT_RUS','LAT_SWD','LIT_DEN','LIT_EST','LIT_FIN','LIT_GFR','LIT_LAT','LIT_POL','LIT_RUS','LIT_SWD','POL_DEN','POL_EST','POL_FIN','POL_GFR','POL_LAT','POL_LIT','POL_RUS','POL_SWD','RUS_DEN','RUS_EST','RUS_FIN','RUS_GFR','RUS_LAT','RUS_LIT','RUS_POL','RUS_SWD','SWD_DEN','SWD_EST','SWD_FIN','SWD_GFR','SWD_LAT','SWD_LIT','SWD_POL','SWD_RUS', 'Norms'))])

# Unifying all dyadic subsets into one data frame
BSR_df <- rbind(DEN_EST, DEN_FIN, DEN_GFR,DEN_LAT,DEN_LIT,DEN_POL,DEN_RUS,DEN_SWD,EST_DEN,EST_FIN,EST_GFR,EST_LAT,EST_LIT,EST_POL,EST_RUS,EST_SWD,FIN_DEN,FIN_EST,FIN_GFR,FIN_LAT,FIN_LIT,FIN_POL,FIN_RUS,FIN_SWD,GFR_DEN,GFR_EST,GFR_FIN,GFR_LAT,GFR_LIT,GFR_POL,GFR_RUS,GFR_SWD,LAT_DEN,LAT_EST,LAT_FIN,LAT_GFR,LAT_LIT,LAT_POL,LAT_RUS,LAT_SWD,LIT_DEN,LIT_EST,LIT_FIN,LIT_GFR,LIT_LAT,LIT_POL,LIT_RUS,LIT_SWD,POL_DEN,POL_EST,POL_FIN,POL_GFR,POL_LAT,POL_LIT,POL_RUS,POL_SWD,RUS_DEN,RUS_EST,RUS_FIN,RUS_GFR,RUS_LAT,RUS_LIT,RUS_POL,RUS_SWD,SWD_DEN,SWD_EST,SWD_FIN,SWD_GFR,SWD_LAT,SWD_LIT,SWD_POL,SWD_RUS)
BSR_ext <- BSR_df

# Save the new complete data frame in its original version
write.csv(BSR_ext, "BSR_ext.csv")
BSR_ext <- BSR_ext_new2

# Multiply import/export prior to logarithmization?
# BSR_df$expab <- BSR_df$expab*1000000
# BSR_df$impab <- BSR_df$impab*1000000
# BSR_df$GDP <- BSR_df$GDP*1000000

BSR_ext_new3$expab <- BSR_ext_new3$expab
BSR_ext_new3$impab <- BSR_ext_new3$impab
BSR_ext_new3$GDP <- BSR_ext_new3$GDP
BSR_ext_new2$tradeop <- ((BSR_ext_new2$expab+BSR_ext_new2$impab)/BSR_ext_new2$GDP)*100 
BSR_ext_new2$ln.tradeop <- log(BSR_ext_new2$tradeop)



# Adding the Trade openness (and ln version) to data frame
tradeop <- ((BSR_df$expab+BSR_df$impab)/BSR_df$GDP)*10000
BSR_df <- cbind(BSR_df, tradeop)
ln.tradeop <- log(as.numeric(BSR_df$tradeop))
BSR_df <- cbind(BSR_df, ln.tradeop)



# Replace negative log values in ln.tradeop with NA
library(stringr)
# BSR_df2 <- BSR_df
# BSR_df2$ln.tradeop <- str_replace_all(BSR_df2$ln.tradeop, "-inf", "NA")
 BSR_ext$ln.tradeop <- str_replace_all(BSR_ext$ln.tradeop, "-Inf", "NA")
 BSR_ext$ln.tradeop <- as.numeric(BSR_ext$ln.tradeop)

# Another (better) way to do the above
# BSR_df2 <- within(BSR_df2, ln.tradeop[ln.tradeop<0] <- NA)
# BSR_df2$ln.tradeop <- as.numeric(BSR_df2$ln.tradeop)

# Using a new identifier for the BSR_df to prevent unwanted messing up
# BSR_df2 <- BSR_df
# BSR_ext <- BSR_df2

# Adding GDP per Capita
GDPpC <- (BSR_ext$GDP*1000)/BSR_ext$Population
BSR_ext <- cbind(BSR_ext, GDPpC)
ln.GDPpC <- log(BSR_ext$GDPpC)
BSR_ext <- cbind(BSR_ext, ln.GDPpC)

rm(list= ls()[!(ls() %in% c('BSR_ext', 'BSR_df','Norms'))])

# Subsetting the new data frame int dyads for statistical analysis
BSR
# Denamrk
DEN_EST <- BSR_ext %>% filter(acra == "DEN", acrb %in% "EST")
DEN_FIN <- BSR_ext %>% filter(acra == "DEN", acrb %in% "FIN")
DEN_GFR <- BSR_ext %>% filter(acra == "DEN", acrb %in% "GFR")
DEN_LAT <- BSR_ext %>% filter(acra == "DEN", acrb %in% "LAT")
DEN_LIT <- BSR_ext %>% filter(acra == "DEN", acrb %in% "LIT")
DEN_POL <- BSR_ext %>% filter(acra == "DEN", acrb %in% "POL")
DEN_RUS <- BSR_ext %>% filter(acra == "DEN", acrb %in% "RUS")
DEN_SWD <- BSR_ext %>% filter(acra == "DEN", acrb %in% "SWD")
# Estonia
EST_DEN <- BSR_ext %>% filter(acra == "EST", acrb %in% "DEN")
EST_FIN <- BSR_ext %>% filter(acra == "EST", acrb %in% "FIN")
EST_GFR <- BSR_ext %>% filter(acra == "EST", acrb %in% "GFR")
EST_LAT <- BSR_ext %>% filter(acra == "EST", acrb %in% "LAT")
EST_LIT <- BSR_ext %>% filter(acra == "EST", acrb %in% "LIT")
EST_POL <- BSR_ext %>% filter(acra == "EST", acrb %in% "POL")
EST_RUS <- BSR_ext %>% filter(acra == "EST", acrb %in% "RUS")
EST_SWD <- BSR_ext %>% filter(acra == "EST", acrb %in% "SWD")
# Finland
FIN_DEN <- BSR_ext %>% filter(acra == "FIN", acrb %in% "DEN")
FIN_EST <- BSR_ext %>% filter(acra == "FIN", acrb %in% "EST")
FIN_GFR <- BSR_ext %>% filter(acra == "FIN", acrb %in% "GFR")
FIN_LAT <- BSR_ext %>% filter(acra == "FIN", acrb %in% "LAT")
FIN_LIT <- BSR_ext %>% filter(acra == "FIN", acrb %in% "LIT")
FIN_POL <- BSR_ext %>% filter(acra == "FIN", acrb %in% "POL")
FIN_RUS <- BSR_ext %>% filter(acra == "FIN", acrb %in% "RUS")
FIN_SWD <- BSR_ext %>% filter(acra == "FIN", acrb %in% "SWD")
# Germany
GFR_DEN <- BSR_ext %>% filter(acra == "GFR", acrb %in% "DEN")
GFR_EST <- BSR_ext %>% filter(acra == "GFR", acrb %in% "EST")
GFR_FIN <- BSR_ext %>% filter(acra == "GFR", acrb %in% "FIN")
GFR_LAT <- BSR_ext %>% filter(acra == "GFR", acrb %in% "LAT")
GFR_LIT <- BSR_ext %>% filter(acra == "GFR", acrb %in% "LIT")
GFR_POL <- BSR_ext %>% filter(acra == "GFR", acrb %in% "POL")
GFR_RUS <- BSR_ext %>% filter(acra == "GFR", acrb %in% "RUS")
GFR_SWD <- BSR_ext %>% filter(acra == "GFR", acrb %in% "SWD")
# Latvia
LAT_DEN <- BSR_ext %>% filter(acra == "LAT", acrb %in% "DEN")
LAT_EST <- BSR_ext %>% filter(acra == "LAT", acrb %in% "EST")
LAT_FIN <- BSR_ext %>% filter(acra == "LAT", acrb %in% "FIN")
LAT_GFR <- BSR_ext %>% filter(acra == "LAT", acrb %in% "GFR")
LAT_LIT <- BSR_ext %>% filter(acra == "LAT", acrb %in% "LIT")
LAT_POL <- BSR_ext %>% filter(acra == "LAT", acrb %in% "POL")
LAT_RUS <- BSR_ext %>% filter(acra == "LAT", acrb %in% "RUS")
LAT_SWD <- BSR_ext %>% filter(acra == "LAT", acrb %in% "SWD")
# Lithuania
LIT_DEN <- BSR_ext %>% filter(acra == "LIT", acrb %in% "DEN")
LIT_EST <- BSR_ext %>% filter(acra == "LIT", acrb %in% "EST")
LIT_FIN <- BSR_ext %>% filter(acra == "LIT", acrb %in% "FIN")
LIT_GFR <- BSR_ext %>% filter(acra == "LIT", acrb %in% "GFR")
LIT_LAT <- BSR_ext %>% filter(acra == "LIT", acrb %in% "LAT")
LIT_POL <- BSR_ext %>% filter(acra == "LIT", acrb %in% "POL")
LIT_RUS <- BSR_ext %>% filter(acra == "LIT", acrb %in% "RUS")
LIT_SWD <- BSR_ext %>% filter(acra == "LIT", acrb %in% "SWD")
# Poland
POL_DEN <- BSR_ext %>% filter(acra == "POL", acrb %in% "DEN")
POL_EST <- BSR_ext %>% filter(acra == "POL", acrb %in% "EST")
POL_FIN <- BSR_ext %>% filter(acra == "POL", acrb %in% "FIN")
POL_GFR <- BSR_ext %>% filter(acra == "POL", acrb %in% "GFR")
POL_LAT <- BSR_ext %>% filter(acra == "POL", acrb %in% "LAT")
POL_LIT <- BSR_ext %>% filter(acra == "POL", acrb %in% "LIT")
POL_RUS <- BSR_ext %>% filter(acra == "POL", acrb %in% "RUS")
POL_SWD <- BSR_ext %>% filter(acra == "POL", acrb %in% "SWD")
# Russia
RUS_DEN <- BSR_ext %>% filter(acra == "RUS", acrb %in% "DEN")
RUS_EST <- BSR_ext %>% filter(acra == "RUS", acrb %in% "EST")
RUS_FIN <- BSR_ext %>% filter(acra == "RUS", acrb %in% "FIN")
RUS_GFR <- BSR_ext %>% filter(acra == "RUS", acrb %in% "GFR")
RUS_LAT <- BSR_ext %>% filter(acra == "RUS", acrb %in% "LAT")
RUS_LIT <- BSR_ext %>% filter(acra == "RUS", acrb %in% "LIT")
RUS_POL <- BSR_ext %>% filter(acra == "RUS", acrb %in% "POL")
RUS_SWD <- BSR_ext %>% filter(acra == "RUS", acrb %in% "SWD")
# Sweden
SWD_DEN <- BSR_ext %>% filter(acra == "SWD", acrb %in% "DEN")
SWD_EST <- BSR_ext %>% filter(acra == "SWD", acrb %in% "EST")
SWD_FIN <- BSR_ext %>% filter(acra == "SWD", acrb %in% "FIN")
SWD_GFR <- BSR_ext %>% filter(acra == "SWD", acrb %in% "GFR")
SWD_LAT <- BSR_ext %>% filter(acra == "SWD", acrb %in% "LAT")
SWD_LIT <- BSR_ext %>% filter(acra == "SWD", acrb %in% "LIT")
SWD_POL <- BSR_ext %>% filter(acra == "SWD", acrb %in% "POL")
SWD_RUS <- BSR_ext %>% filter(acra == "SWD", acrb %in% "RUS")

############################################################
# STATISTICS (finally)
############################################################

# Some preparatory work
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point(size = 0.5) +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5))) + theme_gray()
  #                    "Intercept =",signif(fit$coef[[1]],5 ),
  #                    " Slope =",signif(fit$coef[[2]], 5),
  #                    " P =",signif(summary(fit)$coef[2,4], 5)))
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, 
                      labs=list(), labpos=list(c(0.5,0.03), c(0.03,0.5))) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
    
    if(!length(labs) == 0){
      grid.text(labs[1], x=labpos[[1]][1], y=labpos[[1]][2], gp=gpar(fontsize=16))
      grid.text(labs[2], x=labpos[[2]][1], y=labpos[[2]][2], rot=90, gp=gpar(fontsize=16))
    }
  }
}

# BA ######################################################
#BSRContiguity$EST <- as.factor(BSRContiguity$EST)
#rep(BSRPolitySC2$EST[8], length=nrow(EST_RUS))
# BSRContiguity$EST <- as.factor(BSRContiguity$EST)
# plot_BA_EST_LAT + facet_grid( ~ Norms$ba_cum[1:24, ])

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$ba_cum
y <- EST_LAT$ln.tradeop

fit_BA_EST_LAT <- lm(y ~ x1) 
summary(fit_BA_EST_LAT) 
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_BA_EST_LAT)
plot_BA_EST_LAT <- p + ggtitle(paste( "EST - LAT", "(", "R2 = ",signif(summary(fit_BA_EST_LAT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))


y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$ba_cum
y <- EST_LIT$ln.tradeop

fit_BA_EST_LIT <- lm(y ~ x1) 
summary(fit_BA_EST_LIT) 
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_BA_EST_LIT)
plot_BA_EST_LIT <- p + ggtitle(paste( "EST - LIT", "(", "R2 = ",signif(summary(fit_BA_EST_LIT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))


y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$ba_cum
y <- LAT_EST$ln.tradeop

fit_BA_LAT_EST <- lm(y ~ x1) 
summary(fit_BA_LAT_EST) 
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_BA_LAT_EST)
plot_BA_LAT_EST <- p + ggtitle(paste( "LAT - EST", "(", "R2 = ",signif(summary(fit_BA_LAT_EST)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))


y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$ba_cum
y <- LAT_LIT$ln.tradeop

fit_BA_LAT_LIT <- lm(y ~ x1) 
summary(fit_BA_LAT_LIT) 
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_BA_LAT_LIT)
plot_BA_LAT_LIT <- p + ggtitle(paste( "LAT - LIT", "(", "R2 = ",signif(summary(fit_BA_LAT_LIT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))


y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$ba_cum
y <- LIT_EST$ln.tradeop

fit_BA_LIT_EST <- lm(y ~ x1) 
summary(fit_BA_LIT_EST) 
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_BA_LIT_EST)
plot_BA_LIT_EST <- p + ggtitle(paste( "ESLIT_T -", "(", "R2 = ",signif(summary(fit_BA_LIT_EST)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))


y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$ba_cum
y <- LIT_LAT$ln.tradeop

fit_BA_LIT_LAT <- lm(y ~ x1) 
summary(fit_BA_LIT_LAT) 
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_BA_LIT_LAT)
plot_BA_LIT_LAT <- p + ggtitle(paste( "LIT - LAT", "(", "R2 = ",signif(summary(fit_BA_LIT_LAT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))
y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$ba_cum
y <- EST_LAT$ln.tradeop

fit_BA_EST_LAT <- lm(y ~ x1) 
summary(fit_BA_EST_LAT) 
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_BA_EST_LAT)
plot_BA_EST_LAT <- p + ggtitle(paste( "EST - LAT", "(", "R2 = ",signif(summary(fit_BA_EST_LAT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))


y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$ba_cum
y <- EST_LIT$ln.tradeop

fit_BA_EST_LIT <- lm(y ~ x1) 
summary(fit_BA_EST_LIT) 
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_BA_EST_LIT)
plot_BA_EST_LIT <- p + ggtitle(paste( "EST - LIT", "(", "R2 = ",signif(summary(fit_BA_EST_LIT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))


y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$ba_cum
y <- LAT_EST$ln.tradeop

fit_BA_LAT_EST <- lm(y ~ x1) 
summary(fit_BA_LAT_EST) 
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_BA_LAT_EST)
plot_BA_LAT_EST <- p + ggtitle(paste( "LAT - EST", "(", "R2 = ",signif(summary(fit_BA_LAT_EST)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))


y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$ba_cum
y <- LAT_LIT$ln.tradeop

fit_BA_LAT_LIT <- lm(y ~ x1) 
summary(fit_BA_LAT_LIT) 
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_BA_LAT_LIT)
plot_BA_LAT_LIT <- p + ggtitle(paste( "LAT - LIT", "(", "R2 = ",signif(summary(fit_BA_LAT_LIT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))


y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$ba_cum
y <- LIT_EST$ln.tradeop

fit_BA_LIT_EST <- lm(y ~ x1) 
summary(fit_BA_LIT_EST) 
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_BA_LIT_EST)
plot_BA_LIT_EST <- p + ggtitle(paste( "ESLIT_T -", "(", "R2 = ",signif(summary(fit_BA_LIT_EST)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))


y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$ba_cum
y <- LIT_LAT$ln.tradeop

fit_BA_LIT_LAT <- lm(y ~ x1) 
summary(fit_BA_LIT_LAT) 
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_BA_LIT_LAT)
plot_BA_LIT_LAT <- p + ggtitle(paste( "LIT - LAT", "(", "R2 = ",signif(summary(fit_BA_LIT_LAT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

# Creating a unified plot for all BA plots + tikzdevice output
# tikz(sanitize = TRUE, file = "Illustrations/BA_TO.tex")
#multiplot(plot_BA_EST_LAT, plot_BA_EST_LIT, plot_BA_LAT_EST, plot_BA_LAT_LIT, plot_BA_LIT_EST, plot_BA_LIT_LAT, cols = 3, labs=list("Norms", "Log Trade Openness [%]"))
# dev.off()

# Creating the LaTeX table
stargazer(fit_BA_EST_LAT, glmm_BA_EST_LAT, fit_BA_EST_LIT, glmm_BA_EST_LIT, fit_BA_LAT_EST, glmm_BA_LAT_EST, fit_BA_LAT_LIT, glmm_BA_LAT_LIT, fit_BA_LIT_EST, glmm_BA_LIT_EST, fit_BA_LIT_LAT, glmm_BA_LIT_LAT) + xlab("Norms") + ylab("Log Trade Openess [%]")

library(grid)
library(gridExtra)

tikz(sanitize = TRUE, file = "Illustrations/BA_TO.tex", width = 4, height = 6)
grid.arrange(plot_BA_EST_LAT, plot_BA_EST_LIT, plot_BA_LAT_EST, plot_BA_LAT_LIT, plot_BA_LIT_EST, plot_BA_LIT_LAT, ncol = 2, left = textGrob("Log Trade Openness [%]", gp=gpar(fontsize=4), rot = 90), bottom = textGrob("Norms", gp=gpar(fontsize=4)))
dev.off()

# CBSS ####################################################
y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- DEN_EST$ln.tradeop

fit_CBSS_DEN_EST <- lm(y ~ x1) 
summary(fit_CBSS_DEN_EST)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_DEN_EST)
plot_CBSS_DEN_EST <- p + ggtitle(paste( "DEN - EST", "(", "R2 = ",signif(summary(fit_CBSS_DEN_EST)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- DEN_FIN$ln.tradeop

fit_CBSS_DEN_FIN <- lm(y ~ x1) 
summary(fit_CBSS_DEN_FIN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_DEN_FIN)
plot_CBSS_DEN_FIN <- p + ggtitle(paste( "DEN - FIN", "(", "R2 = ",signif(summary(fit_CBSS_DEN_FIN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- DEN_GFR$ln.tradeop

fit_CBSS_DEN_GFR <- lm(y ~ x1) 
summary(fit_CBSS_DEN_GFR)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_DEN_GFR)
plot_CBSS_DEN_GFR <- p + ggtitle(paste( "DEN - GFR", "(", "R2 = ",signif(summary(fit_CBSS_DEN_GFR)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- DEN_LAT$ln.tradeop

fit_CBSS_DEN_LAT <- lm(y ~ x1) 
summary(fit_CBSS_DEN_LAT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_DEN_LAT)
plot_CBSS_DEN_LAT <- p + ggtitle(paste( "DEN - LAT", "(", "R2 = ",signif(summary(fit_CBSS_DEN_LAT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- DEN_LIT$ln.tradeop

fit_CBSS_DEN_LIT <- lm(y ~ x1) 
summary(fit_CBSS_DEN_LIT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_DEN_LIT)
plot_CBSS_DEN_LIT <- p + ggtitle(paste( "DEN - LIT", "(", "R2 = ",signif(summary(fit_CBSS_DEN_LIT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- DEN_POL$ln.tradeop

fit_CBSS_DEN_POL <- lm(y ~ x1) 
summary(fit_CBSS_DEN_POL)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_DEN_POL)
plot_CBSS_DEN_POL <- p + ggtitle(paste( "DEN - POL", "(", "R2 = ",signif(summary(fit_CBSS_DEN_POL)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- DEN_RUS$ln.tradeop

fit_CBSS_DEN_RUS <- lm(y ~ x1) 
summary(fit_CBSS_DEN_RUS)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_DEN_RUS)
plot_CBSS_DEN_RUS <- p + ggtitle(paste( "DEN - RUS", "(", "R2 = ",signif(summary(fit_CBSS_DEN_RUS)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
x2 <- BSRPolitySC2$DEN
y <- DEN_SWD$ln.tradeop

fit_CBSS_DEN_SWD <- lm(y ~ x1 + x2) 
summary(fit_CBSS_DEN_SWD)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_DEN_SWD)
plot_CBSS_DEN_SWD <- p + ggtitle(paste( "DEN - SWD", "(", "R2 = ",signif(summary(fit_CBSS_DEN_SWD)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

BSRPolitySC2$DEN <- as.numeric(BSRPolitySC2$DEN)
y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- EST_DEN$ln.tradeop

fit_CBSS_EST_DEN <- lm(y ~ x1) 
summary(fit_CBSS_EST_DEN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_EST_DEN)
plot_CBSS_EST_DEN <- p + ggtitle(paste( "EST - DEN", "(", "R2 = ",signif(summary(fit_CBSS_EST_DEN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- EST_FIN$ln.tradeop

fit_CBSS_EST_FIN <- lm(y ~ x1) 
summary(fit_CBSS_EST_FIN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_EST_FIN)
plot_CBSS_EST_FIN <- p + ggtitle(paste( "EST - FIN", "(", "R2 = ",signif(summary(fit_CBSS_EST_FIN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- EST_GFR$ln.tradeop

fit_CBSS_EST_GFR <- lm(y ~ x1) 
summary(fit_CBSS_EST_GFR)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_EST_GFR)
plot_CBSS_EST_GFR <- p + ggtitle(paste( "EST - GFR", "(", "R2 = ",signif(summary(fit_CBSS_EST_GFR)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- EST_LAT$ln.tradeop

fit_CBSS_EST_LAT <- lm(y ~ x1) 
summary(fit_CBSS_EST_LAT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_EST_LAT)
plot_CBSS_EST_LAT <- p + ggtitle(paste( "EST - LAT", "(", "R2 = ",signif(summary(fit_CBSS_EST_LAT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- EST_LIT$ln.tradeop

fit_CBSS_EST_LIT <- lm(y ~ x1) 
summary(fit_CBSS_EST_LIT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_EST_LIT)
plot_CBSS_EST_LIT <- p + ggtitle(paste( "EST - LIT", "(", "R2 = ",signif(summary(fit_CBSS_EST_LIT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- EST_POL$ln.tradeop

fit_CBSS_EST_POL <- lm(y ~ x1) 
summary(fit_CBSS_EST_POL)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_EST_POL)
plot_CBSS_EST_POL <- p + ggtitle(paste( "EST - POL", "(", "R2 = ",signif(summary(fit_CBSS_EST_POL)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
x2 <- BSRPolitySC2$EST
y <- EST_RUS$ln.tradeop

fit_CBSS_EST_RUS <- lm(y ~ x1 + x2, data = EST_RUS, na.action="na.exclude") 
summary(fit_CBSS_EST_RUS)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_EST_RUS)
plot_CBSS_EST_RUS <- p + ggtitle(paste( "EST - RUS", "(", "R2 = ",signif(summary(fit_CBSS_EST_RUS)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- EST_SWD$ln.tradeop

fit_CBSS_EST_SWD <- lm(y ~ x1) 
summary(fit_CBSS_EST_SWD)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_EST_SWD)
plot_CBSS_EST_SWD <- p + ggtitle(paste( "EST - SWD", "(", "R2 = ",signif(summary(fit_CBSS_EST_SWD)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- FIN_DEN$ln.tradeop

fit_CBSS_FIN_DEN <- lm(y ~ x1) 
summary(fit_CBSS_FIN_DEN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_FIN_DEN)
plot_CBSS_FIN_DEN <- p + ggtitle(paste( "FIN - DEN", "(", "R2 = ",signif(summary(fit_CBSS_FIN_DEN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- FIN_EST$ln.tradeop

fit_CBSS_FIN_EST <- lm(y ~ x1) 
summary(fit_CBSS_FIN_EST)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_FIN_EST)
plot_CBSS_FIN_EST <- p + ggtitle(paste( "FIN - EST", "(", "R2 = ",signif(summary(fit_CBSS_FIN_EST)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- FIN_GFR$ln.tradeop

fit_CBSS_FIN_GFR <- lm(y ~ x1) 
summary(fit_CBSS_FIN_GFR)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_FIN_GFR)
plot_CBSS_FIN_GFR <- p + ggtitle(paste( "FIN - GFR", "(", "R2 = ",signif(summary(fit_CBSS_FIN_GFR)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- FIN_LAT$ln.tradeop

fit_CBSS_FIN_LAT <- lm(y ~ x1) 
summary(fit_CBSS_FIN_LAT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_FIN_LAT)
plot_CBSS_FIN_LAT <- p + ggtitle(paste( "FIN - LAT", "(", "R2 = ",signif(summary(fit_CBSS_FIN_LAT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- FIN_LIT$ln.tradeop

fit_CBSS_FIN_LIT <- lm(y ~ x1) 
summary(fit_CBSS_FIN_LIT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_FIN_LIT)
plot_CBSS_FIN_LIT <- p + ggtitle(paste( "FIN - LIT", "(", "R2 = ",signif(summary(fit_CBSS_FIN_LIT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- FIN_POL$ln.tradeop

fit_CBSS_FIN_POL <- lm(y ~ x1) 
summary(fit_CBSS_FIN_POL)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_FIN_POL)
plot_CBSS_FIN_POL <- p + ggtitle(paste( "FIN - POL", "(", "R2 = ",signif(summary(fit_CBSS_FIN_POL)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- FIN_RUS$ln.tradeop

fit_CBSS_FIN_RUS <- lm(y ~ x1) 
summary(fit_CBSS_FIN_RUS)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_FIN_RUS)
plot_CBSS_FIN_RUS <- p + ggtitle(paste( "FIN - RUS", "(", "R2 = ",signif(summary(fit_CBSS_FIN_RUS)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- FIN_SWD$ln.tradeop

fit_CBSS_FIN_SWD <- lm(y ~ x1) 
summary(fit_CBSS_FIN_SWD)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_FIN_SWD)
plot_CBSS_FIN_SWD <- p + ggtitle(paste( "FIN - SWD", "(", "R2 = ",signif(summary(fit_CBSS_FIN_SWD)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- GFR_DEN$ln.tradeop

fit_CBSS_GFR_DEN <- lm(y ~ x1) 
summary(fit_CBSS_GFR_DEN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_GFR_DEN)
plot_CBSS_GFR_DEN <- p + ggtitle(paste( "GFR - DEN", "(", "R2 = ",signif(summary(fit_CBSS_GFR_DEN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- GFR_EST$ln.tradeop

fit_CBSS_GFR_EST <- lm(y ~ x1) 
summary(fit_CBSS_GFR_EST)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_GFR_EST)
plot_CBSS_GFR_EST <- p + ggtitle(paste( "GFR - EST", "(", "R2 = ",signif(summary(fit_CBSS_GFR_EST)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- GFR_FIN$ln.tradeop

fit_CBSS_GFR_FIN <- lm(y ~ x1) 
summary(fit_CBSS_GFR_FIN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_GFR_FIN)
plot_CBSS_GFR_FIN <- p + ggtitle(paste( "GFR - FIN", "(", "R2 = ",signif(summary(fit_CBSS_GFR_FIN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- GFR_LAT$ln.tradeop

fit_CBSS_GFR_LAT <- lm(y ~ x1) 
summary(fit_CBSS_GFR_LAT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_GFR_LAT)
plot_CBSS_GFR_LAT <- p + ggtitle(paste( "GFR - LAT", "(", "R2 = ",signif(summary(fit_CBSS_GFR_LAT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- GFR_LIT$ln.tradeop

fit_CBSS_GFR_LIT <- lm(y ~ x1) 
summary(fit_CBSS_GFR_LIT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_GFR_LIT)
plot_CBSS_GFR_LIT <- p + ggtitle(paste( "GFR - LIT", "(", "R2 = ",signif(summary(fit_CBSS_GFR_LIT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- GFR_POL$ln.tradeop

fit_CBSS_GFR_POL <- lm(y ~ x1) 
summary(fit_CBSS_GFR_POL)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_GFR_POL)
plot_CBSS_GFR_POL <- p + ggtitle(paste( "GFR - POL", "(", "R2 = ",signif(summary(fit_CBSS_GFR_POL)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- GFR_RUS$ln.tradeop

fit_CBSS_GFR_RUS <- lm(y ~ x1) 
summary(fit_CBSS_GFR_RUS)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_GFR_RUS)
plot_CBSS_GFR_RUS <- p + ggtitle(paste( "GFR - RUS", "(", "R2 = ",signif(summary(fit_CBSS_GFR_RUS)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- GFR_SWD$ln.tradeop

fit_CBSS_GFR_SWD <- lm(y ~ x1) 
summary(fit_CBSS_GFR_SWD)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_GFR_SWD)
plot_CBSS_GFR_SWD <- p + ggtitle(paste( "GFR - SWD", "(", "R2 = ",signif(summary(fit_CBSS_GFR_SWD)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- LAT_DEN$ln.tradeop

fit_CBSS_LAT_DEN <- lm(y ~ x1) 
summary(fit_CBSS_LAT_DEN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_LAT_DEN)
plot_CBSS_LAT_DEN <- p + ggtitle(paste( "LAT - DEN", "(", "R2 = ",signif(summary(fit_CBSS_LAT_DEN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- LAT_EST$ln.tradeop

fit_CBSS_LAT_EST <- lm(y ~ x1) 
summary(fit_CBSS_LAT_EST)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_LAT_EST)
plot_CBSS_LAT_EST <- p + ggtitle(paste( "LAT - EST", "(", "R2 = ",signif(summary(fit_CBSS_LAT_EST)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- LAT_FIN$ln.tradeop

fit_CBSS_LAT_FIN <- lm(y ~ x1) 
summary(fit_CBSS_LAT_FIN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_LAT_FIN)
plot_CBSS_LAT_FIN <- p + ggtitle(paste( "LAT - FIN", "(", "R2 = ",signif(summary(fit_CBSS_LAT_FIN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- LAT_GFR$ln.tradeop

fit_CBSS_LAT_GFR <- lm(y ~ x1) 
summary(fit_CBSS_LAT_GFR)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_LAT_GFR)
plot_CBSS_LAT_GFR <- p + ggtitle(paste( "LAT - GFR", "(", "R2 = ",signif(summary(fit_CBSS_LAT_GFR)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- LAT_LIT$ln.tradeop

fit_CBSS_LAT_LIT <- lm(y ~ x1) 
summary(fit_CBSS_LAT_LIT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_LAT_LIT)
plot_CBSS_LAT_LIT <- p + ggtitle(paste( "LAT - LIT", "(", "R2 = ",signif(summary(fit_CBSS_LAT_LIT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- LAT_POL$ln.tradeop

fit_CBSS_LAT_POL <- lm(y ~ x1) 
summary(fit_CBSS_LAT_POL)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_LAT_POL)
plot_CBSS_LAT_POL <- p + ggtitle(paste( "LAT - POL", "(", "R2 = ",signif(summary(fit_CBSS_LAT_POL)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- LAT_RUS$ln.tradeop

fit_CBSS_LAT_RUS <- lm(y ~ x1) 
summary(fit_CBSS_LAT_RUS)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_LAT_RUS)
plot_CBSS_LAT_RUS <- p + ggtitle(paste( "LAT - RUS", "(", "R2 = ",signif(summary(fit_CBSS_LAT_RUS)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- LAT_SWD$ln.tradeop

fit_CBSS_LAT_SWD <- lm(y ~ x1) 
summary(fit_CBSS_LAT_SWD)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_LAT_SWD)
plot_CBSS_LAT_SWD <- p + ggtitle(paste( "LAT - SWD", "(", "R2 = ",signif(summary(fit_CBSS_LAT_SWD)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- LIT_DEN$ln.tradeop

fit_CBSS_LIT_DEN <- lm(y ~ x1) 
summary(fit_CBSS_LIT_DEN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_LIT_DEN)
plot_CBSS_LIT_DEN <- p + ggtitle(paste( "LIT - DEN", "(", "R2 = ",signif(summary(fit_CBSS_LIT_DEN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- LIT_EST$ln.tradeop

fit_CBSS_LIT_EST <- lm(y ~ x1) 
summary(fit_CBSS_LIT_EST)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_LIT_EST)
plot_CBSS_LIT_EST <- p + ggtitle(paste( "LIT - EST", "(", "R2 = ",signif(summary(fit_CBSS_LIT_EST)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- LIT_FIN$ln.tradeop

fit_CBSS_LIT_FIN <- lm(y ~ x1) 
summary(fit_CBSS_LIT_FIN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_LIT_FIN)
plot_CBSS_LIT_FIN <- p + ggtitle(paste( "LIT - FIN", "(", "R2 = ",signif(summary(fit_CBSS_LIT_FIN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- LIT_GFR$ln.tradeop

fit_CBSS_LIT_GFR <- lm(y ~ x1) 
summary(fit_CBSS_LIT_GFR)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_LIT_GFR)
plot_CBSS_LIT_GFR <- p + ggtitle(paste( "LIT - GFR", "(", "R2 = ",signif(summary(fit_CBSS_LIT_GFR)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- LIT_LAT$ln.tradeop

fit_CBSS_LIT_LAT <- lm(y ~ x1) 
summary(fit_CBSS_LIT_LAT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_LIT_LAT)
plot_CBSS_LIT_LAT <- p + ggtitle(paste( "LIT - LAT", "(", "R2 = ",signif(summary(fit_CBSS_LIT_LAT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- LIT_POL$ln.tradeop

fit_CBSS_LIT_POL <- lm(y ~ x1) 
summary(fit_CBSS_LIT_POL)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_LIT_POL)
plot_CBSS_LIT_POL <- p + ggtitle(paste( "LIT - POL", "(", "R2 = ",signif(summary(fit_CBSS_LIT_POL)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- LIT_RUS$ln.tradeop

fit_CBSS_LIT_RUS <- lm(y ~ x1) 
summary(fit_CBSS_LIT_RUS)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_LIT_RUS)
plot_CBSS_LIT_RUS <- p + ggtitle(paste( "LIT - RUS", "(", "R2 = ",signif(summary(fit_CBSS_LIT_RUS)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- LIT_SWD$ln.tradeop

fit_CBSS_LIT_SWD <- lm(y ~ x1) 
summary(fit_CBSS_LIT_SWD)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_LIT_SWD)
plot_CBSS_LIT_SWD <- p + ggtitle(paste( "LIT - SWD", "(", "R2 = ",signif(summary(fit_CBSS_LIT_SWD)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- POL_DEN$ln.tradeop

fit_CBSS_POL_DEN <- lm(y ~ x1) 
summary(fit_CBSS_POL_DEN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_POL_DEN)
plot_CBSS_POL_DEN <- p + ggtitle(paste( "POL - DEN", "(", "R2 = ",signif(summary(fit_CBSS_POL_DEN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- POL_EST$ln.tradeop

fit_CBSS_POL_EST <- lm(y ~ x1) 
summary(fit_CBSS_POL_EST)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_POL_EST)
plot_CBSS_POL_EST <- p + ggtitle(paste( "POL - EST", "(", "R2 = ",signif(summary(fit_CBSS_POL_EST)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- POL_FIN$ln.tradeop

fit_CBSS_POL_FIN <- lm(y ~ x1) 
summary(fit_CBSS_POL_FIN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_POL_FIN)
plot_CBSS_POL_FIN <- p + ggtitle(paste( "POL - FIN", "(", "R2 = ",signif(summary(fit_CBSS_POL_FIN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- POL_GFR$ln.tradeop

fit_CBSS_POL_GFR <- lm(y ~ x1) 
summary(fit_CBSS_POL_GFR)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_POL_GFR)
plot_CBSS_POL_GFR <- p + ggtitle(paste( "POL - GFR", "(", "R2 = ",signif(summary(fit_CBSS_POL_GFR)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- POL_LAT$ln.tradeop

fit_CBSS_POL_LAT <- lm(y ~ x1) 
summary(fit_CBSS_POL_LAT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_POL_LAT)
plot_CBSS_POL_LAT <- p + ggtitle(paste( "POL - LAT", "(", "R2 = ",signif(summary(fit_CBSS_POL_LAT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- POL_LIT$ln.tradeop

fit_CBSS_POL_LIT <- lm(y ~ x1) 
summary(fit_CBSS_POL_LIT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_POL_LIT)
plot_CBSS_POL_LIT <- p + ggtitle(paste( "POL - LIT", "(", "R2 = ",signif(summary(fit_CBSS_POL_LIT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- POL_RUS$ln.tradeop

fit_CBSS_POL_RUS <- lm(y ~ x1) 
summary(fit_CBSS_POL_RUS)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_POL_RUS)
plot_CBSS_POL_RUS <- p + ggtitle(paste( "POL - RUS", "(", "R2 = ",signif(summary(fit_CBSS_POL_RUS)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- POL_SWD$ln.tradeop

fit_CBSS_POL_SWD <- lm(y ~ x1) 
summary(fit_CBSS_POL_SWD)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_POL_SWD)
plot_CBSS_POL_SWD <- p + ggtitle(paste( "POL - SWD", "(", "R2 = ",signif(summary(fit_CBSS_POL_SWD)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- RUS_DEN$ln.tradeop

fit_CBSS_RUS_DEN <- lm(y ~ x1) 
summary(fit_CBSS_RUS_DEN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_RUS_DEN)
plot_CBSS_RUS_DEN <- p + ggtitle(paste( "RUS - DEN", "(", "R2 = ",signif(summary(fit_CBSS_RUS_DEN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- RUS_EST$ln.tradeop

fit_CBSS_RUS_EST <- lm(y ~ x1) 
summary(fit_CBSS_RUS_EST)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_RUS_EST)
plot_CBSS_RUS_EST <- p + ggtitle(paste( "RUS - EST", "(", "R2 = ",signif(summary(fit_CBSS_RUS_EST)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- RUS_FIN$ln.tradeop

fit_CBSS_RUS_FIN <- lm(y ~ x1) 
summary(fit_CBSS_RUS_FIN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_RUS_FIN)
plot_CBSS_RUS_FIN <- p + ggtitle(paste( "RUS - FIN", "(", "R2 = ",signif(summary(fit_CBSS_RUS_FIN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- RUS_GFR$ln.tradeop

fit_CBSS_RUS_GFR <- lm(y ~ x1) 
summary(fit_CBSS_RUS_GFR)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_RUS_GFR)
plot_CBSS_RUS_GFR <- p + ggtitle(paste( "RUS - GFR", "(", "R2 = ",signif(summary(fit_CBSS_RUS_GFR)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- RUS_LAT$ln.tradeop

fit_CBSS_RUS_LAT <- lm(y ~ x1) 
summary(fit_CBSS_RUS_LAT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_RUS_LAT)
plot_CBSS_RUS_LAT <- p + ggtitle(paste( "RUS - LAT", "(", "R2 = ",signif(summary(fit_CBSS_RUS_LAT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- RUS_LIT$ln.tradeop

fit_CBSS_RUS_LIT <- lm(y ~ x1) 
summary(fit_CBSS_RUS_LIT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_RUS_LIT)
plot_CBSS_RUS_LIT <- p + ggtitle(paste( "RUS - LIT", "(", "R2 = ",signif(summary(fit_CBSS_RUS_LIT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- RUS_POL$ln.tradeop

fit_CBSS_RUS_POL <- lm(y ~ x1) 
summary(fit_CBSS_RUS_POL)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_RUS_POL)
plot_CBSS_RUS_POL <- p + ggtitle(paste( "RUS - POL", "(", "R2 = ",signif(summary(fit_CBSS_RUS_POL)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- RUS_SWD$ln.tradeop

fit_CBSS_RUS_SWD <- lm(y ~ x1) 
summary(fit_CBSS_RUS_SWD)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_RUS_SWD)
plot_CBSS_RUS_SWD <- p + ggtitle(paste( "RUS - SWD", "(", "R2 = ",signif(summary(fit_CBSS_RUS_SWD)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- SWD_DEN$ln.tradeop

fit_CBSS_SWD_DEN <- lm(y ~ x1) 
summary(fit_CBSS_SWD_DEN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_SWD_DEN)
plot_CBSS_SWD_DEN <- p + ggtitle(paste( "SWD - DEN", "(", "R2 = ",signif(summary(fit_CBSS_SWD_DEN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- SWD_EST$ln.tradeop

fit_CBSS_SWD_EST <- lm(y ~ x1) 
summary(fit_CBSS_SWD_EST)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_SWD_EST)
plot_CBSS_SWD_EST <- p + ggtitle(paste( "SWD - EST", "(", "R2 = ",signif(summary(fit_CBSS_SWD_EST)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- SWD_FIN$ln.tradeop

fit_CBSS_SWD_FIN <- lm(y ~ x1) 
summary(fit_CBSS_SWD_FIN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_SWD_FIN)
plot_CBSS_SWD_FIN <- p + ggtitle(paste( "SWD - FIN", "(", "R2 = ",signif(summary(fit_CBSS_SWD_FIN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- SWD_GFR$ln.tradeop

fit_CBSS_SWD_GFR <- lm(y ~ x1) 
summary(fit_CBSS_SWD_GFR)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_SWD_GFR)
plot_CBSS_SWD_GFR <- p + ggtitle(paste( "SWD - GFR", "(", "R2 = ",signif(summary(fit_CBSS_SWD_GFR)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- SWD_LAT$ln.tradeop

fit_CBSS_SWD_LAT <- lm(y ~ x1) 
summary(fit_CBSS_SWD_LAT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_SWD_LAT)
plot_CBSS_SWD_LAT <- p + ggtitle(paste( "SWD - LAT", "(", "R2 = ",signif(summary(fit_CBSS_SWD_LAT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- SWD_LIT$ln.tradeop

fit_CBSS_SWD_LIT <- lm(y ~ x1) 
summary(fit_CBSS_SWD_LIT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_SWD_LIT)
plot_CBSS_SWD_LIT <- p + ggtitle(paste( "SWD - LIT", "(", "R2 = ",signif(summary(fit_CBSS_SWD_LIT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- SWD_POL$ln.tradeop

fit_CBSS_SWD_POL <- lm(y ~ x1) 
summary(fit_CBSS_SWD_POL)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_SWD_POL)
plot_CBSS_SWD_POL <- p + ggtitle(paste( "SWD - POL", "(", "R2 = ",signif(summary(fit_CBSS_SWD_POL)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$cbss_cum
y <- SWD_RUS$ln.tradeop

fit_CBSS_SWD_RUS <- lm(y ~ x1) 
summary(fit_CBSS_SWD_RUS)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_CBSS_SWD_RUS)
plot_CBSS_SWD_RUS <- p + ggtitle(paste( "SWD - RUS", "(", "R2 = ",signif(summary(fit_CBSS_SWD_RUS)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))



# ggplot(GFR_RUS, aes(GFR_RUS$year)) + geom_line(aes(y = GFR_RUS$expab, col = "EXP")) + geom_line(aes(y = GFR_RUS$impab, col = "IMP")) +  geom_line(aes(y = GFR_RUS$tradeop, col = "TO"))
# EU ######################################################
y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- DEN_EST$ln.tradeop

fit_EU_DEN_EST <- lm(y ~ x1) 
summary(fit_EU_DEN_EST)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_DEN_EST)
plot_EU_DEN_EST <- p + ggtitle(paste( "DEN - EST", "(", "R2 = ",signif(summary(fit_EU_DEN_EST)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- DEN_FIN$ln.tradeop

fit_EU_DEN_FIN <- lm(y ~ x1) 
summary(fit_EU_DEN_FIN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_DEN_FIN)
plot_EU_DEN_FIN <- p + ggtitle(paste( "DEN - FIN", "(", "R2 = ",signif(summary(fit_EU_DEN_FIN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- DEN_GFR$ln.tradeop

fit_EU_DEN_GFR <- lm(y ~ x1) 
summary(fit_EU_DEN_GFR)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_DEN_GFR)
plot_EU_DEN_GFR <- p + ggtitle(paste( "DEN - GFR", "(", "R2 = ",signif(summary(fit_EU_DEN_GFR)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- DEN_LAT$ln.tradeop

fit_EU_DEN_LAT <- lm(y ~ x1) 
summary(fit_EU_DEN_LAT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_DEN_LAT)
plot_EU_DEN_LAT <- p + ggtitle(paste( "DEN - LAT", "(", "R2 = ",signif(summary(fit_EU_DEN_LAT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- DEN_LIT$ln.tradeop

fit_EU_DEN_LIT <- lm(y ~ x1) 
summary(fit_EU_DEN_LIT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_DEN_LIT)
plot_EU_DEN_LIT <- p + ggtitle(paste( "DEN - LIT", "(", "R2 = ",signif(summary(fit_EU_DEN_LIT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- DEN_POL$ln.tradeop

fit_EU_DEN_POL <- lm(y ~ x1) 
summary(fit_EU_DEN_POL)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_DEN_POL)
plot_EU_DEN_POL <- p + ggtitle(paste( "DEN - POL", "(", "R2 = ",signif(summary(fit_EU_DEN_POL)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- DEN_SWD$ln.tradeop

fit_EU_DEN_SWD <- lm(y ~ x1) 
summary(fit_EU_DEN_SWD)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_DEN_SWD)
plot_EU_DEN_SWD <- p + ggtitle(paste( "DEN - SWD", "(", "R2 = ",signif(summary(fit_EU_DEN_SWD)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- EST_DEN$ln.tradeop

fit_EU_EST_DEN <- lm(y ~ x1) 
summary(fit_EU_EST_DEN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_EST_DEN)
plot_EU_EST_DEN <- p + ggtitle(paste( "EST - DEN", "(", "R2 = ",signif(summary(fit_EU_EST_DEN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- EST_FIN$ln.tradeop

fit_EU_EST_FIN <- lm(y ~ x1) 
summary(fit_EU_EST_FIN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_EST_FIN)
plot_EU_EST_FIN <- p + ggtitle(paste( "EST - FIN", "(", "R2 = ",signif(summary(fit_EU_EST_FIN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- EST_GFR$ln.tradeop

fit_EU_EST_GFR <- lm(y ~ x1) 
summary(fit_EU_EST_GFR)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_EST_GFR)
plot_EU_EST_GFR <- p + ggtitle(paste( "EST - GFR", "(", "R2 = ",signif(summary(fit_EU_EST_GFR)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- EST_LAT$ln.tradeop

fit_EU_EST_LAT <- lm(y ~ x1) 
summary(fit_EU_EST_LAT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_EST_LAT)
plot_EU_EST_LAT <- p + ggtitle(paste( "EST - LAT", "(", "R2 = ",signif(summary(fit_EU_EST_LAT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- EST_LIT$ln.tradeop

fit_EU_EST_LIT <- lm(y ~ x1) 
summary(fit_EU_EST_LIT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_EST_LIT)
plot_EU_EST_LIT <- p + ggtitle(paste( "EST - LIT", "(", "R2 = ",signif(summary(fit_EU_EST_LIT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- EST_POL$ln.tradeop

fit_EU_EST_POL <- lm(y ~ x1) 
summary(fit_EU_EST_POL)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_EST_POL)
plot_EU_EST_POL <- p + ggtitle(paste( "EST - POL", "(", "R2 = ",signif(summary(fit_EU_EST_POL)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- EST_SWD$ln.tradeop

fit_EU_EST_SWD <- lm(y ~ x1) 
summary(fit_EU_EST_SWD)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_EST_SWD)
plot_EU_EST_SWD <- p + ggtitle(paste( "EST - SWD", "(", "R2 = ",signif(summary(fit_EU_EST_SWD)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- FIN_DEN$ln.tradeop

fit_EU_FIN_DEN <- lm(y ~ x1) 
summary(fit_EU_FIN_DEN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_FIN_DEN)
plot_EU_FIN_DEN <- p + ggtitle(paste( "FIN - DEN", "(", "R2 = ",signif(summary(fit_EU_FIN_DEN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- FIN_EST$ln.tradeop

fit_EU_FIN_EST <- lm(y ~ x1) 
summary(fit_EU_FIN_EST)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_FIN_EST)
plot_EU_FIN_EST <- p + ggtitle(paste( "FIN - EST", "(", "R2 = ",signif(summary(fit_EU_FIN_EST)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- FIN_GFR$ln.tradeop

fit_EU_FIN_GFR <- lm(y ~ x1) 
summary(fit_EU_FIN_GFR)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_FIN_GFR)
plot_EU_FIN_GFR <- p + ggtitle(paste( "FIN - GFR", "(", "R2 = ",signif(summary(fit_EU_FIN_GFR)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- FIN_LAT$ln.tradeop

fit_EU_FIN_LAT <- lm(y ~ x1) 
summary(fit_EU_FIN_LAT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_FIN_LAT)
plot_EU_FIN_LAT <- p + ggtitle(paste( "FIN - LAT", "(", "R2 = ",signif(summary(fit_EU_FIN_LAT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- FIN_LIT$ln.tradeop

fit_EU_FIN_LIT <- lm(y ~ x1) 
summary(fit_EU_FIN_LIT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_FIN_LIT)
plot_EU_FIN_LIT <- p + ggtitle(paste( "FIN - LIT", "(", "R2 = ",signif(summary(fit_EU_FIN_LIT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- FIN_POL$ln.tradeop

fit_EU_FIN_POL <- lm(y ~ x1) 
summary(fit_EU_FIN_POL)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_FIN_POL)
plot_EU_FIN_POL <- p + ggtitle(paste( "FIN - POL", "(", "R2 = ",signif(summary(fit_EU_FIN_POL)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- FIN_SWD$ln.tradeop

fit_EU_FIN_SWD <- lm(y ~ x1) 
summary(fit_EU_FIN_SWD)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_FIN_SWD)
plot_EU_FIN_SWD <- p + ggtitle(paste( "FIN - SWD", "(", "R2 = ",signif(summary(fit_EU_FIN_SWD)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- GFR_DEN$ln.tradeop

fit_EU_GFR_DEN <- lm(y ~ x1) 
summary(fit_EU_GFR_DEN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_GFR_DEN)
plot_EU_GFR_DEN <- p + ggtitle(paste( "GFR - DEN", "(", "R2 = ",signif(summary(fit_EU_GFR_DEN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- GFR_EST$ln.tradeop

fit_EU_GFR_EST <- lm(y ~ x1) 
summary(fit_EU_GFR_EST)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_GFR_EST)
plot_EU_GFR_EST <- p + ggtitle(paste( "GFR - EST", "(", "R2 = ",signif(summary(fit_EU_GFR_EST)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- GFR_FIN$ln.tradeop

fit_EU_GFR_FIN <- lm(y ~ x1) 
summary(fit_EU_GFR_FIN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_GFR_FIN)
plot_EU_GFR_FIN <- p + ggtitle(paste( "GFR - FIN", "(", "R2 = ",signif(summary(fit_EU_GFR_FIN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- GFR_LAT$ln.tradeop

fit_EU_GFR_LAT <- lm(y ~ x1) 
summary(fit_EU_GFR_LAT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_GFR_LAT)
plot_EU_GFR_LAT <- p + ggtitle(paste( "GFR - LAT", "(", "R2 = ",signif(summary(fit_EU_GFR_LAT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- GFR_LIT$ln.tradeop

fit_EU_GFR_LIT <- lm(y ~ x1) 
summary(fit_EU_GFR_LIT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_GFR_LIT)
plot_EU_GFR_LIT <- p + ggtitle(paste( "GFR - LIT", "(", "R2 = ",signif(summary(fit_EU_GFR_LIT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- GFR_POL$ln.tradeop

fit_EU_GFR_POL <- lm(y ~ x1) 
summary(fit_EU_GFR_POL)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_GFR_POL)
plot_EU_GFR_POL <- p + ggtitle(paste( "GFR - POL", "(", "R2 = ",signif(summary(fit_EU_GFR_POL)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- GFR_SWD$ln.tradeop

fit_EU_GFR_SWD <- lm(y ~ x1) 
summary(fit_EU_GFR_SWD)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_GFR_SWD)
plot_EU_GFR_SWD <- p + ggtitle(paste( "GFR - SWD", "(", "R2 = ",signif(summary(fit_EU_GFR_SWD)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- LAT_DEN$ln.tradeop

fit_EU_LAT_DEN <- lm(y ~ x1) 
summary(fit_EU_LAT_DEN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_LAT_DEN)
plot_EU_LAT_DEN <- p + ggtitle(paste( "LAT - DEN", "(", "R2 = ",signif(summary(fit_EU_LAT_DEN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- LAT_EST$ln.tradeop

fit_EU_LAT_EST <- lm(y ~ x1) 
summary(fit_EU_LAT_EST)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_LAT_EST)
plot_EU_LAT_EST <- p + ggtitle(paste( "LAT - EST", "(", "R2 = ",signif(summary(fit_EU_LAT_EST)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- LAT_FIN$ln.tradeop

fit_EU_LAT_FIN <- lm(y ~ x1) 
summary(fit_EU_LAT_FIN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_LAT_FIN)
plot_EU_LAT_FIN <- p + ggtitle(paste( "LAT - FIN", "(", "R2 = ",signif(summary(fit_EU_LAT_FIN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- LAT_GFR$ln.tradeop

fit_EU_LAT_GFR <- lm(y ~ x1) 
summary(fit_EU_LAT_GFR)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_LAT_GFR)
plot_EU_LAT_GFR <- p + ggtitle(paste( "LAT - GFR", "(", "R2 = ",signif(summary(fit_EU_LAT_GFR)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- LAT_LIT$ln.tradeop

fit_EU_LAT_LIT <- lm(y ~ x1) 
summary(fit_EU_LAT_LIT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_LAT_LIT)
plot_EU_LAT_LIT <- p + ggtitle(paste( "LAT - LIT", "(", "R2 = ",signif(summary(fit_EU_LAT_LIT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- LAT_POL$ln.tradeop

fit_EU_LAT_POL <- lm(y ~ x1) 
summary(fit_EU_LAT_POL)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_LAT_POL)
plot_EU_LAT_POL <- p + ggtitle(paste( "LAT - POL", "(", "R2 = ",signif(summary(fit_EU_LAT_POL)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- LAT_SWD$ln.tradeop

fit_EU_LAT_SWD <- lm(y ~ x1) 
summary(fit_EU_LAT_SWD)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_LAT_SWD)
plot_EU_LAT_SWD <- p + ggtitle(paste( "LAT - SWD", "(", "R2 = ",signif(summary(fit_EU_LAT_SWD)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- LIT_DEN$ln.tradeop

fit_EU_LIT_DEN <- lm(y ~ x1) 
summary(fit_EU_LIT_DEN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_LIT_DEN)
plot_EU_LIT_DEN <- p + ggtitle(paste( "LIT - DEN", "(", "R2 = ",signif(summary(fit_EU_LIT_DEN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- LIT_EST$ln.tradeop

fit_EU_LIT_EST <- lm(y ~ x1) 
summary(fit_EU_LIT_EST)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_LIT_EST)
plot_EU_LIT_EST <- p + ggtitle(paste( "LIT - EST", "(", "R2 = ",signif(summary(fit_EU_LIT_EST)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- LIT_FIN$ln.tradeop

fit_EU_LIT_FIN <- lm(y ~ x1) 
summary(fit_EU_LIT_FIN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_LIT_FIN)
plot_EU_LIT_FIN <- p + ggtitle(paste( "LIT - FIN", "(", "R2 = ",signif(summary(fit_EU_LIT_FIN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- LIT_GFR$ln.tradeop

fit_EU_LIT_GFR <- lm(y ~ x1) 
summary(fit_EU_LIT_GFR)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_LIT_GFR)
plot_EU_LIT_GFR <- p + ggtitle(paste( "LIT - GFR", "(", "R2 = ",signif(summary(fit_EU_LIT_GFR)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- LIT_LAT$ln.tradeop

fit_EU_LIT_LAT <- lm(y ~ x1) 
summary(fit_EU_LIT_LAT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_LIT_LAT)
plot_EU_LIT_LAT <- p + ggtitle(paste( "LIT - LAT", "(", "R2 = ",signif(summary(fit_EU_LIT_LAT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- LIT_POL$ln.tradeop

fit_EU_LIT_POL <- lm(y ~ x1) 
summary(fit_EU_LIT_POL)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_LIT_POL)
plot_EU_LIT_POL <- p + ggtitle(paste( "LIT - POL", "(", "R2 = ",signif(summary(fit_EU_LIT_POL)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- LIT_SWD$ln.tradeop

fit_EU_LIT_SWD <- lm(y ~ x1) 
summary(fit_EU_LIT_SWD)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_LIT_SWD)
plot_EU_LIT_SWD <- p + ggtitle(paste( "LIT - SWD", "(", "R2 = ",signif(summary(fit_EU_LIT_SWD)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- POL_DEN$ln.tradeop

fit_EU_POL_DEN <- lm(y ~ x1) 
summary(fit_EU_POL_DEN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_POL_DEN)
plot_EU_POL_DEN <- p + ggtitle(paste( "POL - DEN", "(", "R2 = ",signif(summary(fit_EU_POL_DEN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- POL_EST$ln.tradeop

fit_EU_POL_EST <- lm(y ~ x1) 
summary(fit_EU_POL_EST)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_POL_EST)
plot_EU_POL_EST <- p + ggtitle(paste( "POL - EST", "(", "R2 = ",signif(summary(fit_EU_POL_EST)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- POL_FIN$ln.tradeop

fit_EU_POL_FIN <- lm(y ~ x1) 
summary(fit_EU_POL_FIN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_POL_FIN)
plot_EU_POL_FIN <- p + ggtitle(paste( "POL - FIN", "(", "R2 = ",signif(summary(fit_EU_POL_FIN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- POL_GFR$ln.tradeop

fit_EU_POL_GFR <- lm(y ~ x1) 
summary(fit_EU_POL_GFR)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_POL_GFR)
plot_EU_POL_GFR <- p + ggtitle(paste( "POL - GFR", "(", "R2 = ",signif(summary(fit_EU_POL_GFR)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- POL_LAT$ln.tradeop

fit_EU_POL_LAT <- lm(y ~ x1) 
summary(fit_EU_POL_LAT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_POL_LAT)
plot_EU_POL_LAT <- p + ggtitle(paste( "POL - LAT", "(", "R2 = ",signif(summary(fit_EU_POL_LAT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- POL_LIT$ln.tradeop

fit_EU_POL_LIT <- lm(y ~ x1) 
summary(fit_EU_POL_LIT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_POL_LIT)
plot_EU_POL_LIT <- p + ggtitle(paste( "POL - LIT", "(", "R2 = ",signif(summary(fit_EU_POL_LIT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- POL_SWD$ln.tradeop

fit_EU_POL_SWD <- lm(y ~ x1) 
summary(fit_EU_POL_SWD)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_POL_SWD)
plot_EU_POL_SWD <- p + ggtitle(paste( "POL - SWD", "(", "R2 = ",signif(summary(fit_EU_POL_SWD)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- SWD_DEN$ln.tradeop

fit_EU_SWD_DEN <- lm(y ~ x1) 
summary(fit_EU_SWD_DEN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_SWD_DEN)
plot_EU_SWD_DEN <- p + ggtitle(paste( "SWD - DEN", "(", "R2 = ",signif(summary(fit_EU_SWD_DEN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- SWD_EST$ln.tradeop

fit_EU_SWD_EST <- lm(y ~ x1) 
summary(fit_EU_SWD_EST)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_SWD_EST)
plot_EU_SWD_EST <- p + ggtitle(paste( "SWD - EST", "(", "R2 = ",signif(summary(fit_EU_SWD_EST)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- SWD_FIN$ln.tradeop

fit_EU_SWD_FIN <- lm(y ~ x1) 
summary(fit_EU_SWD_FIN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_SWD_FIN)
plot_EU_SWD_FIN <- p + ggtitle(paste( "SWD - FIN", "(", "R2 = ",signif(summary(fit_EU_SWD_FIN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- SWD_GFR$ln.tradeop

fit_EU_SWD_GFR <- lm(y ~ x1) 
summary(fit_EU_SWD_GFR)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_SWD_GFR)
plot_EU_SWD_GFR <- p + ggtitle(paste( "SWD - GFR", "(", "R2 = ",signif(summary(fit_EU_SWD_GFR)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- SWD_LAT$ln.tradeop

fit_EU_SWD_LAT <- lm(y ~ x1) 
summary(fit_EU_SWD_LAT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_SWD_LAT)
plot_EU_SWD_LAT <- p + ggtitle(paste( "SWD - LAT", "(", "R2 = ",signif(summary(fit_EU_SWD_LAT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- SWD_LIT$ln.tradeop

fit_EU_SWD_LIT <- lm(y ~ x1) 
summary(fit_EU_SWD_LIT)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_SWD_LIT)
plot_EU_SWD_LIT <- p + ggtitle(paste( "SWD - LIT", "(", "R2 = ",signif(summary(fit_EU_SWD_LIT)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$eu_cum
y <- SWD_POL$ln.tradeop

fit_EU_SWD_POL <- lm(y ~ x1) 
summary(fit_EU_SWD_POL)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_EU_SWD_POL)
plot_EU_SWD_POL <- p + ggtitle(paste( "SWD - POL", "(", "R2 = ",signif(summary(fit_EU_SWD_POL)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

# NORDEN ##################################################

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$norden_cum
y <- DEN_FIN$ln.tradeop

fit_Norden_DEN_FIN <- lm(y ~ x1) 
summary(fit_Norden_DEN_FIN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_Norden_DEN_FIN)
plot_Norden_DEN_FIN <- p + ggtitle(paste( "DEN - FIN", "(", "R2 = ",signif(summary(fit_Norden_DEN_FIN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$norden_cum
y <- DEN_SWD$ln.tradeop

fit_Norden_DEN_SWD <- lm(y ~ x1) 
summary(fit_Norden_DEN_SWD)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_Norden_DEN_SWD)
plot_Norden_DEN_SWD <- p + ggtitle(paste( "DEN - SWD", "(", "R2 = ",signif(summary(fit_Norden_DEN_SWD)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$norden_cum
y <- FIN_DEN$ln.tradeop

fit_Norden_FIN_DEN <- lm(y ~ x1) 
summary(fit_Norden_FIN_DEN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_Norden_FIN_DEN)
plot_Norden_FIN_DEN <- p + ggtitle(paste( "FIN - DEN", "(", "R2 = ",signif(summary(fit_Norden_FIN_DEN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$norden_cum
y <- FIN_SWD$ln.tradeop

fit_Norden_FIN_SWD <- lm(y ~ x1) 
summary(fit_Norden_FIN_SWD)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_Norden_FIN_SWD)
plot_Norden_FIN_SWD <- p + ggtitle(paste( "FIN - SWD", "(", "R2 = ",signif(summary(fit_Norden_FIN_SWD)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$norden_cum
y <- SWD_DEN$ln.tradeop

fit_Norden_SWD_DEN <- lm(y ~ x1) 
summary(fit_Norden_SWD_DEN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_Norden_SWD_DEN)
plot_Norden_SWD_DEN <- p + ggtitle(paste( "SWD - DEN", "(", "R2 = ",signif(summary(fit_Norden_SWD_DEN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))

y <- NULL
x1 <- NULL
x2 <- NULL
fit <- NULL
x1 <- Norms$norden_cum
y <- SWD_FIN$ln.tradeop

fit_Norden_SWD_FIN <- lm(y ~ x1) 
summary(fit_Norden_SWD_FIN)
corr <- cor(x1, y, use = "pairwise.complete.obs")
p <- ggplotRegression(fit_Norden_SWD_FIN)
plot_Norden_SWD_FIN <- p + ggtitle(paste( "SWD - FIN", "(", "R2 = ",signif(summary(fit_Norden_SWD_FIN)$adj.r.squared, 4), "r = ", signif(corr,4), ")")) + theme(plot.title = element_text(size = rel(0.8)), axis.title.x=element_blank(), axis.title.y=element_blank(), text = element_text(size=4), axis.ticks = element_line(size = 0.2))


# Plots Presentation ############
# BA
tikz(sanitize = TRUE, file = "Illustrations/BA_TO.tex", width = 4, height = 6)
grid.arrange(plot_BA_EST_LAT, plot_BA_EST_LIT, plot_BA_LAT_EST, plot_BA_LAT_LIT, plot_BA_LIT_EST, plot_BA_LIT_LAT, ncol = 2, left = textGrob("Log Trade Openness [%]", gp=gpar(fontsize=4), rot = 90), bottom = textGrob("Norms", gp=gpar(fontsize=4)))
dev.off()

# CBSS
tikz(sanitize = TRUE, file = "Illustrations/CBSS_TO.tex", width = 4, height = 6)
grid.arrange(plot_CBSS_SWD_GFR, plot_CBSS_FIN_DEN, plot_CBSS_RUS_DEN, plot_CBSS_DEN_RUS, plot_CBSS_EST_RUS, plot_CBSS_RUS_EST , ncol = 2, left = textGrob("Log Trade Openness [%]", gp=gpar(fontsize=4), rot = 90), bottom = textGrob("Norms", gp=gpar(fontsize=4)))
dev.off()

# EU
tikz(sanitize = TRUE, file = "Illustrations/EU_TO.tex", width = 4, height = 6)
grid.arrange(plot_EU_DEN_FIN, plot_EU_FIN_DEN, plot_EU_GFR_POL, plot_EU_POL_GFR, plot_EU_EST_FIN, plot_EU_LAT_SWD, ncol = 2, left = textGrob("Log Trade Openness [%]", gp=gpar(fontsize=4), rot = 90), bottom = textGrob("Norms", gp=gpar(fontsize=4)))
dev.off()

# Norden
tikz(sanitize = TRUE, file = "Illustrations/Norden_TO.tex", width = 4, height = 6)
grid.arrange(plot_Norden_DEN_FIN, plot_Norden_DEN_SWD, plot_Norden_FIN_DEN, plot_Norden_FIN_SWD, plot_Norden_SWD_DEN, plot_Norden_SWD_FIN, ncol = 2, left = textGrob("Log Trade Openness [%]", gp=gpar(fontsize=4), rot = 90), bottom = textGrob("Norms", gp=gpar(fontsize=4)))
dev.off()

# Plots Paper ############
# BA
tikz(sanitize = TRUE, file = "Illustrations/PAPER_BA_TO.tex", width = 6, height = 6)
grid.arrange(paper_plot_BA_EST_LAT, paper_plot_BA_EST_LIT, paper_plot_BA_LAT_EST, paper_plot_BA_LAT_LIT, paper_plot_BA_LIT_EST, paper_plot_BA_LIT_LAT, ncol = 2, left = textGrob("ln Trade Openness [%]", gp=gpar(fontsize=8), rot = 90), bottom = textGrob("Norms", gp=gpar(fontsize=8)))
dev.off()

# CBSS
tikz(sanitize = TRUE, file = "Illustrations/PAPER_CBSS_TO.tex", width = 6, height = 6)
grid.arrange(paper_plot_CBSS_SWD_GFR, paper_plot_CBSS_FIN_DEN, paper_plot_CBSS_RUS_DEN, paper_plot_CBSS_DEN_RUS, paper_plot_CBSS_EST_RUS, paper_plot_CBSS_RUS_EST , ncol = 2, left = textGrob("ln Trade Openness [%]", gp=gpar(fontsize=8), rot = 90), bottom = textGrob("Norms", gp=gpar(fontsize=8)))
dev.off()

# EU
tikz(sanitize = TRUE, file = "Illustrations/PAPER_EU_TO.tex", width = 6, height = 6)
grid.arrange(paper_plot_EU_DEN_FIN, paper_plot_EU_FIN_DEN, paper_plot_EU_GFR_POL, paper_plot_EU_POL_GFR, paper_plot_EU_EST_FIN, paper_plot_EU_LAT_SWD, ncol = 2, left = textGrob("ln Trade Openness [%]", gp=gpar(fontsize=8), rot = 90), bottom = textGrob("Norms", gp=gpar(fontsize=8)))
dev.off()

# Norden
tikz(sanitize = TRUE, file = "Illustrations/PAPER_Norden_TO.tex", width = 6, height = 6)
grid.arrange(paper_plot_Norden_DEN_FIN, paper_plot_Norden_DEN_SWD, paper_plot_Norden_FIN_DEN, paper_plot_Norden_FIN_SWD, paper_plot_Norden_SWD_DEN, paper_plot_Norden_SWD_FIN, ncol = 2, left = textGrob("ln Trade Openness [%]", gp=gpar(fontsize=8), rot = 90), bottom = textGrob("Norms", gp=gpar(fontsize=8)))
dev.off()

# Tables ##########################
# BA
stargazer(fit_BA_EST_LAT, fit_BA_EST_LAT, fit_BA_LAT_EST, fit_BA_LAT_LIT, fit_BA_LIT_EST, fit_BA_LIT_LAT)
# CBSS
stargazer(fit_CBSS_SWD_GFR, fit_CBSS_FIN_DEN, fit_CBSS_RUS_DEN, fit_CBSS_DEN_RUS, fit_CBSS_EST_RUS, fit_CBSS_RUS_EST)
# EU
stargazer(fit_EU_DEN_FIN, fit_EU_FIN_DEN, fit_EU_GFR_POL, fit_EU_POL_GFR, fit_EU_EST_FIN, fit_EU_LAT_SWD)
# Norden
stargazer(fit_Norden_DEN_FIN, fit_Norden_DEN_SWD, fit_Norden_FIN_DEN, fit_Norden_FIN_SWD, fit_Norden_SWD_DEN, fit_Norden_SWD_FIN)
# Gravity ##########################

library(stargazer)
library(ggrepel)
library(tikzDevice)
library(grid)

# 1995
BSR_distance_1995_names <- transform(BSR_distance_1995, newcol=paste(BSR_distance_1995$acra, BSR_distance_1995$acrb, sep=" - "))
Legend <- BSR_distance_1995_names$newcol

corr_dist <- NULL
BSR_divided <- BSR_distance_1995$tradeop/100
corr_dist <- lm(log(BSR_divided) ~ log(BSR_distance_1995$Distance) + BSR_distance_1995$PolitySC2)
summary(corr_dist)
corr <- cor(log(BSR_distance_1995$Distance), log(BSR_divided), use = "pairwise.complete.obs")
p <-ggplotRegression(corr_dist)
plot_dist_1995_paper <- p  + ggtitle(paste( "1995", "(", "R2 = ",signif(summary(corr_dist)$adj.r.squared, 5), "r = ", signif(corr,5), ")")) + xlab("ln Distance") + ylab("ln Trade Openness [%]") + theme(plot.title = element_text(size = rel(1.2), vjust = 0), 
                                                                                                                                                                                                       axis.title = element_text(size = rel(1.2)),
                                                                                                                                                                                                       text = element_text(size=8))
plot_dist_1995 <- p + ggtitle(paste( "Gravity Model of Trade Openness 1995", "(", "R2 = ",signif(summary(corr_dist)$adj.r.squared, 5), "r = ", signif(corr,5), ")")) + xlab("ln Distance") + ylab("ln Trade Openness [%]") + 
  geom_label_repel(aes(label = as.character(Legend)), show.legend = TRUE, size = 1, segment.size = 0.2, label.size = 0.1,box.padding = unit(0.1, "lines"),
                   point.padding = unit(0.5, "lines")) + scale_color_manual(name="Legend")+ theme(plot.title = element_text(size = rel(0.8), vjust = 0), 
                                                                                                  axis.title = element_text(size = rel(0.8)),
                                                                                                  text = element_text(size=5),
                                                                                                  axis.title.y = element_text( vjust=2 ),
                                                                                                  axis.title.x = element_text( vjust=-0.5 ))

summ_dist_1995 <- summary(corr_dist)
corr_dist_1995 <- corr_dist

tikz(sanitize = TRUE, file = "Illustrations/BSR_Distance_1995.tex", width = 3, height = 3)
plot_dist_1995
dev.off()

tikz(sanitize = TRUE, file = "Illustrations/BSR_Distance_1995_paper.tex", width = 3, height = 3)
plot_dist_1995_paper
dev.off()


# 2000
BSR_distance_2000_names <- transform(BSR_distance_2000, newcol=paste(BSR_distance_2000$acra, BSR_distance_2000$acrb, sep=" - "))
Legend <- BSR_distance_2000_names$newcol

BSR_divided <- BSR_distance_2000$tradeop/100
corr_dist <- lm(log(BSR_divided) ~ log(BSR_distance_2000$Distance) + BSR_distance_2000$PolitySC2)
summary(corr_dist)
corr <- cor(log(BSR_distance_2000$Distance), log(BSR_divided), use = "pairwise.complete.obs")
p <-ggplotRegression(corr_dist)
plot_dist_2000_paper <- p  + ggtitle(paste( "2000", "(", "R2 = ",signif(summary(corr_dist)$adj.r.squared, 5), "r = ", signif(corr,5), ")")) + xlab("ln Distance") + ylab("ln Trade Openness [%]") + theme(plot.title = element_text(size = rel(1.2), vjust = 0), 
                                                                                                                                                                                                        axis.title = element_text(size = rel(1.2)),
                                                                                                                                                                                                        text = element_text(size=8))
plot_dist_2000 <- p + ggtitle(paste( "Gravity Model of Trade Openness 2000", "(", "R2 = ",signif(summary(corr_dist)$adj.r.squared, 5), "r = ", signif(corr,5), ")")) + xlab("ln Distance") + ylab("ln Trade Openness [%]") + 
  geom_label_repel(aes(label = as.character(Legend)), show.legend = TRUE, size = 1, segment.size = 0.2, label.size = 0.1,box.padding = unit(0.1, "lines"),
                   point.padding = unit(0.5, "lines")) + scale_color_manual(name="Legend")+ theme(plot.title = element_text(size = rel(0.8), vjust = 0), 
                                                                                                  axis.title = element_text(size = rel(0.8)),
                                                                                                  text = element_text(size=5),
                                                                                                  axis.title.y = element_text( vjust=2 ),
                                                                                                  axis.title.x = element_text( vjust=-0.5 ))

corr_dist_2000 <- corr_dist

tikz(sanitize = TRUE, file = "Illustrations/BSR_Distance_2000.tex", width = 3, height = 3)
plot_dist_2000
dev.off()

tikz(sanitize = TRUE, file = "Illustrations/BSR_Distance_2000_paper.tex", width = 3, height = 3)
plot_dist_2000_paper
dev.off()

# 2005
BSR_distance_2005_names <- transform(BSR_distance_2005, newcol=paste(BSR_distance_2005$acra, BSR_distance_2005$acrb, sep=" - "))
Legend <- BSR_distance_2005_names$newcol

BSR_divided <- BSR_distance_2005$tradeop/100
corr_dist <- lm(log(BSR_divided) ~ log(BSR_distance_2005$Distance) + BSR_distance_2005$PolitySC2)
summary(corr_dist)
corr <- cor(log(BSR_distance_2005$Distance), log(BSR_divided), use = "pairwise.complete.obs")
p <-ggplotRegression(corr_dist)
plot_dist_2005_paper <- p  + ggtitle(paste( "2005", "(", "R2 = ",signif(summary(corr_dist)$adj.r.squared, 5), "r = ", signif(corr,5), ")")) + xlab("ln Distance") + ylab("ln Trade Openness [%]") + theme(plot.title = element_text(size = rel(1.2), vjust = 0), 
                                                                                                                                                                                                       axis.title = element_text(size = rel(1.2)),
                                                                                                                                                                                                       text = element_text(size=8))
plot_dist_2005 <- p + ggtitle(paste( "Gravity Model of Trade Openness 2005", "(", "R2 = ",signif(summary(corr_dist)$adj.r.squared, 5), "r = ", signif(corr,5), ")")) + xlab("ln Distance") + ylab("ln Trade Openness [%]") + 
  geom_label_repel(aes(label = as.character(Legend)), show.legend = TRUE, size = 1, segment.size = 0.2, label.size = 0.1,box.padding = unit(0.1, "lines"),
                   point.padding = unit(0.5, "lines")) + scale_color_manual(name="Legend")+ theme(plot.title = element_text(size = rel(0.8), vjust = 0), 
                                                                                                  axis.title = element_text(size = rel(0.8)),
                                                                                                  text = element_text(size=5),
                                                                                                  axis.title.y = element_text( vjust=2 ),
                                                                                                  axis.title.x = element_text( vjust=-0.5 ))

summ_dist_2005 <- summary(corr_dist)
corr_dist_2005 <- corr_dist

tikz(sanitize = TRUE, file = "Illustrations/BSR_Distance_2005.tex", width = 3, height = 3)
plot_dist_2005
dev.off()

tikz(sanitize = TRUE, file = "Illustrations/BSR_Distance_2005_paper.tex", width = 3, height = 3)
plot_dist_2005_paper
dev.off()

# 2010
BSR_distance_2010_names <- transform(BSR_distance_2010, newcol=paste(BSR_distance_2010$acra, BSR_distance_2010$acrb, sep=" - "))
Legend <- BSR_distance_2010_names$newcol

BSR_divided <- BSR_distance_2010$tradeop/100
corr_dist <- lm(log(BSR_divided) ~ log(BSR_distance_2010$Distance) + BSR_distance_2010$PolitySC2)
summary(corr_dist)
corr <- cor(log(BSR_distance_2010$Distance), log(BSR_divided), use = "pairwise.complete.obs")
p <-ggplotRegression(corr_dist)
plot_dist_2010_paper <- p  + ggtitle(paste( "2010", "(", "R2 = ",signif(summary(corr_dist)$adj.r.squared, 5), "r = ", signif(corr,5), ")")) + xlab("ln Distance") + ylab("ln Trade Openness [%]") + theme(plot.title = element_text(size = rel(1.2), vjust = 0), 
                                                                                                                                                                                                       axis.title = element_text(size = rel(1.2)),
                                                                                                                                                                                                       text = element_text(size=8))
plot_dist_2010 <- p + ggtitle(paste( "Gravity Model of Trade Openness 2010", "(", "R2 = ",signif(summary(corr_dist)$adj.r.squared, 5), "r = ", signif(corr,5), ")")) + xlab("ln Distance") + ylab("ln Trade Openness [%]") + 
  geom_label_repel(aes(label = as.character(Legend)), show.legend = TRUE, size = 1, segment.size = 0.2, label.size = 0.1,box.padding = unit(0.1, "lines"),
                   point.padding = unit(0.5, "lines")) + scale_color_manual(name="Legend")+ theme(plot.title = element_text(size = rel(0.8), vjust = 0), 
                                                                                                  axis.title = element_text(size = rel(0.8)),
                                                                                                  text = element_text(size=5),
                                                                                                  axis.title.y = element_text( vjust=2 ),
                                                                                                  axis.title.x = element_text( vjust=-0.5 ))

summ_dist_2010 <- summary(corr_dist)
corr_dist_2010 <- corr_dist

tikz(sanitize = TRUE, file = "Illustrations/BSR_Distance_2010.tex", width = 3, height = 3)
plot_dist_2010
dev.off()

tikz(sanitize = TRUE, file = "Illustrations/BSR_Distance_2010_paper.tex", width = 3, height = 3)
plot_dist_2010_paper
dev.off()

# 2015
BSR_distance_2015_names <- transform(BSR_distance_2015, newcol=paste(BSR_distance_2015$acra, BSR_distance_2015$acrb, sep=" - "))
Legend <- BSR_distance_2015_names$newcol

BSR_divided <- BSR_distance_2015$tradeop/100
corr_dist <- lm(log(BSR_divided) ~ log(BSR_distance_2015$Distance) + BSR_distance_2015$PolitySC2)
summary(corr_dist)
corr <- cor(log(BSR_distance_2015$Distance), log(BSR_divided), use = "pairwise.complete.obs")
p <-ggplotRegression(corr_dist)
plot_dist_2015_paper <- p  + ggtitle(paste( "2015", "(", "R2 = ",signif(summary(corr_dist)$adj.r.squared, 5), "r = ", signif(corr,5), ")")) + xlab("ln Distance") + ylab("ln Trade Openness [%]") + theme(plot.title = element_text(size = rel(1.2), vjust = 0), 
        axis.title = element_text(size = rel(1.2)),
        text = element_text(size=8))
plot_dist_2015 <- p + ggtitle(paste( "Gravity Model of Trade Openness 2015", "(", "R2 = ",signif(summary(corr_dist)$adj.r.squared, 5), "r = ", signif(corr,5), ")")) + xlab("ln Distance") + ylab("ln Trade Openness [%]") + 
  geom_label_repel(aes(label = as.character(Legend)), show.legend = TRUE, size = 1, segment.size = 0.2, label.size = 0.1,box.padding = unit(0.1, "lines"),
                   point.padding = unit(0.5, "lines")) + scale_color_manual(name="Legend")+ theme(plot.title = element_text(size = rel(0.8), vjust = 0), 
                                                                                                  axis.title = element_text(size = rel(0.8)),
                                                                                                  text = element_text(size=5),
                                                                                                  axis.title.y = element_text( vjust=2 ),
                                                                                                  axis.title.x = element_text( vjust=-0.5 ))
corr_dist_2015 <- corr_dist
summ_dist_2015 <- summary(corr_dist)

tikz(sanitize = TRUE, file = "Illustrations/BSR_Distance_2015.tex", width = 3, height = 3)
plot_dist_2015
dev.off()

tikz(sanitize = TRUE, file = "Illustrations/BSR_Distance_2015_paper.tex", width = 3, height = 3)
plot_dist_2015_paper
dev.off()

stargazer(corr_dist_1991, corr_dist_1995, corr_dist_2000, corr_dist_2005, corr_dist_2010, corr_dist_2015)
