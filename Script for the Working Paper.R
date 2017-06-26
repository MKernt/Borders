# This .r script belongs to the working paper: 
# The Normative Influence of IGOs on their Member States: A Conceptualization and Measurement Approach
# by Martin kerntopf, 26.06.2017
# Please cite if used!

##install packages if needed
#install.packages("ggplot2")
#install.packages("ggthemes")
#install.packages("ggrepel")
#install.packages("grid")
#install.packages("gridExtra")
#install.packages("reshape2")
#install.packages("dplyr")
#install.packages("broom")
#install.packages("stargazer")
#install.packages("stringr")

library(ggplot2)
library(ggthemes)
library(ggrepel)
library(grid)
library(gridExtra)
library(reshape2)
library(dplyr)
library(broom)
library(stargazer)
library(stringr)


#####---------------------------------------------------------------------------------
##### This script needs the AoS dataset to be found at https://github.com/MKernt/Borders/tree/Norms
#### Global
## Model 1: OLS Regression for each Var
# Trade

global_reg_tradefreq <- lm(AoS$ln.tradeop ~ AoS$ECOfreq)
#p_global_reg_tradefreq <- summary(global_reg_tradefreq)

global_reg_tradecum <- lm(AoS$ln.tradeop ~ AoS$ECOcum)
#p_global_reg_tradecum <- summary(global_reg_tradecum)

# Migration

global_reg_migfreq <- lm(AoS$ln.migop ~ AoS$PMfreq)
#p_global_reg_migfreq <- summary(global_reg_migfreq)

global_reg_migcum <- lm(AoS$ln.migop ~ AoS$PMcum)
#p_global_reg_migcum <- summary(global_reg_migcum)

# Capital

global_reg_capfreq <- lm(AoS$ln.capop ~ AoS$CAPfreq)
#p_global_reg_capfreq <- summary(global_reg_capfreq)

global_reg_capcum <- lm(AoS$ln.capop ~ AoS$CAPcum)
#p_global_reg_capcum <- summary(global_reg_capcum)

# Services

global_reg_servfreq <- lm(AoS$ln.servop ~ AoS$SERfreq)
#p_global_reg_servfreq <- summary(global_reg_servfreq)

global_reg_servcum <- lm(AoS$ln.servop ~ AoS$SERcum)
#p_global_reg_servcum <- summary(global_reg_servcum)



stargazer(global_reg_tradefreq, 
          global_reg_tradecum, 
          global_reg_migfreq, 
          global_reg_migcum,
          global_reg_capfreq,
          global_reg_capcum, 
          global_reg_servfreq,
          global_reg_servcum)


## Model 2: OLS Regression for each Var with controll/dummy Var
# Trade

globald_reg_tradefreq <- lm(AoS$ln.tradeop ~ AoS$ECOfreq + 
                              AoS$crisis + 
                              AoS$globcrisis +
                              AoS$domcrisis + 
                              AoS$PolitySC2 + 
                              AoS$contiguity +
                              AoS$bordercontrols +
                              AoS$EU_memb)
#p_globald_reg_tradefreq <- summary(globald_reg_tradefreq)

globald_reg_tradecum <- lm(AoS$ln.tradeop ~ AoS$ECOcum + 
                             AoS$crisis + 
                             AoS$globcrisis +
                             AoS$domcrisis + 
                             AoS$PolitySC2 + 
                             AoS$contiguity +
                             AoS$bordercontrols +
                             AoS$EU_memb)
#p_globald_reg_tradecum <- summary(globald_reg_tradecum)

# Migration

globald_reg_migfreq <- lm(AoS$ln.migop ~ AoS$PMfreq + 
                            AoS$crisis + 
                            AoS$globcrisis +
                            AoS$domcrisis + 
                            AoS$refugeecrisis +
                            AoS$PolitySC2 + 
                            AoS$contiguity +
                            AoS$bordercontrols +
                            AoS$EU_memb)
#p_globald_reg_migfreq <- summary(globald_reg_migfreq)

globald_reg_migcum <- lm(AoS$ln.migop ~ AoS$PMcum + 
                           AoS$crisis + 
                           AoS$globcrisis +
                           AoS$domcrisis + 
                           AoS$refugeecrisis +
                           AoS$PolitySC2 + 
                           AoS$contiguity +
                           AoS$bordercontrols +
                           AoS$EU_memb)
#p_globald_reg_migcum <- summary(globald_reg_migcum)

# Capital

globald_reg_capfreq <- lm(AoS$ln.capop ~ AoS$CAPfreq + 
                            AoS$crisis + 
                            AoS$globcrisis +
                            AoS$domcrisis + 
                            AoS$bankcrisis +
                            AoS$PolitySC2 + 
                            AoS$EU_memb)
#p_global_reg_capfreq <- summary(global_reg_capfreq)

globald_reg_capcum <- lm(AoS$ln.capop ~ AoS$CAPcum + 
                           AoS$crisis + 
                           AoS$globcrisis +
                           AoS$domcrisis + 
                           AoS$bankcrisis +
                           AoS$PolitySC2 + 
                           AoS$EU_memb)
#p_global_reg_capcum <- summary(global_reg_capcum)

# Services

globald_reg_servfreq <- lm(AoS$ln.servop ~ AoS$SERfreq + 
                             AoS$crisis + 
                             AoS$globcrisis +
                             AoS$domcrisis + 
                             AoS$HId +
                             AoS$PolitySC2 + 
                             AoS$EU_memb)
#p_global_reg_servfreq <- summary(global_reg_servfreq)

globald_reg_servcum <- lm(AoS$ln.servop ~ AoS$SERcum + 
                            AoS$crisis + 
                            AoS$globcrisis +
                            AoS$domcrisis + 
                            AoS$HId +
                            AoS$PolitySC2 + 
                            AoS$EU_memb)
#p_global_reg_servcum <- summary(global_reg_servcum)

stargazer(globald_reg_tradefreq, 
          globald_reg_tradecum, 
          globald_reg_migfreq, 
          globald_reg_migcum,
          globald_reg_capfreq,
          globald_reg_capcum, 
          globald_reg_servfreq,
          globald_reg_servcum)

#####---------------------------------------------------------------------------------
#### Statewise

DEN <- subset(AoS, acra=="DEN")
EST <- subset(AoS, acra=="EST")
FIN <- subset(AoS, acra=="FIN")
GFR <- subset(AoS, acra=="GFR")
LAT <- subset(AoS, acra=="LAT")
LIT <- subset(AoS, acra=="LIT")
POL <- subset(AoS, acra=="POL")
RUS <- subset(AoS, acra=="RUS")
SWD <- subset(AoS, acra=="SWD")

## Model 1: OLS Regression for each Var
# Denmark
DEN_reg_capcum <- lm(DEN$ln.capop ~ DEN$CAPcum)
DEN_reg_capfreq <- lm(DEN$ln.capop ~ DEN$CAPfreq)
DEN_reg_migcum <- lm(DEN$ln.migop ~ DEN$PMcum)
DEN_reg_migfreq <- lm(DEN$ln.migop ~ DEN$PMfreq)
DEN_reg_servcum <- lm(DEN$ln.servop ~ DEN$SERcum)
DEN_reg_servfreq <- lm(DEN$ln.servop ~ DEN$SERfreq)
DEN_reg_tradecum <- lm(DEN$ln.tradeop ~ DEN$ECOcum)
DEN_reg_tradefreq <- lm(DEN$ln.tradeop ~ DEN$ECOfreq)

stargazer(DEN_reg_tradefreq, DEN_reg_tradecum, DEN_reg_migfreq, DEN_reg_migcum, DEN_reg_capfreq, DEN_reg_capcum, DEN_reg_servfreq, DEN_reg_servcum)

# Estonia
EST_reg_capcum <- lm(EST$ln.capop ~ EST$CAPcum)
EST_reg_capfreq <- lm(EST$ln.capop ~ EST$CAPfreq)
EST_reg_migcum <- lm(EST$ln.migop ~ EST$PMcum)
EST_reg_migfreq <- lm(EST$ln.migop ~ EST$PMfreq)
EST_reg_servcum <- lm(EST$ln.servop ~ EST$SERcum)
EST_reg_servfreq <- lm(EST$ln.servop ~ EST$SERfreq)
EST_reg_tradecum <- lm(EST$ln.tradeop ~ EST$ECOcum)
EST_reg_tradefreq <- lm(EST$ln.tradeop ~ EST$ECOfreq)

stargazer(EST_reg_tradefreq, EST_reg_tradecum, EST_reg_migfreq, EST_reg_migcum, EST_reg_capfreq, EST_reg_capcum, EST_reg_servfreq, EST_reg_servcum)

# Finaland
FIN_reg_capcum <- lm(FIN$ln.capop ~ FIN$CAPcum)
FIN_reg_capfreq <- lm(FIN$ln.capop ~ FIN$CAPfreq)
FIN_reg_migcum <- lm(FIN$ln.migop ~ FIN$PMcum)
FIN_reg_migfreq <- lm(FIN$ln.migop ~ FIN$PMfreq)
FIN_reg_servcum <- lm(FIN$ln.servop ~ FIN$SERcum)
FIN_reg_servfreq <- lm(FIN$ln.servop ~ FIN$SERfreq)
FIN_reg_tradecum <- lm(FIN$ln.tradeop ~ FIN$ECOcum)
FIN_reg_tradefreq <- lm(FIN$ln.tradeop ~ FIN$ECOfreq)

stargazer(FIN_reg_tradefreq, FIN_reg_tradecum, FIN_reg_migfreq, FIN_reg_migcum, FIN_reg_capfreq, FIN_reg_capcum, FIN_reg_servfreq, FIN_reg_servcum)

# Germany
GFR_reg_capcum <- lm(GFR$ln.capop ~ GFR$CAPcum)
GFR_reg_capfreq <- lm(GFR$ln.capop ~ GFR$CAPfreq)
GFR_reg_migcum <- lm(GFR$ln.migop ~ GFR$PMcum)
GFR_reg_migfreq <- lm(GFR$ln.migop ~ GFR$PMfreq)
GFR_reg_servcum <- lm(GFR$ln.servop ~ GFR$SERcum)
GFR_reg_servfreq <- lm(GFR$ln.servop ~ GFR$SERfreq)
GFR_reg_tradecum <- lm(GFR$ln.tradeop ~ GFR$ECOcum)
GFR_reg_tradefreq <- lm(GFR$ln.tradeop ~ GFR$ECOfreq)

stargazer(GFR_reg_tradefreq, GFR_reg_tradecum, GFR_reg_migfreq, GFR_reg_migcum, GFR_reg_capfreq, GFR_reg_capcum, GFR_reg_servfreq, GFR_reg_servcum)

# Latvia
LAT_reg_capcum <- lm(LAT$ln.capop ~ LAT$CAPcum)
LAT_reg_capfreq <- lm(LAT$ln.capop ~ LAT$CAPfreq)
LAT_reg_migcum <- lm(LAT$ln.migop ~ LAT$PMcum)
LAT_reg_migfreq <- lm(LAT$ln.migop ~ LAT$PMfreq)
LAT_reg_servcum <- lm(LAT$ln.servop ~ LAT$SERcum)
LAT_reg_servfreq <- lm(LAT$ln.servop ~ LAT$SERfreq)
LAT_reg_tradecum <- lm(LAT$ln.tradeop ~ LAT$ECOcum)
LAT_reg_tradefreq <- lm(LAT$ln.tradeop ~ LAT$ECOfreq)

stargazer(LAT_reg_tradefreq, LAT_reg_tradecum, LAT_reg_migfreq, LAT_reg_migcum, LAT_reg_capfreq, LAT_reg_capcum, LAT_reg_servfreq, LAT_reg_servcum)

# Lithuania
LIT_reg_capcum <- lm(LIT$ln.capop ~ LIT$CAPcum)
LIT_reg_capfreq <- lm(LIT$ln.capop ~ LIT$CAPfreq)
#LIT_reg_migcum <- lm(LIT$ln.migop ~ LIT$PMcum)
#LIT_reg_migfreq <- lm(LIT$ln.migop ~ LIT$PMfreq)
LIT_reg_servcum <- lm(LIT$ln.servop ~ LIT$SERcum)
LIT_reg_servfreq <- lm(LIT$ln.servop ~ LIT$SERfreq)
LIT_reg_tradecum <- lm(LIT$ln.tradeop ~ LIT$ECOcum)
LIT_reg_tradefreq <- lm(LIT$ln.tradeop ~ LIT$ECOfreq)

stargazer(LIT_reg_tradefreq, LIT_reg_tradecum, LIT_reg_capfreq, LIT_reg_capcum, LIT_reg_servfreq, LIT_reg_servcum)

# Poland
POL_reg_capcum <- lm(POL$ln.capop ~ POL$CAPcum)
POL_reg_capfreq <- lm(POL$ln.capop ~ POL$CAPfreq)
POL_reg_migcum <- lm(POL$ln.migop ~ POL$PMcum)
POL_reg_migfreq <- lm(POL$ln.migop ~ POL$PMfreq)
POL_reg_servcum <- lm(POL$ln.servop ~ POL$SERcum)
POL_reg_servfreq <- lm(POL$ln.servop ~ POL$SERfreq)
POL_reg_tradecum <- lm(POL$ln.tradeop ~ POL$ECOcum)
POL_reg_tradefreq <- lm(POL$ln.tradeop ~ POL$ECOfreq)

stargazer(POL_reg_tradefreq, POL_reg_tradecum, POL_reg_migfreq, POL_reg_migcum, POL_reg_capfreq, POL_reg_capcum, POL_reg_servfreq, POL_reg_servcum)

# Russia
RUS_reg_capcum <- lm(RUS$ln.capop ~ RUS$CAPcum)
RUS_reg_capfreq <- lm(RUS$ln.capop ~ RUS$CAPfreq)
#RUS_reg_migcum <- lm(RUS$ln.migop ~ RUS$PMcum)
#RUS_reg_migfreq <- lm(RUS$ln.migop ~ RUS$PMfreq)
RUS_reg_servcum <- lm(RUS$ln.servop ~ RUS$SERcum)
RUS_reg_servfreq <- lm(RUS$ln.servop ~ RUS$SERfreq)
RUS_reg_tradecum <- lm(RUS$ln.tradeop ~ RUS$ECOcum)
RUS_reg_tradefreq <- lm(RUS$ln.tradeop ~ RUS$ECOfreq)

stargazer(RUS_reg_tradefreq, RUS_reg_tradecum, RUS_reg_capfreq, RUS_reg_capcum, RUS_reg_servfreq, RUS_reg_servcum)

# Sweden
SWD_reg_capcum <- lm(SWD$ln.capop ~ SWD$CAPcum)
SWD_reg_capfreq <- lm(SWD$ln.capop ~ SWD$CAPfreq)
SWD_reg_migcum <- lm(SWD$ln.migop ~ SWD$PMcum)
SWD_reg_migfreq <- lm(SWD$ln.migop ~ SWD$PMfreq)
SWD_reg_servcum <- lm(SWD$ln.servop ~ SWD$SERcum)
SWD_reg_servfreq <- lm(SWD$ln.servop ~ SWD$SERfreq)
SWD_reg_tradecum <- lm(SWD$ln.tradeop ~ SWD$ECOcum)
SWD_reg_tradefreq <- lm(SWD$ln.tradeop ~ SWD$ECOfreq)

stargazer(SWD_reg_tradefreq, SWD_reg_tradecum, SWD_reg_migfreq, SWD_reg_migcum, SWD_reg_capfreq, SWD_reg_capcum, SWD_reg_servfreq, SWD_reg_servcum)

## Model 2: OLS Regression for each Var + dummy Var
# Denmark
DEN_reg_capcum <- lm(DEN$ln.capop ~ DEN$CAPcum + 
                       DEN$crisis + 
                       DEN$globcrisis +
                       DEN$domcrisis + 
                       DEN$bankcrisis +
                       DEN$PolitySC2 + 
                       DEN$EU_memb)
DEN_reg_capfreq <- lm(DEN$ln.capop ~ DEN$CAPfreq + 
                        DEN$crisis + 
                        DEN$globcrisis +
                        DEN$domcrisis + 
                        DEN$bankcrisis +
                        DEN$PolitySC2 + 
                        DEN$EU_memb)
DEN_reg_migcum <- lm(DEN$ln.migop ~ DEN$PMcum + 
                       DEN$crisis + 
                       DEN$globcrisis +
                       DEN$domcrisis + 
                       DEN$refugeecrisis +
                       DEN$PolitySC2 + 
                       DEN$contiguity +
                       DEN$bordercontrols +
                       DEN$EU_memb)
DEN_reg_migfreq <- lm(DEN$ln.migop ~ DEN$PMfreq + 
                        DEN$crisis + 
                        DEN$globcrisis +
                        DEN$domcrisis + 
                        DEN$refugeecrisis +
                        DEN$PolitySC2 + 
                        DEN$contiguity +
                        DEN$bordercontrols +
                        DEN$EU_memb)
DEN_reg_servcum <- lm(DEN$ln.servop ~ DEN$SERcum + 
                        DEN$crisis + 
                        DEN$globcrisis +
                        DEN$domcrisis + 
                        DEN$HId +
                        DEN$PolitySC2 + 
                        DEN$EU_memb)
DEN_reg_servfreq <- lm(DEN$ln.servop ~ DEN$SERfreq + 
                         DEN$crisis + 
                         DEN$globcrisis +
                         DEN$domcrisis + 
                         DEN$HId +
                         DEN$PolitySC2 + 
                         DEN$EU_memb)
DEN_reg_tradecum <- lm(DEN$ln.tradeop ~ DEN$ECOcum + 
                         DEN$crisis + 
                         DEN$globcrisis +
                         DEN$domcrisis + 
                         DEN$PolitySC2 + 
                         DEN$contiguity +
                         DEN$bordercontrols +
                         DEN$EU_memb)
DEN_reg_tradefreq <- lm(DEN$ln.tradeop ~ DEN$ECOfreq + 
                          DEN$crisis + 
                          DEN$globcrisis +
                          DEN$domcrisis + 
                          DEN$PolitySC2 + 
                          DEN$contiguity +
                          DEN$bordercontrols +
                          DEN$EU_memb)

stargazer(DEN_reg_tradefreq, DEN_reg_tradecum, DEN_reg_migfreq, DEN_reg_migcum, DEN_reg_capfreq, DEN_reg_capcum, DEN_reg_servfreq, DEN_reg_servcum)

# Estonia
EST_reg_capcum <- lm(EST$ln.capop ~ EST$CAPcum + 
                       EST$crisis + 
                       EST$globcrisis +
                       EST$domcrisis + 
                       EST$bankcrisis +
                       EST$PolitySC2 + 
                       EST$EU_memb)
EST_reg_capfreq <- lm(EST$ln.capop ~ EST$CAPfreq + 
                        EST$crisis + 
                        EST$globcrisis +
                        EST$domcrisis + 
                        EST$bankcrisis +
                        EST$PolitySC2 + 
                        EST$EU_memb)
EST_reg_migcum <- lm(EST$ln.migop ~ EST$PMcum + 
                       EST$crisis + 
                       EST$globcrisis +
                       EST$domcrisis + 
                       EST$refugeecrisis +
                       EST$PolitySC2 + 
                       EST$contiguity +
                       EST$bordercontrols +
                       EST$EU_memb)
EST_reg_migfreq <- lm(EST$ln.migop ~ EST$PMfreq + 
                        EST$crisis + 
                        EST$globcrisis +
                        EST$domcrisis + 
                        EST$refugeecrisis +
                        EST$PolitySC2 + 
                        EST$contiguity +
                        EST$bordercontrols +
                        EST$EU_memb)
EST_reg_servcum <- lm(EST$ln.servop ~ EST$SERcum + 
                        EST$crisis + 
                        EST$globcrisis +
                        EST$domcrisis + 
                        EST$HId +
                        EST$PolitySC2 + 
                        EST$EU_memb)
EST_reg_servfreq <- lm(EST$ln.servop ~ EST$SERfreq + 
                         EST$crisis + 
                         EST$globcrisis +
                         EST$domcrisis + 
                         EST$HId +
                         EST$PolitySC2 + 
                         EST$EU_memb)
EST_reg_tradecum <- lm(EST$ln.tradeop ~ EST$ECOcum + 
                         EST$crisis + 
                         EST$globcrisis +
                         EST$domcrisis + 
                         EST$PolitySC2 + 
                         EST$contiguity +
                         EST$bordercontrols +
                         EST$EU_memb)
EST_reg_tradefreq <- lm(EST$ln.tradeop ~ EST$ECOfreq + 
                          EST$crisis + 
                          EST$globcrisis +
                          EST$domcrisis + 
                          EST$PolitySC2 + 
                          EST$contiguity +
                          EST$bordercontrols +
                          EST$EU_memb)

stargazer(EST_reg_tradefreq, EST_reg_tradecum, EST_reg_migfreq, EST_reg_migcum, EST_reg_capfreq, EST_reg_capcum, EST_reg_servfreq, EST_reg_servcum)

# Finaland
FIN_reg_capcum <- lm(FIN$ln.capop ~ FIN$CAPcum + 
                       FIN$crisis + 
                       FIN$globcrisis +
                       FIN$domcrisis + 
                       FIN$bankcrisis +
                       FIN$PolitySC2 + 
                       FIN$EU_memb)
FIN_reg_capfreq <- lm(FIN$ln.capop ~ FIN$CAPfreq + 
                        FIN$crisis + 
                        FIN$globcrisis +
                        FIN$domcrisis + 
                        FIN$bankcrisis +
                        FIN$PolitySC2 + 
                        FIN$EU_memb)
FIN_reg_migcum <- lm(FIN$ln.migop ~ FIN$PMcum + 
                       FIN$crisis + 
                       FIN$globcrisis +
                       FIN$domcrisis + 
                       FIN$refugeecrisis +
                       FIN$PolitySC2 + 
                       FIN$contiguity +
                       FIN$bordercontrols +
                       FIN$EU_memb)
FIN_reg_migfreq <- lm(FIN$ln.migop ~ FIN$PMfreq + 
                        FIN$crisis + 
                        FIN$globcrisis +
                        FIN$domcrisis + 
                        FIN$refugeecrisis +
                        FIN$PolitySC2 + 
                        FIN$contiguity +
                        FIN$bordercontrols +
                        FIN$EU_memb)
FIN_reg_servcum <- lm(FIN$ln.servop ~ FIN$SERcum + 
                        FIN$crisis + 
                        FIN$globcrisis +
                        FIN$domcrisis + 
                        FIN$HId +
                        FIN$PolitySC2 + 
                        FIN$EU_memb)
FIN_reg_servfreq <- lm(FIN$ln.servop ~ FIN$SERfreq + 
                         FIN$crisis + 
                         FIN$globcrisis +
                         FIN$domcrisis + 
                         FIN$HId +
                         FIN$PolitySC2 + 
                         FIN$EU_memb)
FIN_reg_tradecum <- lm(FIN$ln.tradeop ~ FIN$ECOcum + 
                         FIN$crisis + 
                         FIN$globcrisis +
                         FIN$domcrisis + 
                         FIN$PolitySC2 + 
                         FIN$contiguity +
                         FIN$bordercontrols +
                         FIN$EU_memb)
FIN_reg_tradefreq <- lm(FIN$ln.tradeop ~ FIN$ECOfreq + 
                          FIN$crisis + 
                          FIN$globcrisis +
                          FIN$domcrisis + 
                          FIN$PolitySC2 + 
                          FIN$contiguity +
                          FIN$bordercontrols +
                          FIN$EU_memb)

stargazer(FIN_reg_tradefreq, FIN_reg_tradecum, FIN_reg_migfreq, FIN_reg_migcum, FIN_reg_capfreq, FIN_reg_capcum, FIN_reg_servfreq, FIN_reg_servcum)

# Germany
GFR_reg_capcum <- lm(GFR$ln.capop ~ GFR$CAPcum + 
                       GFR$crisis + 
                       GFR$globcrisis +
                       GFR$domcrisis + 
                       GFR$bankcrisis +
                       GFR$PolitySC2 + 
                       GFR$EU_memb)
GFR_reg_capfreq <- lm(GFR$ln.capop ~ GFR$CAPfreq + 
                        GFR$crisis + 
                        GFR$globcrisis +
                        GFR$domcrisis + 
                        GFR$bankcrisis +
                        GFR$PolitySC2 + 
                        GFR$EU_memb)
GFR_reg_migcum <- lm(GFR$ln.migop ~ GFR$PMcum + 
                       GFR$crisis + 
                       GFR$globcrisis +
                       GFR$domcrisis + 
                       GFR$refugeecrisis +
                       GFR$PolitySC2 + 
                       GFR$contiguity +
                       GFR$bordercontrols +
                       GFR$EU_memb)
GFR_reg_migfreq <- lm(GFR$ln.migop ~ GFR$PMfreq + 
                        GFR$crisis + 
                        GFR$globcrisis +
                        GFR$domcrisis + 
                        GFR$refugeecrisis +
                        GFR$PolitySC2 + 
                        GFR$contiguity +
                        GFR$bordercontrols +
                        GFR$EU_memb)
GFR_reg_servcum <- lm(GFR$ln.servop ~ GFR$SERcum + 
                        GFR$crisis + 
                        GFR$globcrisis +
                        GFR$domcrisis + 
                        GFR$HId +
                        GFR$PolitySC2 + 
                        GFR$EU_memb)
GFR_reg_servfreq <- lm(GFR$ln.servop ~ GFR$SERfreq + 
                         GFR$crisis + 
                         GFR$globcrisis +
                         GFR$domcrisis + 
                         GFR$HId +
                         GFR$PolitySC2 + 
                         GFR$EU_memb)
GFR_reg_tradecum <- lm(GFR$ln.tradeop ~ GFR$ECOcum + 
                         GFR$crisis + 
                         GFR$globcrisis +
                         GFR$domcrisis + 
                         GFR$PolitySC2 + 
                         GFR$contiguity +
                         GFR$bordercontrols +
                         GFR$EU_memb)
GFR_reg_tradefreq <- lm(GFR$ln.tradeop ~ GFR$ECOfreq + 
                          GFR$crisis + 
                          GFR$globcrisis +
                          GFR$domcrisis + 
                          GFR$PolitySC2 + 
                          GFR$contiguity +
                          GFR$bordercontrols +
                          GFR$EU_memb)

stargazer(GFR_reg_tradefreq, GFR_reg_tradecum, GFR_reg_migfreq, GFR_reg_migcum, GFR_reg_capfreq, GFR_reg_capcum, GFR_reg_servfreq, GFR_reg_servcum)

# Latvia
LAT_reg_capcum <- lm(LAT$ln.capop ~ LAT$CAPcum + 
                       LAT$crisis + 
                       LAT$globcrisis +
                       LAT$domcrisis + 
                       LAT$bankcrisis +
                       LAT$PolitySC2 + 
                       LAT$EU_memb)
LAT_reg_capfreq <- lm(LAT$ln.capop ~ LAT$CAPfreq + 
                        LAT$crisis + 
                        LAT$globcrisis +
                        LAT$domcrisis + 
                        LAT$bankcrisis +
                        LAT$PolitySC2 + 
                        LAT$EU_memb)
LAT_reg_migcum <- lm(LAT$ln.migop ~ LAT$PMcum + 
                       LAT$crisis + 
                       LAT$globcrisis +
                       LAT$domcrisis + 
                       LAT$refugeecrisis +
                       LAT$PolitySC2 + 
                       LAT$contiguity +
                       LAT$bordercontrols +
                       LAT$EU_memb)
LAT_reg_migfreq <- lm(LAT$ln.migop ~ LAT$PMfreq + 
                        LAT$crisis + 
                        LAT$globcrisis +
                        LAT$domcrisis + 
                        LAT$refugeecrisis +
                        LAT$PolitySC2 + 
                        LAT$contiguity +
                        LAT$bordercontrols +
                        LAT$EU_memb)
LAT_reg_servcum <- lm(LAT$ln.servop ~ LAT$SERcum + 
                        LAT$crisis + 
                        LAT$globcrisis +
                        LAT$domcrisis + 
                        LAT$HId +
                        LAT$PolitySC2 + 
                        LAT$EU_memb)
LAT_reg_servfreq <- lm(LAT$ln.servop ~ LAT$SERfreq + 
                         LAT$crisis + 
                         LAT$globcrisis +
                         LAT$domcrisis + 
                         LAT$HId +
                         LAT$PolitySC2 + 
                         LAT$EU_memb)
LAT_reg_tradecum <- lm(LAT$ln.tradeop ~ LAT$ECOcum + 
                         LAT$crisis + 
                         LAT$globcrisis +
                         LAT$domcrisis + 
                         LAT$PolitySC2 + 
                         LAT$contiguity +
                         LAT$bordercontrols +
                         LAT$EU_memb)
LAT_reg_tradefreq <- lm(LAT$ln.tradeop ~ LAT$ECOfreq + 
                          LAT$crisis + 
                          LAT$globcrisis +
                          LAT$domcrisis + 
                          LAT$PolitySC2 + 
                          LAT$contiguity +
                          LAT$bordercontrols +
                          LAT$EU_memb)

stargazer(LAT_reg_tradefreq, LAT_reg_tradecum, LAT_reg_migfreq, LAT_reg_migcum, LAT_reg_capfreq, LAT_reg_capcum, LAT_reg_servfreq, LAT_reg_servcum)

# Lithuania
LIT_reg_capcum <- lm(LIT$ln.capop ~ LIT$CAPcum + 
                       LIT$crisis + 
                       LIT$globcrisis +
                       LIT$domcrisis + 
                       LIT$bankcrisis +
                       LIT$PolitySC2 + 
                       LIT$EU_memb)
LIT_reg_capfreq <- lm(LIT$ln.capop ~ LIT$CAPfreq + 
                        LIT$crisis + 
                        LIT$globcrisis +
                        LIT$domcrisis + 
                        LIT$bankcrisis +
                        LIT$PolitySC2 + 
                        LIT$EU_memb)
LIT_reg_servcum <- lm(LIT$ln.servop ~ LIT$SERcum + 
                        LIT$crisis + 
                        LIT$globcrisis +
                        LIT$domcrisis + 
                        LIT$HId +
                        LIT$PolitySC2 + 
                        LIT$EU_memb)
LIT_reg_servfreq <- lm(LIT$ln.servop ~ LIT$SERfreq + 
                         LIT$crisis + 
                         LIT$globcrisis +
                         LIT$domcrisis + 
                         LIT$HId +
                         LIT$PolitySC2 + 
                         LIT$EU_memb)
LIT_reg_tradecum <- lm(LIT$ln.tradeop ~ LIT$ECOcum + 
                         LIT$crisis + 
                         LIT$globcrisis +
                         LIT$domcrisis + 
                         LIT$PolitySC2 + 
                         LIT$contiguity +
                         LIT$bordercontrols +
                         LIT$EU_memb)
LIT_reg_tradefreq <- lm(LIT$ln.tradeop ~ LIT$ECOfreq + 
                          LIT$crisis + 
                          LIT$globcrisis +
                          LIT$domcrisis + 
                          LIT$PolitySC2 + 
                          LIT$contiguity +
                          LIT$bordercontrols +
                          LIT$EU_memb)

stargazer(LIT_reg_tradefreq, LIT_reg_tradecum, LIT_reg_capfreq, LIT_reg_capcum, LIT_reg_servfreq, LIT_reg_servcum)

# Poland
POL_reg_capcum <- lm(POL$ln.capop ~ POL$CAPcum + 
                       POL$crisis + 
                       POL$globcrisis +
                       POL$domcrisis + 
                       POL$bankcrisis +
                       POL$PolitySC2 + 
                       POL$EU_memb)
POL_reg_capfreq <- lm(POL$ln.capop ~ POL$CAPfreq + 
                        POL$crisis + 
                        POL$globcrisis +
                        POL$domcrisis + 
                        POL$bankcrisis +
                        POL$PolitySC2 + 
                        POL$EU_memb)
POL_reg_migcum <- lm(POL$ln.migop ~ POL$PMcum + 
                       POL$crisis + 
                       POL$globcrisis +
                       POL$domcrisis + 
                       POL$refugeecrisis +
                       POL$PolitySC2 + 
                       POL$contiguity +
                       POL$bordercontrols +
                       POL$EU_memb)
POL_reg_migfreq <- lm(POL$ln.migop ~ POL$PMfreq + 
                        POL$crisis + 
                        POL$globcrisis +
                        POL$domcrisis + 
                        POL$refugeecrisis +
                        POL$PolitySC2 + 
                        POL$contiguity +
                        POL$bordercontrols +
                        POL$EU_memb)
POL_reg_servcum <- lm(POL$ln.servop ~ POL$SERcum + 
                        POL$crisis + 
                        POL$globcrisis +
                        POL$domcrisis + 
                        POL$HId +
                        POL$PolitySC2 + 
                        POL$EU_memb)
POL_reg_servfreq <- lm(POL$ln.servop ~ POL$SERfreq + 
                         POL$crisis + 
                         POL$globcrisis +
                         POL$domcrisis + 
                         POL$HId +
                         POL$PolitySC2 + 
                         POL$EU_memb)
POL_reg_tradecum <- lm(POL$ln.tradeop ~ POL$ECOcum + 
                         POL$crisis + 
                         POL$globcrisis +
                         POL$domcrisis + 
                         POL$PolitySC2 + 
                         POL$contiguity +
                         POL$bordercontrols +
                         POL$EU_memb)
POL_reg_tradefreq <- lm(POL$ln.tradeop ~ POL$ECOfreq + 
                          POL$crisis + 
                          POL$globcrisis +
                          POL$domcrisis + 
                          POL$PolitySC2 + 
                          POL$contiguity +
                          POL$bordercontrols +
                          POL$EU_memb)

stargazer(POL_reg_tradefreq, POL_reg_tradecum, POL_reg_migfreq, POL_reg_migcum, POL_reg_capfreq, POL_reg_capcum, POL_reg_servfreq, POL_reg_servcum)


# Sweden
SWD_reg_capcum <- lm(SWD$ln.capop ~ SWD$CAPcum + 
                       SWD$crisis + 
                       SWD$globcrisis +
                       SWD$domcrisis + 
                       SWD$bankcrisis +
                       SWD$PolitySC2 + 
                       SWD$EU_memb)
SWD_reg_capfreq <- lm(SWD$ln.capop ~ SWD$CAPfreq + 
                        SWD$crisis + 
                        SWD$globcrisis +
                        SWD$domcrisis + 
                        SWD$bankcrisis +
                        SWD$PolitySC2 + 
                        SWD$EU_memb)
SWD_reg_migcum <- lm(SWD$ln.migop ~ SWD$PMcum + 
                       SWD$crisis + 
                       SWD$globcrisis +
                       SWD$domcrisis + 
                       SWD$refugeecrisis +
                       SWD$PolitySC2 + 
                       SWD$contiguity +
                       SWD$bordercontrols +
                       SWD$EU_memb)
SWD_reg_migfreq <- lm(SWD$ln.migop ~ SWD$PMfreq + 
                        SWD$crisis + 
                        SWD$globcrisis +
                        SWD$domcrisis + 
                        SWD$refugeecrisis +
                        SWD$PolitySC2 + 
                        SWD$contiguity +
                        SWD$bordercontrols +
                        SWD$EU_memb)
SWD_reg_servcum <- lm(SWD$ln.servop ~ SWD$SERcum + 
                        SWD$crisis + 
                        SWD$globcrisis +
                        SWD$domcrisis + 
                        SWD$HId +
                        SWD$PolitySC2 + 
                        SWD$EU_memb)
SWD_reg_servfreq <- lm(SWD$ln.servop ~ SWD$SERfreq + 
                         SWD$crisis + 
                         SWD$globcrisis +
                         SWD$domcrisis + 
                         SWD$HId +
                         SWD$PolitySC2 + 
                         SWD$EU_memb)
SWD_reg_tradecum <- lm(SWD$ln.tradeop ~ SWD$ECOcum + 
                         SWD$crisis + 
                         SWD$globcrisis +
                         SWD$domcrisis + 
                         SWD$PolitySC2 + 
                         SWD$contiguity +
                         SWD$bordercontrols +
                         SWD$EU_memb)
SWD_reg_tradefreq <- lm(SWD$ln.tradeop ~ SWD$ECOfreq + 
                          SWD$crisis + 
                          SWD$globcrisis +
                          SWD$domcrisis + 
                          SWD$PolitySC2 + 
                          SWD$contiguity +
                          SWD$bordercontrols +
                          SWD$EU_memb)

stargazer(SWD_reg_tradefreq, SWD_reg_tradecum, SWD_reg_migfreq, SWD_reg_migcum, SWD_reg_capfreq, SWD_reg_capcum, SWD_reg_servfreq, SWD_reg_servcum)

#-----Additional dyadic analysis, not included in the Working Paper
ID = rep(1:72, each = 16)
length(Vec)

AoS2 = cbind(ID, AoS)

for (i in 1:72){
  temp_variable <- assign(paste("ID_",i, sep = ""),subset(AoS2, AoS2$ID == i))
  fit <- lm(temp_variable$ln.tradeop ~ temp_variable$ECOcum)
  reg <- summary(fit)$coefficients
  Significance <- ifelse(reg[,4] < .001, "***", 
                         ifelse(reg[ ,4] < .01, " **",
                                ifelse(reg[ ,4] < .05, " *",
                                       round(reg[ ,4], 3))))
  if (i == 1) Data <- cbind("ID" = i, "Acra" = as.character(temp_variable$acra), "Acrb" = as.character(temp_variable$acrb), round(summary(fit)$coefficients,4), "Sig" = Significance, "R Squared" = round(summary(fit)$r.squared,4))
  if (i > 1) Data <- rbind(Data, cbind("ID" = i, "Acra" = as.character(temp_variable$acra), "Acrb" = as.character(temp_variable$acrb), round(summary(fit)$coefficients,5), "Sig" = Significance, "R Squared" = round(summary(fit)$r.squared,4)))
  
}