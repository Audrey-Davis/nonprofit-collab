library(tidyverse)
library(dplyr)
library(ggplot2)
library(survey)
library(shiny)
library(stats)

#####Census region Rcv
Loc_Reg_Mosaic <- xtabs(year4wt ~ CensusRegion9 + FndRaise_LocGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Loc_Reg_Mosaic, shade = T)

State_Reg_Mosaic <- xtabs(year4wt ~ CensusRegion9 + FndRaise_StateGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(State_Reg_Mosaic, shade = T)

Fed_Reg_Mosaic <- xtabs(year4wt ~ CensusRegion9 + FndRaise_FedGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Fed_Reg_Mosaic, shade = T)

Loc_Cntrct_Reg_Mosaic <- xtabs(year4wt ~ CensusRegion9 + FndRaise_LocGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Loc_Cntrct_Reg_Mosaic, shade = T)

State_Cntrct_Reg_Mosaic <- xtabs(year4wt ~ CensusRegion9 + FndRaise_StateGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(State_Cntrct_Reg_Mosaic, shade = T)

Fed_Cntrct_Reg_Mosaic <- xtabs(year4wt ~ CensusRegion9 + FndRaise_FedGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Fed_Cntrct_Reg_Mosaic, shade = T)

Priv_Grnt_Reg_Mosaic <- xtabs(year4wt ~ CensusRegion9 + FndRaise_PFGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Priv_Grnt_Reg_Mosaic, shade = T)

Community_Grnt_Reg_Mosaic <- xtabs(year4wt ~ CensusRegion9 + FndRaise_CFGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Community_Grnt_Reg_Mosaic, shade = T)

Donor_Funds_Reg_Mosaic <- xtabs(year4wt ~ CensusRegion9 + FndRaise_DAF_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Donor_Funds_Reg_Mosaic, shade = T)

Corp_Funds_Reg_Mosaic <- xtabs(year4wt ~ CensusRegion9 + FndRaise_Corp_Found_Grnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Corp_Funds_Reg_Mosaic, shade = T)

UW_Funds_Reg_Mosaic <- xtabs(year4wt ~ CensusRegion9 + FndRaise_UntdWy_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(UW_Funds_Reg_Mosaic, shade = T)

CFC_Funds_Reg_Mosaic <- xtabs(year4wt ~ CensusRegion9 + FndRaise_CombFedCmpgn_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CFC_Funds_Reg_Mosaic, shade = T)

Other_Funds_Reg_Mosaic <- xtabs(year4wt ~ CensusRegion9 + FndRaise_OthrGvngPrgrm_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Other_Funds_Reg_Mosaic, shade = T)

#####Sector Rcv
Loc_Sec_Mosaic <- xtabs(year4wt ~ ntmaj12 + FndRaise_LocGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Loc_Sec_Mosaic, shade = T)

State_Sec_Mosaic <- xtabs(year4wt ~ ntmaj12 + FndRaise_StateGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(State_Sec_Mosaic, shade = T)

Fed_Sec_Mosaic <- xtabs(year4wt ~ ntmaj12 + FndRaise_FedGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Fed_Sec_Mosaic, shade = T)

Loc_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ ntmaj12 + FndRaise_LocGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Loc_Cntrct_Sec_Mosaic, shade = T)

State_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ ntmaj12 + FndRaise_StateGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(State_Cntrct_Sec_Mosaic, shade = T)

Fed_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ ntmaj12 + FndRaise_FedGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Fed_Cntrct_Sec_Mosaic, shade = T)

Priv_Grnt_Sec_Mosaic <- xtabs(year4wt ~ ntmaj12 + FndRaise_PFGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Priv_Grnt_Sec_Mosaic, shade = T)

Community_Grnt_Sec_Mosaic <- xtabs(year4wt ~ ntmaj12 + FndRaise_CFGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Community_Grnt_Sec_Mosaic, shade = T)

Donor_Funds_Sec_Mosaic <- xtabs(year4wt ~ ntmaj12 + FndRaise_DAF_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Donor_Funds_Sec_Mosaic, shade = T)

Corp_Funds_Sec_Mosaic <- xtabs(year4wt ~ ntmaj12 + FndRaise_Corp_Found_Grnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Corp_Funds_Sec_Mosaic, shade = T)

UW_Funds_Sec_Mosaic <- xtabs(year4wt ~ ntmaj12 + FndRaise_UntdWy_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(UW_Funds_Sec_Mosaic, shade = T)

CFC_Funds_Sec_Mosaic <- xtabs(year4wt ~ ntmaj12 + FndRaise_CombFedCmpgn_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CFC_Funds_Sec_Mosaic, shade = T)

Other_Funds_Sec_Mosaic <- xtabs(year4wt ~ ntmaj12 + FndRaise_OthrGvngPrgrm_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Other_Funds_Sec_Mosaic, shade = T)

##### not Helpful
##### Ceo Race Rcv

CeoRace_Loc_Sec_Mosaic <- xtabs(year4wt ~ CEOrace + FndRaise_LocGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoRace_Loc_Sec_Mosaic, shade = T)

CeoRace_State_Sec_Mosaic <- xtabs(year4wt ~ CEOrace + FndRaise_StateGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoRace_State_Sec_Mosaic, shade = T)

CeoRace_Fed_Sec_Mosaic <- xtabs(year4wt ~ CEOrace + FndRaise_FedGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoRace_Fed_Sec_Mosaic, shade = T)

CeoRace_Loc_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ CEOrace + FndRaise_LocGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoRace_Loc_Cntrct_Sec_Mosaic, shade = T)

CeoRace_State_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ CEOrace + FndRaise_StateGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoRace_State_Cntrct_Sec_Mosaic, shade = T)

CeoRace_Fed_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ CEOrace + FndRaise_FedGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoRace_Fed_Cntrct_Sec_Mosaic, shade = T)

CeoRace_Priv_Grnt_Sec_Mosaic <- xtabs(year4wt ~ CEOrace + FndRaise_PFGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoRace_Priv_Grnt_Sec_Mosaic, shade = T)

CeoRace_Community_Grnt_Sec_Mosaic <- xtabs(year4wt ~ CEOrace + FndRaise_CFGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoRace_Community_Grnt_Sec_Mosaic, shade = T)

CeoRace_Donor_Funds_Sec_Mosaic <- xtabs(year4wt ~ CEOrace + FndRaise_DAF_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoRace_Donor_Funds_Sec_Mosaic, shade = T)

CeoRace_Corp_Funds_Sec_Mosaic <- xtabs(year4wt ~ CEOrace + FndRaise_Corp_Found_Grnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoRace_Corp_Funds_Sec_Mosaic, shade = T)

CeoRace_UW_Funds_Sec_Mosaic <- xtabs(year4wt ~ CEOrace + FndRaise_UntdWy_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoRace_UW_Funds_Sec_Mosaic, shade = T)

CeoRace_CFC_Funds_Sec_Mosaic <- xtabs(year4wt ~ CEOrace + FndRaise_CombFedCmpgn_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoRace_CFC_Funds_Sec_Mosaic, shade = T)

CeoRace_Other_Funds_Sec_Mosaic <- xtabs(year4wt ~ CEOrace + FndRaise_OthrGvngPrgrm_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoRace_Other_Funds_Sec_Mosaic, shade = T)

#####not helpful 
##### Chair Race Rcv

ChairRace_Loc_Sec_Mosaic <- xtabs(year4wt ~ BChairrace + FndRaise_LocGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairRace_Loc_Sec_Mosaic, shade = T)

ChairRace_State_Sec_Mosaic <- xtabs(year4wt ~ BChairrace + FndRaise_StateGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairRace_State_Sec_Mosaic, shade = T)

ChairRace_Fed_Sec_Mosaic <- xtabs(year4wt ~ BChairrace + FndRaise_FedGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairRace_Fed_Sec_Mosaic, shade = T)

ChairRace_Loc_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ BChairrace + FndRaise_LocGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairRace_Loc_Cntrct_Sec_Mosaic, shade = T)

ChairRace_State_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ BChairrace + FndRaise_StateGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairRace_Loc_Cntrct_Sec_Mosaic, shade = T)

ChairRace_Fed_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ BChairrace + FndRaise_FedGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairRace_Fed_Cntrct_Sec_Mosaic, shade = T)

ChairRace_Priv_Grnt_Sec_Mosaic <- xtabs(year4wt ~ BChairrace + FndRaise_PFGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairRace_Priv_Grnt_Sec_Mosaic, shade = T)

ChairRace_Community_Grnt_Sec_Mosaic <- xtabs(year4wt ~ BChairrace + FndRaise_CFGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairRace_Community_Grnt_Sec_Mosaic, shade = T)

ChairRace_Donor_Funds_Sec_Mosaic <- xtabs(year4wt ~ BChairrace + FndRaise_DAF_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairRace_Donor_Funds_Sec_Mosaic, shade = T)

ChairRace_Corp_Funds_Sec_Mosaic <- xtabs(year4wt ~ BChairrace + FndRaise_Corp_Found_Grnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairRace_Corp_Funds_Sec_Mosaic, shade = T)

ChairRace_UW_Funds_Sec_Mosaic <- xtabs(year4wt ~ BChairrace + FndRaise_UntdWy_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairRace_UW_Funds_Sec_Mosaic, shade = T)

ChairRace_CFC_Funds_Sec_Mosaic <- xtabs(year4wt ~ BChairrace + FndRaise_CombFedCmpgn_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairRace_CFC_Funds_Sec_Mosaic, shade = T)

ChairRace_Other_Funds_Sec_Mosaic <- xtabs(year4wt ~ BChairrace + FndRaise_OthrGvngPrgrm_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairRace_Other_Funds_Sec_Mosaic, shade = T)

##### Helpful
##### Ceo Age Rcv
CeoAge_Loc_Sec_Mosaic <- xtabs(year4wt ~ Dem_CEO_Age + FndRaise_LocGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoAge_Loc_Sec_Mosaic, shade = T)

CeoAge_State_Sec_Mosaic <- xtabs(year4wt ~ Dem_CEO_Age + FndRaise_StateGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoAge_State_Sec_Mosaic, shade = T)

CeoAge_Fed_Sec_Mosaic <- xtabs(year4wt ~ Dem_CEO_Age + FndRaise_FedGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoAge_Fed_Sec_Mosaic, shade = T)

CeoAge_Loc_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ Dem_CEO_Age + FndRaise_LocGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoAge_Loc_Cntrct_Sec_Mosaic, shade = T)

CeoAge_State_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ Dem_CEO_Age + FndRaise_StateGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoAge_State_Cntrct_Sec_Mosaic, shade = T)

CeoAge_Fed_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ Dem_CEO_Age + FndRaise_FedGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoAge_Fed_Cntrct_Sec_Mosaic, shade = T)

CeoAge_Priv_Grnt_Sec_Mosaic <- xtabs(year4wt ~ Dem_CEO_Age + FndRaise_PFGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoAge_Priv_Grnt_Sec_Mosaic, shade = T)

CeoAge_Community_Grnt_Sec_Mosaic <- xtabs(year4wt ~ Dem_CEO_Age + FndRaise_CFGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoAge_Community_Grnt_Sec_Mosaic, shade = T)

CeoAge_Donor_Funds_Sec_Mosaic <- xtabs(year4wt ~ Dem_CEO_Age + FndRaise_DAF_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoAge_Donor_Funds_Sec_Mosaic, shade = T)

CeoAge_Corp_Funds_Sec_Mosaic <- xtabs(year4wt ~ Dem_CEO_Age + FndRaise_Corp_Found_Grnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoAge_Corp_Funds_Sec_Mosaic, shade = T)

CeoAge_UW_Funds_Sec_Mosaic <- xtabs(year4wt ~ Dem_CEO_Age + FndRaise_UntdWy_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoAge_UW_Funds_Sec_Mosaic, shade = T)

CeoAge_CFC_Funds_Sec_Mosaic <- xtabs(year4wt ~ Dem_CEO_Age + FndRaise_CombFedCmpgn_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoAge_CFC_Funds_Sec_Mosaic, shade = T)

CeoAge_Other_Funds_Sec_Mosaic <- xtabs(year4wt ~ Dem_CEO_Age + FndRaise_OthrGvngPrgrm_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(CeoAge_Other_Funds_Sec_Mosaic, shade = T)

##### Helpful
##### Chair Age
ChairAge_Loc_Sec_Mosaic <- xtabs(year4wt ~ Dem_BChair_Age + FndRaise_LocGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairAge_Loc_Sec_Mosaic, shade = T)

ChairAge_State_Sec_Mosaic <- xtabs(year4wt ~ Dem_BChair_Age + FndRaise_StateGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairAge_State_Sec_Mosaic, shade = T)

ChairAge_Fed_Sec_Mosaic <- xtabs(year4wt ~ Dem_BChair_Age + FndRaise_FedGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairAge_Fed_Sec_Mosaic, shade = T)

ChairAge_Loc_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ Dem_BChair_Age + FndRaise_LocGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairAge_Loc_Cntrct_Sec_Mosaic, shade = T)

ChairAge_State_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ Dem_BChair_Age + FndRaise_StateGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairAge_State_Cntrct_Sec_Mosaic, shade = T)

ChairAge_Fed_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ Dem_BChair_Age + FndRaise_FedGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairAge_Fed_Cntrct_Sec_Mosaic, shade = T)

ChairAge_Priv_Grnt_Sec_Mosaic <- xtabs(year4wt ~ Dem_BChair_Age + FndRaise_PFGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairAge_Priv_Grnt_Sec_Mosaic, shade = T)

ChairAge_Community_Grnt_Sec_Mosaic <- xtabs(year4wt ~ Dem_BChair_Age + FndRaise_CFGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairAge_Community_Grnt_Sec_Mosaic, shade = T)

ChairAge_Donor_Funds_Sec_Mosaic <- xtabs(year4wt ~ Dem_BChair_Age + FndRaise_DAF_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairAge_Donor_Funds_Sec_Mosaic, shade = T)

ChairAge_Corp_Funds_Sec_Mosaic <- xtabs(year4wt ~ Dem_BChair_Age + FndRaise_Corp_Found_Grnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairAge_Corp_Funds_Sec_Mosaic, shade = T)

ChairAge_UW_Funds_Sec_Mosaic <- xtabs(year4wt ~ Dem_BChair_Age + FndRaise_UntdWy_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairAge_UW_Funds_Sec_Mosaic, shade = T)

ChairAge_CFC_Funds_Sec_Mosaic <- xtabs(year4wt ~ Dem_BChair_Age + FndRaise_CombFedCmpgn_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairAge_CFC_Funds_Sec_Mosaic, shade = T)

ChairAge_Other_Funds_Sec_Mosaic <- xtabs(year4wt ~ Dem_BChair_Age + FndRaise_OthrGvngPrgrm_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(ChairAge_Other_Funds_Sec_Mosaic, shade = T)

##### Research is helpful
##### External Affairs do they Conduct and publicize research to the media, the public, or policymakers  
Research_Loc_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_Media + FndRaise_LocGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Research_Loc_Sec_Mosaic, shade = T)

Research_State_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_Media + FndRaise_StateGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Research_State_Sec_Mosaic, shade = T)

Research_Fed_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_Media + FndRaise_FedGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Research_Fed_Sec_Mosaic, shade = T)

Research_Loc_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_Media + FndRaise_LocGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Research_Loc_Cntrct_Sec_Mosaic, shade = T)

Research_State_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_Media + FndRaise_StateGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Research_State_Cntrct_Sec_Mosaic, shade = T)

Research_Fed_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_Media + FndRaise_FedGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Research_Fed_Cntrct_Sec_Mosaic, shade = T)

Research_Priv_Grnt_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_Media + FndRaise_PFGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Research_Priv_Grnt_Sec_Mosaic, shade = T)

Research_Community_Grnt_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_Media + FndRaise_CFGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Research_Community_Grnt_Sec_Mosaic, shade = T)

Research_Donor_Funds_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_Media + FndRaise_DAF_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Research_Donor_Funds_Sec_Mosaic, shade = T)

Research_Corp_Funds_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_Media + FndRaise_Corp_Found_Grnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Research_Corp_Funds_Sec_Mosaic, shade = T)

Research_UW_Funds_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_Media + FndRaise_UntdWy_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Research_UW_Funds_Sec_Mosaic, shade = T)

Research_CFC_Funds_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_Media + FndRaise_CombFedCmpgn_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Research_CFC_Funds_Sec_Mosaic, shade = T)

Research_Other_Funds_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_Media + FndRaise_OthrGvngPrgrm_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Research_Other_Funds_Sec_Mosaic, shade = T)


#####Meet is helpful
##### ExtAffairs_MeetWork meet with government officials about the work you're doing
Meet_Loc_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_MeetWork + FndRaise_LocGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Meet_Loc_Sec_Mosaic, shade = T)

Meet_State_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_MeetWork + FndRaise_StateGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Meet_State_Sec_Mosaic, shade = T)

Meet_Fed_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_MeetWork + FndRaise_FedGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Meet_Fed_Sec_Mosaic, shade = T)

Meet_Loc_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_MeetWork + FndRaise_LocGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Meet_Loc_Cntrct_Sec_Mosaic, shade = T)

Meet_State_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_MeetWork + FndRaise_StateGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Meet_State_Cntrct_Sec_Mosaic, shade = T)

Meet_Fed_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_MeetWork + FndRaise_FedGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Meet_Fed_Cntrct_Sec_Mosaic, shade = T)

Meet_Priv_Grnt_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_MeetWork + FndRaise_PFGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Meet_Priv_Grnt_Sec_Mosaic, shade = T)

Meet_Community_Grnt_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_MeetWork + FndRaise_CFGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Meet_Community_Grnt_Sec_Mosaic, shade = T)

Meet_Donor_Funds_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_MeetWork + FndRaise_DAF_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Meet_Donor_Funds_Sec_Mosaic, shade = T)

Meet_Corp_Funds_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_MeetWork + FndRaise_Corp_Found_Grnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Meet_Corp_Funds_Sec_Mosaic, shade = T)

Meet_UW_Funds_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_MeetWork + FndRaise_UntdWy_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Meet_UW_Funds_Sec_Mosaic, shade = T)

Meet_CFC_Funds_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_MeetWork + FndRaise_CombFedCmpgn_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Meet_CFC_Funds_Sec_Mosaic, shade = T)

Meet_Other_Funds_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_MeetWork + FndRaise_OthrGvngPrgrm_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Meet_Other_Funds_Sec_Mosaic, shade = T)


#####advocacy is not helpful
##### ExtAffairs_OrganizeAdvocacy Organize marches, rallies, protests, boycotts or demonstrations without a focus on legislation
Advocacy_Loc_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_OrganizeAdvocacy + FndRaise_LocGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Advocacy_Loc_Sec_Mosaic, shade = T)

Advocacy_State_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_OrganizeAdvocacy + FndRaise_StateGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Advocacy_State_Sec_Mosaic, shade = T)

Advocacy_Fed_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_OrganizeAdvocacy + FndRaise_FedGvtGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Advocacy_Fed_Sec_Mosaic, shade = T)

Advocacy_Loc_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_OrganizeAdvocacy + FndRaise_LocGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Advocacy_Loc_Cntrct_Sec_Mosaic, shade = T)

Advocacy_State_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_OrganizeAdvocacy + FndRaise_StateGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Advocacy_State_Cntrct_Sec_Mosaic, shade = T)

Advocacy_Fed_Cntrct_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_OrganizeAdvocacy + FndRaise_FedGvtCntrct_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Advocacy_Fed_Cntrct_Sec_Mosaic, shade = T)

Advocacy_Priv_Grnt_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_OrganizeAdvocacy + FndRaise_PFGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Advocacy_Priv_Grnt_Sec_Mosaic, shade = T)

Advocacy_Community_Grnt_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_OrganizeAdvocacy + FndRaise_CFGrnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Advocacy_Community_Grnt_Sec_Mosaic, shade = T)

Advocacy_Donor_Funds_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_OrganizeAdvocacy + FndRaise_DAF_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Advocacy_Donor_Funds_Sec_Mosaic, shade = T)

Advocacy_Corp_Funds_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_OrganizeAdvocacy + FndRaise_Corp_Found_Grnt_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Advocacy_Corp_Funds_Sec_Mosaic, shade = T)

Advocacy_UW_Funds_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_OrganizeAdvocacy + FndRaise_UntdWy_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Advocacy_UW_Funds_Sec_Mosaic, shade = T)

Advocacy_CFC_Funds_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_OrganizeAdvocacy + FndRaise_CombFedCmpgn_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Advocacy_CFC_Funds_Sec_Mosaic, shade = T)

Advocacy_Other_Funds_Sec_Mosaic <- xtabs(year4wt ~ ExtAffairs_OrganizeAdvocacy + FndRaise_OthrGvngPrgrm_Rcv, data = YEAR_04_DATA_PUF)

mosaicplot(Advocacy_Other_Funds_Sec_Mosaic, shade = T)

