#for data manipulation functions
library(tidyverse)

#sets working directory to this scritp's location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read in ctd table from Oceanography Database
#takes primary sigma theta, if NA checks secondary sigma theta
#rounds depths to nearest integer so filter command works further down the line (note, these should be fixed in database, this is a workaround)
ctd <- read.csv("CTD.csv") %>%
  mutate(SIGMA_THETA=ifelse(is.na(PRIMARY_SIGMA_THETA),SECONDARY_SIGMA_THETA,PRIMARY_SIGMA_THETA)) %>%
  mutate(DEPTH=round(DEPTH,digits=0)) %>%
  distinct(STATION_ID,EVENT_CODE,DEPTH,.keep_all=TRUE)

#plot profiles of each CTD cast if you want to inspect
ctd_sub <- ctd %>%
  mutate(survey=substr(STATION_ID,1,6),Station=substr(STATION_ID,9,12)) 
ggplot() +
  #set survey here (year+vessel)
  geom_line(data=subset(ctd_sub,survey=="202203"),aes(x=PRIMARY_TEMPERATURE,y=DEPTH*-1)) +
  facet_wrap(~Station,scales="free")

#create data frame with CTD Max Depth
ctd_sum <- ctd %>%
  group_by(STATION_ID,EVENT_CODE) %>%
  summarise(CTD_MAX_DEPTH = max(DEPTH))

#create data frame with all unnecessary sigma_theta values removed (removes depths less than 4, any sigma thetas over 50,
#any sigma < 0 (note that technically sigmatheta can be mildly negative but I'm assuming these are pumps turning on or actual bad data)
# or any NA sigmatheta.
ctd1 <- ctd %>% 
  arrange(STATION_ID,EVENT_CODE,DEPTH) %>% 
  filter(DEPTH>4 & SIGMA_THETA<50 & !is.na(SIGMA_THETA) & SIGMA_THETA>0)

#remove any stations where the Min Depth record is greater than 5
ctd2 <- ctd1 %>%
  group_by(STATION_ID,EVENT_CODE) %>%
  filter(!any(min(DEPTH) > 5)) 
  

#arrange data frame from above by Depth
mld <- arrange(ctd2,DEPTH) %>% 
  #groups by Station ID
  dplyr::group_by(STATION_ID,EVENT_CODE) %>% 
  #filters on SigmaTheta and returns all rows where the difference in sigma theta from the sigma theta at 5m is <=0.1
  filter(SIGMA_THETA>=(SIGMA_THETA[DEPTH==5]+0.1)) %>%
  summarise(MLD=min(DEPTH))


#indicate what fields weren't generated due to no sigmatheta at 5m
no_5m <- ctd %>%
  arrange(STATION_ID,DEPTH) %>%
  filter(DEPTH==5 & is.na(SIGMA_THETA)) %>%
  dplyr::select(c("STATION_ID","EVENT_CODE")) %>%
  mutate(REASON="no 5m SigmaTheta")


#this code isn't good but the idea is that it should catch the vast majority of well mixed stations.  someone smarter than me can probably do this better.
well_mixed <- ctd1 %>%
  arrange(STATION_ID,DEPTH) %>%
  group_by(STATION_ID,EVENT_CODE) %>%
  filter(DEPTH == 5 | DEPTH == max(DEPTH)) %>%
  arrange(STATION_ID,DEPTH) %>%
  summarise(REASON=ifelse(SIGMA_THETA[2]-SIGMA_THETA[1]>0.1,"calculated mld",ifelse(SIGMA_THETA[2]-SIGMA_THETA[1]<(-0.1),"bad data","well mixed")))


#join in MLD explanationS
df <- left_join(ctd_sum,mld,by=c("STATION_ID","EVENT_CODE"))
df1 <- left_join(df,no_5m,by=c("STATION_ID","EVENT_CODE"))
df2 <- left_join(df1,well_mixed,by=c("STATION_ID","EVENT_CODE"))

#single explanation field
df2$REASON <- ifelse(is.na(df2$REASON.x),df2$REASON.y,df2$REASON.x)
#fix well mixed values with MLD present
df2$CTD_MLD_METADATA <- ifelse(!is.na(df2$MLD),"calculated mld",df2$REASON)
#final MLD data frame
mld_final <- subset(df2,select=c(STATION_ID,EVENT_CODE,CTD_MAX_DEPTH,MLD,CTD_MLD_METADATA))

#read in CAT file
cat <- read.csv("CAT.csv") %>%
  mutate(DEPTH=round(DEPTH,digits=0)) %>%
  distinct(STATION_ID,EVENT_CODE,DEPTH,.keep_all=TRUE)

cat_sub <- cat %>%
  mutate(survey=substr(STATION_ID,1,6),Station=substr(STATION_ID,9,12)) 
ggplot() +
  #set survey here (year+vessel)
  geom_line(data=subset(cat_sub,survey=="202203"),aes(x=TEMPERATURE,y=DEPTH*-1)) +
  facet_wrap(~Station,scales="free")

cat_sum <- cat%>%
  group_by(STATION_ID,EVENT_CODE) %>%
  summarise(CAT_MAX_DEPTH = max(DEPTH))

#create data frame with all unnecessary sigma_theta values removed (removes depths less than 4, any sigma thetas over 50,
#any sigma < 0 (note that technically sigmatheta can be mildly negative but I'm assuming these are pumps turning on or actual bad data)
# or any NA sigmatheta.
cat1 <- cat %>% 
  arrange(STATION_ID,EVENT_CODE,DEPTH) %>% 
  filter(DEPTH>4 & SIGMA_THETA<50 & !is.na(SIGMA_THETA) & SIGMA_THETA>0)

#remove any stations where the Min Depth record is greater than 5
cat2 <- cat1 %>%
  group_by(STATION_ID,EVENT_CODE) %>%
  filter(!any(min(DEPTH) > 5)) 


#arrange data frame from above by Depth
cat_mld <- arrange(cat2,DEPTH) %>% 
  #groups by Station ID
  dplyr::group_by(STATION_ID,EVENT_CODE) %>% 
  #filters on SigmaTheta and returns all rows where the difference in sigma theta from the sigma theta at 5m is <=0.1
  filter(SIGMA_THETA>=(SIGMA_THETA[DEPTH==5]+0.1)) %>%
  summarise(MLD=min(DEPTH))


cat_no_5m <- cat %>%
  arrange(STATION_ID,DEPTH) %>%
  filter(DEPTH==5 & is.na(SIGMA_THETA)) %>%
  dplyr::select(c("STATION_ID","EVENT_CODE")) %>%
  mutate(REASON="no 5m SigmaTheta")


#this code isn't good but the idea is that it should catch the vast majority of well mixed stations.  someone smarter than me can probably do this better.
cat_well_mixed <- cat1 %>%
  arrange(STATION_ID,DEPTH) %>%
  group_by(STATION_ID,EVENT_CODE) %>%
  filter(DEPTH == 5 | DEPTH == max(DEPTH)) %>%
  arrange(STATION_ID,DEPTH) %>%
  summarise(REASON=ifelse(SIGMA_THETA[2]-SIGMA_THETA[1]>0.1,"calculated mld",ifelse(SIGMA_THETA[2]-SIGMA_THETA[1]<(-0.1),"bad data","well mixed")))


#join in MLD explanationS
catdf <- left_join(cat_sum,cat_mld,by=c("STATION_ID","EVENT_CODE"))
catdf1 <- left_join(catdf,cat_no_5m,by=c("STATION_ID","EVENT_CODE"))
catdf2 <- left_join(catdf1,cat_well_mixed,by=c("STATION_ID","EVENT_CODE"))

#single explanation field
catdf2$REASON <- ifelse(is.na(catdf2$REASON.x),catdf2$REASON.y,catdf2$REASON.x)
#fix well mixed values with MLD present
catdf2$CAT_MLD_METADATA <- ifelse(!is.na(catdf2$MLD),"calculated mld",catdf2$REASON)
#final MLD data frame
cat_mld_final <- subset(catdf2,select=c(STATION_ID,EVENT_CODE,CAT_MAX_DEPTH,MLD,CAT_MLD_METADATA))

mld_full <- full_join(mld_final,cat_mld_final,by=c("STATION_ID","EVENT_CODE"))

write.csv(mld_full,"Mixed Layer Depth Database Import.csv")
