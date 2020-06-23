library(estatapi)
#https://cran.r-project.org/web/packages/estatapi/README.html
library(tidyverse)
library(memisc)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(gdata)
old = theme_set(theme_gray(base_family="HiraKakuProN-W3"))
old = theme_set(theme_bw(base_family="HiraKakuProN-W3"))

######################################################################
# Change Working Directory
######################################################################
setwd("/Users/fumiyau/Dropbox (Princeton)/09.1.DropoutCollegeMajor/") 

######################################################################
# Read data: Census mid year population
######################################################################

#---------------------------------------#
# Dropout rates among 2008 cohort
#---------------------------------------#
# 0000050000 その他（編入学者）があるので編入学者は入っていないはず

#関係学科別入学者数 2008
datalist <- estat_getStatsList(appId = appid_fu, searchWord = "学校基本調査 平成２０年度 高等教育機関 大学・大学院")
meta <- estat_getMetaInfo(appId = appid_fu, statsDataId = "0003017068")

enter2008x <- estat_getStatsData(
  appId = appid_fu,
  statsDataId = "0003017068",
  cdCat04 = "0000000030") %>%  #就業年限4年に限る
  filter(cat02_code =="0000000010") %>% 
  filter(cat03_code == "00000") %>% 
  mutate(lab1 = str_sub(cat01_code,-5,-3),
         lab2 = str_sub(cat01_code,-2,-1)) %>% 
  filter(lab2 == "00") %>% 
  dplyr::select(majorx=`関係学科別（大）`,lab1,lab2)

enter2008 <- estat_getStatsData(
  appId = appid_fu,
  statsDataId = "0003017068",
  cdCat04 = "0000000030") %>%  #就業年限4年に限る
  filter(cat02_code =="0000000020" | cat02_code == "0000000030") %>% #男性、女性
  filter(cat03_code != "00000") %>% 
  mutate(major=grepl("【", `関係学科別（大）`, fixed=TRUE)) %>% 
  filter(major==FALSE) %>% 
  mutate(lab1 = str_sub(cat01_code,-5,-3),
         lab2 = str_sub(cat01_code,-2,-1)) %>% 
  left_join(enter2008x, by = c("lab1")) %>% 
  mutate(majorx = if_else(majorx=="幼稚園課程","【教育】",majorx),
         majorx = if_else(majorx=="船舶工学","【工学】",majorx)) %>% 
  dplyr::select(sex=`cat02_code`,inst=`cat03_code`,inst_lab=`設置者別`,major=`cat01_code`,major_lab=`関係学科別（大）`,majorx,enter2008=value,lab1) %>% 
  mutate(major_lab = paste(majorx,"-",major_lab),
         major_lab = gsub("【", "", major_lab, fixed = TRUE),
         major_lab = gsub("】", "", major_lab, fixed = TRUE),
         major_lab = gsub(" ", "", major_lab, fixed = TRUE)) %>% 
  dplyr::select(-majorx)

#関係学科別卒業者数 2012（就業年限4年） 最低修業年数卒業者
datalist <- estat_getStatsList(appId = appid_fu, searchWord = "学校基本調査 卒業後の状況調査 大学 ")
meta <- estat_getMetaInfo(appId = appid_fu, statsDataId = "0003074172")
grad2012 <- estat_getStatsData(
  appId = appid_fu,
  statsDataId = "0003074172",
  cdTime="2012000000",
  cdCat04 = "0000001100") %>% #最低修業年数卒業者
  filter(cat03_code =="0000000020" | cat03_code == "0000000030") %>% #男性、女性
  filter(cat02_code != "00000" & cat02_code != "45000" ) %>% 
  mutate(major=grepl("-", `関係学科別`, fixed=TRUE)) %>% 
  filter(major==TRUE) %>% 
  dplyr::select(sex=`cat03_code`,inst=`cat02_code`,major_lab=`関係学科別`,grad2012=value)

#関係学科別卒業者数 2013（就業年限4年）1年超過
grad2013 <- estat_getStatsData(
  appId = appid_fu,
  statsDataId = "0003074172",
  cdTime="2013000000",
  cdCat04 = "0000001000") %>% #1年超過
  filter(cat03_code =="0000000020" | cat03_code == "0000000030") %>% #男性、女性
  filter(cat02_code != "00000" & cat02_code != "45000" ) %>% 
  mutate(major=grepl("-", `関係学科別`, fixed=TRUE)) %>% 
  filter(major==TRUE) %>% 
  dplyr::select(sex=`cat03_code`,inst=`cat02_code`,major_lab=`関係学科別`,grad2013=value)

#関係学科別卒業者数 2014（就業年限4年）2年超過
meta <- estat_getMetaInfo(appId = appid_fu, statsDataId = "0003106712")
grad2014 <- estat_getStatsData(
  appId = appid_fu,
  statsDataId = "0003106712",
  cdCat04 = "0000002000") %>% #2年超過
  filter(cat03_code =="0000000020" | cat03_code == "0000000030") %>% #男性、女性
  filter(cat02_code != "00000" & cat02_code != "45000" ) %>% 
  mutate(major=grepl("-", `関係学科別`, fixed=TRUE)) %>% 
  filter(major==TRUE) %>% 
  dplyr::select(sex=`cat03_code`,inst=`cat02_code`,major_lab=`関係学科別`,grad2014=value)

#関係学科別卒業者数 2015（就業年限4年）3年超過
meta <- estat_getMetaInfo(appId = appid_fu, statsDataId = "0003143766")
grad2015 <- estat_getStatsData(
  appId = appid_fu,
  statsDataId = "0003143766",
  cdCat04 = "0000003000") %>% #3年超過
  filter(cat03_code =="0000000020" | cat03_code == "0000000030") %>% #男性、女性
  filter(cat02_code != "00000" & cat02_code != "45000" ) %>% 
  mutate(major=grepl("-", `関係学科別`, fixed=TRUE)) %>% 
  filter(major==TRUE) %>% 
  dplyr::select(sex=`cat03_code`,inst=`cat02_code`,major_lab=`関係学科別`,grad2015=value)

#関係学科別卒業者数 2015（就業年限4年）4年超過
meta <- estat_getMetaInfo(appId = appid_fu, statsDataId = "0003198180")
grad2016 <- estat_getStatsData(
  appId = appid_fu,
  statsDataId = "0003198180",
  cdCat04 = "0000006000") %>% #4年超過
  filter(cat03_code =="0000000020" | cat03_code == "0000000030") %>% #男性、女性
  filter(cat02_code != "00000" & cat02_code != "45000" ) %>% 
  mutate(major=grepl("-", `関係学科別`, fixed=TRUE)) %>% 
  filter(major==TRUE) %>% 
  dplyr::select(sex=`cat03_code`,inst=`cat02_code`,major_lab=`関係学科別`,grad2016=value)

merge2008 <- enter2008 %>% 
  left_join(grad2012,by=c("sex","inst","major_lab")) %>% 
  left_join(grad2013,by=c("sex","inst","major_lab")) %>% 
  left_join(grad2014,by=c("sex","inst","major_lab")) %>% 
  left_join(grad2015,by=c("sex","inst","major_lab")) %>% 
  left_join(grad2016,by=c("sex","inst","major_lab")) %>% 
  filter(is.na(grad2012)==FALSE) %>% 
  filter(enter2008 !=0) %>% 
  filter(lab1 != 200) %>% 
  mutate(grad2014=if_else(is.na(grad2014)==TRUE,0,grad2014),
         rate=(grad2012+grad2013+grad2014+grad2015+grad2016)/enter2008,
         dropout=100*(1-rate),
         sex=case_when(
           sex == "0000000020" ~ "Male",
           sex == "0000000030" ~ "Female")) %>% 
  filter(major != "0000001020" & major !="0000004100")%>% 
  dplyr::select(sex,inst_lab,major_lab,dropout) #外れ値を除く

merge2008m <- merge2008 %>% 
  filter(sex=="Male")

merge2008x <- merge2008 %>% 
  filter(sex=="Female") %>% 
  left_join(merge2008m,by=c("inst_lab","major_lab")) %>% 
  mutate(excess = dropout.y - dropout.x) %>% 
  filter(dropout.x>0 & dropout.y>0) %>% 
  group_by(inst_lab) %>% 
  mutate(av = mean(excess))
              
#---------------------------------------#
# Dropout rates among 2012 cohort
#---------------------------------------#

#関係学科別入学者数 2012
datalist <- estat_getStatsList(appId = appid_fu, searchWord = "学校基本調査 平成２３年度以降 高等教育機関《報告書掲載集計》 学校調査 大学・大学院 ")
meta <- estat_getMetaInfo(appId = appid_fu, statsDataId = "0003073995")
enter <- estat_getStatsData(
  appId = appid_fu,
  statsDataId = "0003073995",
  cdTime="2012000000",
  cdCat01 = "0000000031") %>%  #就業年限4年に限る
  filter(cat02_code =="0000000020" | cat02_code == "0000000030") %>% #男性、女性
  mutate(major=grepl("-", `関係学科別`, fixed=TRUE)) %>% 
  filter(major==TRUE) %>% 
  dplyr::select(sex=`cat02_code`,inst=`cat03_code`,inst_lab=`設置者別`,major=`cat04_code`,major_lab=`関係学科別`,enter=value)

#関係学科別卒業者数 2016（就業年限4年）
datalist <- estat_getStatsList(appId = appid_fu, searchWord = "学校基本調査 卒業後の状況調査 大学")
meta <- estat_getMetaInfo(appId = appid_fu, statsDataId = "0003198180")

gradH28 <- estat_getStatsData(
  appId = appid_fu,
  statsDataId = "0003198180",
  cdCat04 = "0000001100") %>% 　#最低修業年数卒業者
  filter(cat03_code =="0000000020" | cat03_code == "0000000030") %>% #男性、女性
  mutate(major=grepl("-", `関係学科別`, fixed=TRUE)) %>% 
  filter(major==TRUE & cat02_code != "45000")%>% 
  dplyr::select(sex=`cat03_code`,inst=`cat02_code`,inst_lab=`設置者等別`,major=`cat06_code`,major_lab=`関係学科別`,grad=value)

df <- enter %>% 
  left_join(gradH28) %>% 
  filter(is.na(grad)==FALSE & enter != 0) %>% 
  mutate(sex=case_when(
    sex == "0000000020" ~ "Male",
    sex == "0000000030" ~ "Female"),
    rate = 100*(1-(grad/enter))) 

######################################################################
# Data Viz: 
######################################################################
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

hline_dat = data.frame(inst_lab=c("国立", "公立", "私立"),
                       threshold=c(2.76, 2.36, 6.28))

ggplot(merge2008x, mapping = aes(x=major_lab,y=excess))+
  geom_point()+facet_wrap(~inst_lab, ncol = 4)+ xlab("関係学科") + ylab("男性中退率-女性中退率（%）")+ coord_flip() +
  theme(legend.title=element_blank()) + 
  geom_hline(data=hline_dat, aes(yintercept = threshold),linetype="dotted") +
  geom_hline(yintercept = 0,linetype="solid",size=0.1) +
  scale_colour_manual(values=cbp1) +ggtitle("2008年度4年制大学入学者に占める8年以内中退率の男女差（点線は平均）")
ggsave(height=10,width=8,dpi=200, filename="2.Results/1.Fig/8yearsDropout2008.png",  family = "Helvetica")

ggplot(df, mapping = aes(x=major_lab,y=rate,group=sex,color=sex,shape=sex))+
  geom_point()+facet_wrap(~inst_lab, ncol = 4)+ xlab("Major") + ylab("")+ coord_flip() +
  theme(legend.title=element_blank()) +
  scale_colour_manual(values=cbp1) +ggtitle("Crude dropout rate by sex and institution type among 2012 cohorts")
ggsave(height=9,width=16,dpi=200, filename="2.Results/1.Fig/Crude2012.png",  family = "Helvetica")


