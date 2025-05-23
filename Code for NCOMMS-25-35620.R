#To: Sugar Rationing During the First 1000 Days of Life and Lifelong Risk of Heart Failure
#by Haoxian Tang, Xuan Zhang, Jingtao Huang, Jianan Hong, Hanyuan Lin, Cuihong Tian, Nan Luo, Xiaojing Chen, Mengyue Lin, Qinglong Yang, Shiwan Wu, Pan Chen, Jiasheng Wen, Liwen Jiang, Youti Zhang, Xuerui Tan, Yequn Chen.


library(readr)
library(dplyr)
library(survival)
library(tidyr)
library(janitor)
library(ggplot2)
library(rstpm2)

#Data collection--------------------------------------------------------------------------


sugar_rationing <- read_csv("D:/sugar_rationing.csv")

sugar_rationing <- sugar_rationing %>%clean_names(case = "mixed")%>%
                   mutate(
                         Participant_ID=as.character(Participant_Id),
                         father_cvd=if_else(multi_str(Illnesses_of_father_Instance_0, c("Heart disease","Stroke","High blood pressure")),1,0),
                         mother_cvd=if_else(multi_str(Illnesses_of_mother_Instance_0, c("Heart disease","Stroke","High blood pressure")),1,0),
                         father_dm=if_else(multi_str(Illnesses_of_father_Instance_0, c("Diabetes")),1,0),
                         mother_dm=if_else(multi_str(Illnesses_of_mother_Instance_0, c("Diabetes")),1,0),
                         family_cvd=if_else(father_cvd==1|father_cvd==1,1,0),
                         family_dm=if_else(father_dm==1|mother_dm==1,1,0),
      
      
                         Race=case_when(
                           Ethnic_background_Instance_0 %in% c("Prefer not to answer","Do not know")~NA,
                           is.na(Ethnic_background_Instance_0)~NA,
                           Ethnic_background_Instance_0 %in% c("White","British","Irish","Any other white background")~"White",
                           TRUE~"Others"
                         ),
      
                         Country=case_when(
                           Country_of_birth_Uk_elsewhere_Instance_0=="England"~"England",
                           Country_of_birth_Uk_elsewhere_Instance_0=="Scotland"~"Scotland",
                           Country_of_birth_Uk_elsewhere_Instance_0=="Wales"~"Wales",
                           TRUE~NA
                         ),
      
                         Breastfed_as_a_baby=case_when(
                           Breastfed_as_a_baby_Instance_0 %in% c("Prefer not to answer","Do not know")~NA,
                           TRUE~Breastfed_as_a_baby_Instance_0
                         ),
      
                         Comparative_body_size_at_age_10=case_when(
                           Comparative_body_size_at_age_10_Instance_0 %in% c("Prefer not to answer","Do not know")~NA,
                           TRUE~Comparative_body_size_at_age_10_Instance_0
                         ),
      
                         Comparative_height_size_at_age_10=case_when(
                           Comparative_height_size_at_age_10_Instance_0 %in% c("Prefer not to answer","Do not know")~NA,
                           TRUE~Comparative_height_size_at_age_10_Instance_0
                           ),
      
                         Maternal_smoking_around_birth=case_when(
                           Maternal_smoking_around_birth_Instance_0 %in% c("Prefer not to answer","Do not know")~NA,
                           TRUE~Maternal_smoking_around_birth_Instance_0
                         )
                        )%>%
                mutate(Month_of_birth = case_when(
                          Month_of_birth == "January" ~ 1,
                          Month_of_birth == "February" ~ 2,
                          Month_of_birth == "March" ~ 3,
                          Month_of_birth == "April" ~ 4,
                          Month_of_birth == "May" ~ 5,
                          Month_of_birth == "June" ~ 6,
                          Month_of_birth == "July" ~ 7,
                          Month_of_birth == "August" ~ 8,
                          Month_of_birth == "September" ~ 9,
                          Month_of_birth == "October" ~ 10,
                          Month_of_birth == "November" ~ 11,
                          Month_of_birth == "December" ~ 12
                        ),
                       
                       Month_of_birth_group=case_when(
                         Month_of_birth<=3~1,
                         Month_of_birth<=6~2,
                         Month_of_birth<=9~3,
                         Month_of_birth<=12~4
                       ),

                       birth_date=Year_of_birth*100+Month_of_birth,

                      Rationing1=case_when(
                        birth_date >=195110 & birth_date <= 195406 ~"rationed",
                        birth_date >=195407 & birth_date <= 195603 ~"non-rationed"
                      ),
                      
                      Rationing2=case_when(
                        birth_date >=195110 & birth_date <= 195203 ~"in_utero_24", 
                        birth_date >=195204 & birth_date <= 195209 ~"in_utero_18", 
                        birth_date >=195210 & birth_date <= 195303 ~"in_utero_12", 
                        birth_date >=195304 & birth_date <= 195309 ~"in_utero_6",
                        birth_date >=195310 & birth_date <= 195406 ~"in_utero",

                        birth_date >=195407 & birth_date <= 195412 ~"after_9_15",
                        birth_date >=195501 & birth_date <= 195506 ~"after_15_21", 
                        birth_date >=195507 & birth_date <= 195603 ~"after_21" 
                      ),
                      
                      Rationing4=case_when(
                        birth_date >=195110 & birth_date <= 195209 ~"in_utero_24", 
                        birth_date >=195210 & birth_date <= 195309 ~"in_utero_12", 
                        birth_date >=195310 & birth_date <= 195406 ~"in_utero", 
                        
                        birth_date >=195407 & birth_date <= 195506 ~"after_9_21", 
                        birth_date >=195507 & birth_date <= 195603 ~"after_21" 
                      )

                )



Circulatory <- readxl::read_excel("E:/Circulatory_system_disorders_participant.xlsx")

Circulatory <- Circulatory %>%clean_names(case = "mixed") %>%
              mutate(Participant_ID=as.character(Participant_Id)
                     )%>%
              multi_join(.,sugar_rationing,by="Participant_ID")%>%

              mutate(outcome_HF=if_else(!is.na(Date_I50_first_reported_heart_failure),1,0),

                     HF_date_YM=format(  
                       lubridate::ymd(Date_I50_first_reported_heart_failure),
                       "%Y-%m"
                     ),

                     Birth_months = Year_of_birth * 12 + Month_of_birth,
                     HF_months = as.numeric(substr(HF_date_YM, 1, 4)) * 12 + as.numeric(substr(HF_date_YM, 6, 7)),
                     HF_time = HF_months - Birth_months, 
                     HF_Age = HF_time / 12 
              )%>%

              mutate(outcome_HF_date=pmin(
                      Date_lost_to_follow_up,
                      as.Date(Date_I50_first_reported_heart_failure, format = "%Y-%m-%d"),
                      as.Date("2022-12-05"),
                      na.rm = TRUE
                    ),
                    outcome_HF_date_YM=format(  
                      lubridate::ymd(outcome_HF_date),
                      "%Y-%m"
                    ),
                    outcome_HF_months = as.numeric(substr(outcome_HF_date_YM, 1, 4)) * 12 + as.numeric(substr(outcome_HF_date_YM, 6, 7)),
                    outcome_HF_time = (outcome_HF_months - Birth_months)/12, 
                    
                     )%>%

              dplyr::select(.,Participant_ID,outcome_HF,outcome_HF_time,HF_Age)

table(Circulatory$outcome_HF)



HF_PRS <- read.table("D:/res.best", header=T, sep="", stringsAsFactors=FALSE)
HF_PRS <- mutate(HF_PRS,
                 Participant_ID=as.character(IID))%>%
          quant(.,c("PRS"),Q=3)%>%
          dplyr::select(.,Participant_ID,PRS,PRS_group)



mydata <- multi_join(sugar_rationing,Circulatory,Baseline_covariates_instance0,HF_PRS,by="Participant_ID")%>%
          mutate(
            Rationing1 = factor(Rationing1,
                                level=c("non-rationed","rationed")),
            Rationing2 = factor(Rationing2,
                                level=c("after_21","after_15_21","after_9_15",    
                                        "in_utero","in_utero_6","in_utero_12","in_utero_18","in_utero_24")),
            Rationing4 = factor(Rationing4,
                                level=c("after_21","after_9_21","in_utero","in_utero_12","in_utero_24"))

          )%>%
          mutate(
            across(c(Age,Year_of_birth, Month_of_birth,PRS,outcome_HF_time,outcome_HF,HF_Age), as.numeric),
            across(c(Sex, Race1, Rationing1,Rationing2,Rationing3,Rationing4,Rationing5,Rationing6,
                     Sex,Race,Country,family_cvd,family_dm,PRS_group), as.factor)
          ) %>%
  
          dplyr::select(.,Participant_ID,Age,
                        Rationing1,Rationing2,Rationing3,Rationing4,Rationing5,Rationing6,
                        outcome_HF_time,outcome_HF,HF_Age,
                        
                        Year_of_birth,Month_of_birth,Month_of_birth_group,
                        Sex,Race,Country,family_cvd,family_dm,PRS,PRS_group,
                        
                        Maternal_smoking_around_birth,
                        Comparative_height_size_at_age_10,
                        Comparative_body_size_at_age_10,
                        
                        Adopted_as_a_child_Instance_0,Part_of_a_multiple_birth_Instance_0,Pregnant,Country)%>%
        as.data.frame()



mydata1 <- eatTools::na_omit_selection(mydata,c("Rationing1","Rationing4"))  
table(mydata1$Rationing1)

mydata1 <- subset(mydata1,Adopted_as_a_child_Instance_0 == "No")  
mydata1 <- subset(mydata1,Part_of_a_multiple_birth_Instance_0 == "No") 
mydata1 <- subset(mydata1,Pregnant=="No") 
mydata1 <- subset(mydata1,Country=="England" | Country=="Scotland"| Country=="Wales")


mydata1 <- eatTools::na_omit_selection(mydata1,c("outcome_HF","outcome_HF_time"))
mydata1 <- eatTools::na_omit_selection(mydata1,c("family_cvd","family_dm","Race","Country","Sex","PRS")) 



#Data analysis--------------------------------------------------------------------------


#1. Table1----------------

sss <- mydata1%>%
        mutate(
          across(c(Age, PRS), as.numeric),
          across(c(Sex, Race, Month_of_birth_group, Country, family_cvd, family_dm,
                   PRS_group,outcome_HF), as.factor)
        ) %>%
        as.data.frame()
      
      csv2desktop(sss)
      
table1 <- show_tableone(data=sss,
                        group_var="Rationing1",
                        include_var=c("Age","Sex","Race","Country","family_cvd",
                                            "family_dm","PRS_group",
                                            "outcome_HF","HF_Age"),
                        output_file = "C:/Users/PC/Desktop/table1.docx")


#2. Figure2----------------


library(jskm)

fit <- survfit(Surv(outcome_HF_time,outcome_HF==1)~Rationing1, data=mydata1)

KM1 <- jskm(fit,
           theme = NULL,
           
           xlabs = "Time",
           ylabs = "Cumulaive incidence",
           legend = TRUE,
           ystrataname = "Group",
           legendposition = c(0.15,0.3),
           ystratalabs  = c("Non-rationed","Rationed"),
           
           linecols = "Set1",
           linewidth = 0.75,
           dashed = F,
           ci = F,
           cut.landmark = 45,
           showpercent = F,
           marks = F,   
           
           pval=T,
           pval.coord=c(10,0.04),

           table =T,
           label.nrisk = "Numbers at risk",
           size.label.nrisk = 10,
           timeby = 5,
           
           xlims = c(0,70),
           ylims = c(0,0.05),
           surv.scale = "default",
           cumhaz = T,
           status.cmprsk = NULL)  


KM1 <- KM1 + theme(
  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")  
)



#3. Table2 & Supplementary Table1----------------


#cox------------------


#curde
fit1 <- coxph(Surv(outcome_HF_time, outcome_HF) ~ Rationing1, 
              data=mydata1)

fit2 <- coxph(Surv(outcome_HF_time, outcome_HF) ~ Rationing2, 
              data=mydata1)

fit3 <- coxph(Surv(outcome_HF_time, outcome_HF) ~ Rationing4, 
              data=mydata1)

cox_result_crude <- show_model(fit1,fit2,fit3,round = 2)
csv2desktop(cox_result_crude)


#adjust
fit1 <- coxph(Surv(outcome_HF_time, outcome_HF) ~ Rationing1+Month_of_birth+Race+Country+Sex+family_cvd+family_dm+PRS,
              data=mydata1)

fit2 <- coxph(Surv(outcome_HF_time, outcome_HF) ~ Rationing2+Month_of_birth+Race+Country+Sex+family_cvd+family_dm+PRS, 
              data=mydata1)

fit3 <- coxph(Surv(outcome_HF_time, outcome_HF) ~ Rationing4+Month_of_birth+Race+Country+Sex+family_cvd+family_dm+PRS, 
              data=mydata1)

cox_result_adjust <- show_model(fit1,fit2,fit3,round = 2)
csv2desktop(cox_result_adjust)


#rstpm2-------------

library(rstpm2)

#curde

m1 <- pstpm2(Surv(outcome_HF_time, outcome_HF == 1) ~ Rationing1,
             data = mydata1)  

m2 <- pstpm2(Surv(outcome_HF_time, outcome_HF == 1) ~ Rationing2,
             data = mydata1)

m3 <- pstpm2(Surv(outcome_HF_time, outcome_HF == 1) ~ Rationing4,
             data = mydata1)   

m4 <- pstpm2(Surv(outcome_HF_time, outcome_HF == 1) ~ Rationing4_con,
             data = mydata1)   


rstpm2_result_crude <- as.data.frame(summary(m1)@coef)
rstpm2_result_crude <- tibble::rownames_to_column(rstpm2_result_crude, var = "Group")
rstpm2_result_crude1 <- rstpm2_result_crude[2,]


rstpm2_result_crude <- as.data.frame(summary(m2)@coef)
rstpm2_result_crude <- tibble::rownames_to_column(rstpm2_result_crude, var = "Group")
rstpm2_result_crude2 <- rstpm2_result_crude[2:8,]


rstpm2_result_crude <- as.data.frame(summary(m3)@coef)
rstpm2_result_crude <- tibble::rownames_to_column(rstpm2_result_crude, var = "Group")
rstpm2_result_crude3 <- rstpm2_result_crude[2:5,]


rstpm2_result_crude <- as.data.frame(summary(m4)@coef)
rstpm2_result_crude <- tibble::rownames_to_column(rstpm2_result_crude, var = "Group")
rstpm2_result_crude4 <- rstpm2_result_crude[2,]


rstpm2_result_crude <- bind_rows(rstpm2_result_crude1,rstpm2_result_crude2,rstpm2_result_crude3,rstpm2_result_crude4)%>%
                       mutate(HR=exp(Estimate),
                              CL_up=exp(Estimate+1.96*`Std. Error`),
                              CL_low=exp(Estimate-1.96*`Std. Error`)
                              )

rstpm2_result_crude$HR_CL <- paste(sprintf("%.2f", rstpm2_result_crude$HR), 
                                   "(", sprintf("%.2f", rstpm2_result_crude$CL_low), 
                                   ",", sprintf("%.2f", rstpm2_result_crude$CL_up), 
                                   ")", sep = "")




#adjust

m1 <- pstpm2(Surv(outcome_HF_time, outcome_HF == 1) ~ Rationing1+Month_of_birth+Race+Country+Sex+family_cvd+family_dm+PRS,
             data = mydata1)  

m2 <- pstpm2(Surv(outcome_HF_time, outcome_HF == 1) ~ Rationing2+Month_of_birth+Race+Country+Sex+family_cvd+family_dm+PRS,
             data = mydata1)

m3 <- pstpm2(Surv(outcome_HF_time, outcome_HF == 1) ~ Rationing4+Month_of_birth+Race+Country+Sex+family_cvd+family_dm+PRS,
             data = mydata1)   

m4 <- pstpm2(Surv(outcome_HF_time, outcome_HF == 1) ~ Rationing4_con+Month_of_birth+Race+Country+Sex+family_cvd+family_dm+PRS,
             data = mydata1)   



rstpm2_result_adjust <- as.data.frame(summary(m1)@coef)
rstpm2_result_adjust <- tibble::rownames_to_column(rstpm2_result_adjust, var = "Group")
rstpm2_result_adjust1 <- rstpm2_result_adjust[2,]


rstpm2_result_adjust <- as.data.frame(summary(m2)@coef)
rstpm2_result_adjust <- tibble::rownames_to_column(rstpm2_result_adjust, var = "Group")
rstpm2_result_adjust2 <- rstpm2_result_adjust[2:8,]


rstpm2_result_adjust <- as.data.frame(summary(m3)@coef)
rstpm2_result_adjust <- tibble::rownames_to_column(rstpm2_result_adjust, var = "Group")
rstpm2_result_adjust3 <- rstpm2_result_adjust[2:5,]


rstpm2_result_adjust <- as.data.frame(summary(m4)@coef)
rstpm2_result_adjust <- tibble::rownames_to_column(rstpm2_result_adjust, var = "Group")
rstpm2_result_adjust4 <- rstpm2_result_adjust[2,]


rstpm2_result_adjust <- bind_rows(rstpm2_result_adjust1,rstpm2_result_adjust2,rstpm2_result_adjust3,rstpm2_result_adjust4)%>%
  mutate(HR=exp(Estimate),
         CL_up=exp(Estimate+1.96*`Std. Error`),
         CL_low=exp(Estimate-1.96*`Std. Error`)
  )

rstpm2_result_adjust$HR_CL <- paste(sprintf("%.2f", rstpm2_result_adjust$HR), 
                                   "(", sprintf("%.2f", rstpm2_result_adjust$CL_low), 
                                   ",", sprintf("%.2f", rstpm2_result_adjust$CL_up), 
                                   ")", sep = "")




#PAF-----------------
paf <- mydata1
paf$Rationing1 <- factor(paf$Rationing1,levels = c("rationed","non-rationed"))

paf$outcome_HF <- as.factor(paf$outcome_HF)
paf$Rationing1 <- as.factor(paf$Rationing1)

des(paf$outcome_HF,paf$Rationing1)

pc <- 523/1551


#cox

fit1 <- coxph(Surv(outcome_HF_time, outcome_HF==1) ~ Rationing1,
              data=paf)
R <- 1.15399

fit1 <- coxph(Surv(outcome_HF_time, outcome_HF==1) ~ Rationing1+Month_of_birth+Race+Country+Sex+family_cvd+family_dm+PRS,
              data=paf)
R <- 1.146

PAF <- pc*(R-1)/R  #0.04398284

#case <- 0.0440*55496832.84 #2441861
#case_low <- 0.0440*48996497.57 #2155846
#case_up <- 0.0440*63842413.20 #2809066


#灵活参数
m1 <- pstpm2(Surv(outcome_HF_time, outcome_HF == 1) ~ Rationing1,
             data = paf) 
R <- exp(0.15684226) 

m1 <- pstpm2(Surv(outcome_HF_time, outcome_HF == 1) ~ Rationing1+Month_of_birth+Race+Country+Sex+family_cvd+family_dm+PRS,
             data = paf) 

R <- exp(0.14984805) #1.161658
#R_low <- exp(0.14984805-1.96*0.0551434) #1.042651
#R_up <- exp(0.14984805+1.96*0.0551434) #1.294247

PAF <- pc*(R-1)/R  

#case <- 0.0465*55496832.84 #2580603



#4. Table 3--------------------------------------------------------------------------


ss <- mutate(mydata1,
             PRS_group=if_else(PRS_group=="Group2","Group1",PRS_group))

ss$PRS_group <- factor(ss$PRS_group,level=c("Group1","Group3"))
ss$Rationing1 <- factor(ss$Rationing1,level=c("rationed","non-rationed"))


fit1 <- coxph(Surv(outcome_HF_time, outcome_HF==1) ~ Rationing1*PRS_group+Sex+Month_of_birth+Race+Country+family_cvd+family_dm,
              data=ss)


library(interactionR)

out<- interactionR(fit1,
                   exposure_names =c("Rationing1", "PRS_group"),
                   ci.type ="delta", 
                   ci.level = 0.95,
                   em = F) 

interactionR_table(out) 




#5. Supplementary Table2--------------------------------------------------------------------------

m1 <- lm(HF_Age ~ Rationing1,
         data = mydata1)  
m2 <- lm(HF_Age ~ Rationing2,
         data = mydata1)  
m3 <- lm(HF_Age ~ Rationing4,
         data = mydata1)  
m4 <- lm(HF_Age ~ Rationing1+Month_of_birth+Race+Country+Sex+family_cvd+family_dm+PRS,
         data = mydata1)  
m5 <- lm(HF_Age ~ Rationing2+Month_of_birth+Race+Country+Sex+family_cvd+family_dm+PRS,
         data = mydata1)  
m6 <- lm(HF_Age ~ Rationing4+Month_of_birth+Race+Country+Sex+family_cvd+family_dm+PRS,
         data = mydata1)  

HF_Age <- show_model(m1,m2,m3,m4,m5,m6)

csv2desktop(HF_Age)




#6. Supplementary Table3--------------------------------------------------------------------------


#cox------------------

#adjust
fit1 <- coxph(Surv(outcome_HF_time, outcome_HF) ~ Rationing1+Month_of_birth+Race+Country+Sex+family_cvd+family_dm+PRS+
                Maternal_smoking_around_birth+Comparative_body_size_at_age_10+Comparative_height_size_at_age_10,
              data=mydata1)

fit2 <- coxph(Surv(outcome_HF_time, outcome_HF) ~ Rationing4+Month_of_birth+Race+Country+Sex+family_cvd+family_dm+PRS+
                Maternal_smoking_around_birth+Comparative_body_size_at_age_10+Comparative_height_size_at_age_10,
              data=mydata1)

cox_result_adjust <- show_model(fit1,fit2,round = 2)
csv2desktop(cox_result_adjust)



#rstpm2-------------

library(rstpm2)

#adjust

m1 <- pstpm2(Surv(outcome_HF_time, outcome_HF == 1) ~ Rationing1+Month_of_birth+Race+Country+Sex+family_cvd+family_dm+PRS+
               Maternal_smoking_around_birth+Comparative_body_size_at_age_10+Comparative_height_size_at_age_10,
             data = mydata1)  

m2 <- pstpm2(Surv(outcome_HF_time, outcome_HF == 1) ~ Rationing4+Month_of_birth+Race+Country+Sex+family_cvd+family_dm+PRS+
               Maternal_smoking_around_birth+Comparative_body_size_at_age_10+Comparative_height_size_at_age_10,
             data = mydata1)   

m3 <- pstpm2(Surv(outcome_HF_time, outcome_HF == 1) ~ Rationing4_con+Month_of_birth+Race+Country+Sex+family_cvd+family_dm+PRS+
               Maternal_smoking_around_birth+Comparative_body_size_at_age_10+Comparative_height_size_at_age_10,
             data = mydata1)   



rstpm2_result_adjust <- as.data.frame(summary(m1)@coef)
rstpm2_result_adjust <- tibble::rownames_to_column(rstpm2_result_adjust, var = "Group")
rstpm2_result_adjust1 <- rstpm2_result_adjust[2,]


rstpm2_result_adjust <- as.data.frame(summary(m2)@coef)
rstpm2_result_adjust <- tibble::rownames_to_column(rstpm2_result_adjust, var = "Group")
rstpm2_result_adjust2 <- rstpm2_result_adjust[2:5,]


rstpm2_result_adjust <- as.data.frame(summary(m3)@coef)
rstpm2_result_adjust <- tibble::rownames_to_column(rstpm2_result_adjust, var = "Group")
rstpm2_result_adjust3 <- rstpm2_result_adjust[2,]


rstpm2_result_adjust <- bind_rows(rstpm2_result_adjust1,rstpm2_result_adjust2,rstpm2_result_adjust3)%>%
  mutate(HR=exp(Estimate),
         CL_up=exp(Estimate+1.96*`Std. Error`),
         CL_low=exp(Estimate-1.96*`Std. Error`)
  )

rstpm2_result_adjust$HR_CL <- paste(sprintf("%.2f", rstpm2_result_adjust$HR), 
                                   "(", sprintf("%.2f", rstpm2_result_adjust$CL_low), 
                                   ",", sprintf("%.2f", rstpm2_result_adjust$CL_up), 
                                   ")", sep = "")





#7. Supplementary Table4--------------------------------------------------------------------------


library(flexsurv)

m1 <- flexsurvreg(Surv(outcome_HF_time, outcome_HF == 1) ~ Rationing1 + Month_of_birth+Race+Country+Sex+family_cvd+family_dm+PRS,
                     data = mydata1, dist = "gompertz")  

m2 <- flexsurvreg(Surv(outcome_HF_time, outcome_HF == 1) ~ Rationing4 + Month_of_birth+Race+Country+Sex+family_cvd+family_dm+PRS,
                     data = mydata1, dist = "gompertz")  

m3 <- flexsurvreg(Surv(outcome_HF_time, outcome_HF == 1) ~ Rationing4_con + Month_of_birth+Race+Country+Sex+family_cvd+family_dm+PRS,
                  data = mydata1, dist = "gompertz")  




Gompertz_result_adjust <- as.data.frame(tidy(m1))
Gompertz_result_adjust1 <- Gompertz_result_adjust[3,]


Gompertz_result_adjust <- as.data.frame(tidy(m2))
Gompertz_result_adjust2 <- Gompertz_result_adjust[3:6,]


Gompertz_result_adjust <- as.data.frame(tidy(m3))
Gompertz_result_adjust3 <- Gompertz_result_adjust[3,]



Gompertz_result_adjust <- bind_rows(Gompertz_result_adjust1,Gompertz_result_adjust2,Gompertz_result_adjust3)%>%
  mutate(HR=exp(estimate),
         CL_up=exp(estimate+1.96*std.error),
         CL_low=exp(estimate-1.96*std.error)
  )

Gompertz_result_adjust$HR_CL <- paste(sprintf("%.2f", Gompertz_result_adjust$HR), 
                                    "(", sprintf("%.2f", Gompertz_result_adjust$CL_low), 
                                    ",", sprintf("%.2f", Gompertz_result_adjust$CL_up), 
                                    ")", sep = "")



