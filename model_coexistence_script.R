## script to model species coexistence from a pot experiment

# loading package
library(here) # where am I?
library(styler) # nice and consistent styling of your code

path<-here()
setwd(path)

coexistence_data<-read.table("data/coexistence_advanced_course.txt",
                             header = T,
                             sep = " ")

#getting the lambda of species 1, crepis
data_crepis<-subset(coexistence_data, 
                    coexistence_data$foc_specie=="cre.bie")
rownames(data_crepis)<-NULL

#getting the lambda of my species 2, dactylis
data_dactylis<-subset(coexistence_data,
                      coexistence_data$foc_specie=="dac.glo")
rownames(data_dactylis)<-NULL

#getting biomass of crepis growing alone
data_crepis_alone<-subset(data_crepis,
                          data_crepis$comp_specie=="0")
head(data_crepis_alone)

#getting biomass of dactylis growing alone
data_dactylis_alone<-subset(data_dactylis,
                            data_dactylis$comp_specie=="0")

#calculate lambda for crepis
lambda_crepis<-mean(data_crepis_alone$Biomasse)

#calculate lambda for dactylis
lambda_dactylis<-mean(data_dactylis_alone$Biomasse)

#getting the mean biomass of crepis growing intraspecific competition
data_crepis_intra<-subset(data_crepis, data_crepis$Competition==1)

#getting the biomass of dactylis growing in intraspecific competition
data_dactylis_intra<-subset(data_dactylis, data_dactylis$Competition==1)

#calculating intraspecific competition coefficient for crepis
crepis_intra<-log(mean(data_crepis_intra$Biomasse)/lambda_crepis)

#calculation intraspecific conpetition coefficient for dactylis
dactylis_intra<-log(mean(data_dactylis_intra$Biomasse)/lambda_dactylis)

#calculating interspecific competition coefficient of dactylis impact on crepis
crepis_inter_data<-subset(data_crepis, data_crepis$comp_specie=="dac.glo")

#calculating interspecific comeptition coeffient of crepis impact on dactylis
dactylis_inter_data<-subset(data_dactylis, data_dactylis$comp_specie=="cre.bie")

#calculate the log response ratio
crepis_inter<-log(mean(crepis_inter_data$Biomasse)/lambda_crepis)
dactylis_inter<-log(mean(dactylis_inter_data$Biomasse)/lambda_dactylis)

#niche differences
niche_diff<-1-sqrt((crepis_inter*dactylis_inter)/(crepis_intra*dactylis_intra))

#fitness differences
fitness_diff<-lambda_dactylis/lambda_crepis*sqrt((crepis_intra*crepis_inter)/(dactylis_intra*dactylis_inter))



##########################################################

