library(tidyverse)

# Problems with this database:
# 1) It does not include NSCC!


########################################################################################
# USE THIS FUNCTION TO WRAP TEXT IF TOO LONG

swr = function(string, nwrap=30) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}
swr = Vectorize(swr)

swrSmall = function(string, nwrap=6) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}
swrSmall = Vectorize(swrSmall)

# Change to TABLE$COLUMN = swr(SAME)

AllStudentsByImm$`Major Field of Study` = swr(AllStudentsByImm$`Major Field of Study`)
########################################################################################


CurrentITStudentsInNS <- Enrolment_2019_2020 %>% 
  filter(`Report year` == "2019-2020" 
         & `Major Field of Study` == "Mathematics, computer and information sciences" 
         & `Province of study` == "Nova Scotia")

AllCurrentStudentsInNS <- Enrolment_2019_2020 %>% 
  filter(`Report year` == "2019-2020" & `Province of study` == "Nova Scotia")


# GENDER

CurrentITStudentsByGender <- CurrentITStudentsInNS %>% 
  group_by(`Gender`) %>% 
  summarise(Total_Enroled = sum(Enrolment))

ggplot(data = CurrentITStudentsByGender, aes(x = `Gender`, y = Total_Enroled, fill = Gender)) + 
  geom_col() +
  ggtitle(label = "Total IT Students in NS by Gender (2019-2020)") +
  theme(legend.position = "none")+
  ylab("Total Enrolment")

CurrentITStudentsByStatusAndGender <- CurrentITStudentsInNS %>% 
  group_by(`Registration status`, `Gender`) %>% 
  summarise(Total_Enroled = sum(Enrolment))

ggplot(data = CurrentITStudentsByStatusAndGender, aes(x = Gender, y = Total_Enroled, fill = Gender)) + 
  geom_col() +
  facet_wrap(~ `Registration status`)+
  ggtitle(label = "Total IT Students in NS by Gender and Registration Status (2019-2020)")+
  theme(legend.position = "none")+
  ylab("Total Enrolment")


# INSTITUTION

CurrentITStudentsByUni <- CurrentITStudentsInNS %>% 
  group_by(`Institution`) %>% 
  summarise(Total_Enroled = sum(Enrolment))

ggplot(data = CurrentITStudentsByUni, aes(x = Institution, y = Total_Enroled, fill = Institution)) + 
  geom_col() +
  ggtitle(label = "Total IT Students in NS by Institution (2019-2020)")+
  theme(legend.position = "none")+
  ylab("Total Enrolment")


# LEVEL OF STUDY

CurrentITStudentsByLevel <- CurrentITStudentsInNS %>% 
  group_by(`Level of study`) %>% 
  summarise(Total_Enroled = sum(Enrolment))

ggplot(data = CurrentITStudentsByLevel, aes(x = `Level of study`, y = Total_Enroled, fill = `Level of study`)) + 
  geom_col() +
  ggtitle(label = "Total IT Students in NS by Level (2019-2020)")+
  theme(legend.position = "none")+
  ylab("Total Enrolment")


# AGE

CurrentITStudentsByAge <- CurrentITStudentsInNS %>% 
  group_by(`Age group`) %>% 
  summarise(Total_Enroled = sum(Enrolment))

ggplot(data = CurrentITStudentsByAge, aes(x = `Age group`, y = Total_Enroled, fill = `Age group`)) + 
  geom_col() +
  ggtitle(label = "Total IT Students in NS by Age (2019-2020)")+
  theme(legend.position = "none")+
  ylab("Total Enrolment")

AllCurrentStudentsByAge <- AllCurrentStudentsInNS %>% 
  group_by(`Age group`, `Major Field of Study`) %>% 
  summarise(Total_Enroled = sum(Enrolment))

AllCurrentStudentsByAge$`Major Field of Study` = swr(AllCurrentStudentsByAge$`Major Field of Study`)

ggplot(data = AllCurrentStudentsByAge, aes(x = `Age group`, y = Total_Enroled, fill = `Age group`)) + 
  geom_col() +
  facet_wrap(~ `Major Field of Study`)+
  ggtitle(label = "Students in NS by Age Group & Major (2019-2020)")+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("Total Enrolment")


# REGISTRATION STATUS

CurrentITStudentsByRegStatus <- CurrentITStudentsInNS %>% 
  group_by(`Registration status`) %>% 
  summarise(Total_Enroled = sum(Enrolment))

ggplot(data = CurrentITStudentsByRegStatus, aes(x = `Registration status`, y = Total_Enroled, fill = `Registration status`)) + 
  geom_col() +
  ggtitle(label = "Total IT Students in NS by Registration Status (2019-2020)")+
  theme(legend.position = "none")+
  ylab("Total Enrolment")


# IMMIGRATION STATUS

CurrentITStudentsByImm <- CurrentITStudentsInNS %>% 
  group_by(`Immigration status`) %>% 
  summarise(Total_Enroled = sum(Enrolment))

ggplot(data = CurrentITStudentsByImm, aes(x = `Immigration status`, y = Total_Enroled, fill = `Immigration status`)) + 
  geom_col() +
  ggtitle(label = "Total IT Students in NS by Immigration Status (2019-2020)")+
  theme(legend.position = "none")+
  ylab("Total Enrolment")

AllCurrentStudentsByImm <- AllCurrentStudentsInNS %>% 
  group_by(`Immigration status`, `Major Field of Study`) %>% 
  summarise(Total_Enroled = sum(Enrolment))

AllCurrentStudentsByImm$`Major Field of Study` = swr(AllCurrentStudentsByImm$`Major Field of Study`)

ggplot(data = AllCurrentStudentsByImm, aes(x = `Immigration status`, y = Total_Enroled, fill = `Immigration status`)) + 
  geom_col() +
  facet_wrap(~ `Major Field of Study`)+
  ggtitle(label = "Students in NS by Immigration Status & Major (2019-2020)")+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("Total Enrolment")

AllYearsStudentsByImm <- Enrolment_2019_2020 %>%
  filter(`Major Field of Study` == "Mathematics, computer and information sciences" 
         & `Province of study` == "Nova Scotia") %>%
  group_by(`Immigration status`, `Report year`, `Major Field of Study`) %>% 
  summarise(Total_Enroled = sum(Enrolment))

ggplot(data = AllYearsStudentsByImm, aes(x = `Immigration status`, y = Total_Enroled, fill = `Immigration status`)) + 
  geom_col() +
  facet_wrap(~ `Report year`)+
  ggtitle(label = "IT Students in NS by Immigration Status & Year (2019-2020)")+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("Total Enrolment")


# OVERALL ENROLEMENT

ITEnrolmentByYear <- Enrolment_2019_2020 %>%
  filter(`Major Field of Study` == "Mathematics, computer and information sciences" 
         & `Province of study` == "Nova Scotia") %>%
  group_by(`Report year`, `Major Field of Study`) %>% 
  summarise(Total_Enroled = sum(Enrolment))

ggplot(data = ITEnrolmentByYear, aes(x = `Report year`, y = Total_Enroled, fill = `Report year`)) + 
  geom_col() +
  ggtitle(label = "Total IT Students in NS by Year")+
  theme(legend.position = "none")+
  ylab("Total Enrolment")


AllYearsStudentsInNS <- Enrolment_2019_2020 %>% 
  filter(`Province of study` == "Nova Scotia") %>%
  group_by(`Report year`, `Major Field of Study`) %>% 
  summarise(Total_Enroled = sum(Enrolment))

ggplot(data = AllYearsStudentsInNS, aes(x = `Major Field of Study`, y = Total_Enroled, color = `Major Field of Study`)) + 
  geom_point() +
  facet_wrap(~ `Report year`)+
  ggtitle(label = "NS Student Enrolment by Major and Year")+
  theme(legend.position = "right",
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("Total Enrolment")


ggplot(data = AllYearsStudentsInNS, aes(x = `Report year`, y = Total_Enroled, color = `Major Field of Study`, group = `Major Field of Study`)) + 
  geom_point() +
  geom_line() +
  ggtitle(label = "NS Student Enrolment by Major and Year")+
  theme(legend.position = "right")+
  ylab("Total Enrolment")


