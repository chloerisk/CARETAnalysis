library(tidyverse)

# Problems with this database:
# 1) It does not include NSCC!


########################################################################################
# USE THIS FUNCTION TO WRAP TEXT IF TOO LONG

swr = function(string, nwrap=30) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}
swr = Vectorize(swr)

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

StudentsByGender <- CurrentITStudentsInNS %>% 
  group_by(`Gender`) %>% 
  summarise(Total_Enroled = sum(Enrolment))

ggplot(data = StudentsByGender, aes(x = `Gender`, y = Total_Enroled, fill = Gender)) + 
  geom_col() +
  ggtitle(label = "Total IT Students in NS by Gender (2019-2020)") +
  theme(legend.position = "none")+
  ylab("Total Enrolment")

StudentsByStatusAndGender <- CurrentITStudentsInNS %>% 
  group_by(`Registration status`, `Gender`) %>% 
  summarise(Total_Enroled = sum(Enrolment))

ggplot(data = StudentsByStatusAndGender, aes(x = Gender, y = Total_Enroled, fill = Gender)) + 
  geom_col() +
  facet_wrap(~ `Registration status`)+
  ggtitle(label = "Total IT Students in NS by Gender and Registration Status (2019-2020)")+
  theme(legend.position = "none")+
  ylab("Total Enrolment")


# INSTITUTION

StudentsByUni <- CurrentITStudentsInNS %>% 
  group_by(`Institution`) %>% 
  summarise(Total_Enroled = sum(Enrolment))

ggplot(data = StudentsByUni, aes(x = Institution, y = Total_Enroled, fill = Institution)) + 
  geom_col() +
  ggtitle(label = "Total IT Students in NS by Institution (2019-2020)")+
  theme(legend.position = "none")+
  ylab("Total Enrolment")


# LEVEL OF STUDY

StudentsByLevel <- CurrentITStudentsInNS %>% 
  group_by(`Level of study`) %>% 
  summarise(Total_Enroled = sum(Enrolment))

ggplot(data = StudentsByLevel, aes(x = `Level of study`, y = Total_Enroled, fill = `Level of study`)) + 
  geom_col() +
  ggtitle(label = "Total IT Students in NS by Level (2019-2020)")+
  theme(legend.position = "none")+
  ylab("Total Enrolment")


# AGE

StudentsByAge <- CurrentITStudentsInNS %>% 
  group_by(`Age group`) %>% 
  summarise(Total_Enroled = sum(Enrolment))

ggplot(data = StudentsByAge, aes(x = `Age group`, y = Total_Enroled, fill = `Age group`)) + 
  geom_col() +
  ggtitle(label = "Total IT Students in NS by Age (2019-2020)")+
  theme(legend.position = "none")+
  ylab("Total Enrolment")

AllStudentsByAge <- AllCurrentStudentsInNS %>% 
  group_by(`Age group`, `Major Field of Study`) %>% 
  summarise(Total_Enroled = sum(Enrolment))

ggplot(data = AllStudentsByAge, aes(x = `Age group`, y = Total_Enroled, fill = `Age group`)) + 
  geom_col() +
  facet_wrap(~ `Major Field of Study`)+
  ggtitle(label = "Students in NS by Age Group & Major (2019-2020)")+
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ylab("Total Enrolment")


# CREDENTIAL TYPE

StudentsByCredential <- CurrentITStudentsInNS %>% 
  group_by(`Credential type`, `Institution`) %>% 
  summarise(Total_Enroled = sum(Enrolment))

ggplot(data = StudentsByCredential, aes(x = `Credential type`, y = Total_Enroled, fill = `Credential type`)) + 
  geom_col() +
  ggtitle(label = "Total IT Students in NS by Credential Type (2019-2020)")+
  theme(legend.position = "none")+
  ylab("Total Enrolment")


# IMMIGRATION STATUS

StudentsByImm <- CurrentITStudentsInNS %>% 
  group_by(`Immigration status`) %>% 
  summarise(Total_Enroled = sum(Enrolment))

ggplot(data = StudentsByImm, aes(x = `Immigration status`, y = Total_Enroled, fill = `Immigration status`)) + 
  geom_col() +
  ggtitle(label = "Total IT Students in NS by Immigration Status (2019-2020)")+
  theme(legend.position = "none")+
  ylab("Total Enrolment")

AllStudentsByImm <- AllCurrentStudentsInNS %>% 
  group_by(`Immigration status`, `Major Field of Study`) %>% 
  summarise(Total_Enroled = sum(Enrolment))

ggplot(data = AllStudentsByImm, aes(x = `Immigration status`, y = Total_Enroled, fill = `Immigration status`)) + 
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

# REGISTRATION STATUS

