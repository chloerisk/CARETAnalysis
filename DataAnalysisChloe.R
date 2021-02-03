library(tidyverse)

#line graphs for NS students based on gender and major
NSStudentsByGender_Major <- Enrolment_2019_2020 %>% 
  filter(`Province of study` == "Nova Scotia") %>%
  group_by(`Report year`, `Gender`, `Major Field of Study`) %>%
  summarize(Yearly_Total = sum(Enrolment))

ggplot(data = NSStudentsByGender_Major, 
       mapping = aes(x =`Report year`, 
                     y = Yearly_Total, 
                     colour = `Gender`, 
                     group = `Gender`)) + 
  geom_line(size = 1.3) +
  geom_point() + 
  facet_wrap(.~`Major Field of Study`, 
             scales = "free_y") +
  ggtitle(label = "Students in NS by Major and Gender") +
  xlab("Academic Year") + 
  ylab("Yearly Enrolment") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 75, vjust = 0.5, hjust = 0.5))


#Mathematics, computer and information sciences students by gender from 2003-2020
ITStudentsByGender <- Enrolment_2019_2020 %>% 
  filter(`Province of study` == "Nova Scotia", `Major Field of Study` == "Mathematics, computer and information sciences") %>%
  group_by(`Report year`, `Gender`) %>%
  summarize(Yearly_Total = sum(Enrolment))

ggplot(data = ITStudentsByGender, aes(x = `Report year`, y = Yearly_Total, colour = `Gender`, fill = `Gender`, group = `Gender`)) + 
  geom_bar(stat = "identity") + 
  ggtitle(label = "Mathematics, Computer and Information Sciences Students by Gender from 2003-2020")

#Line graph for International Students enrolled in NS by Year and Gender
FullTimeIntStudentsInNS <- Enrolment_2019_2020 %>% 
  filter(`Province of study` == "Nova Scotia" & `Immigration status` == "International") %>%
  group_by(`Report year`, `Gender`) %>%
  summarize(Yearly_Total = sum(Enrolment))
ggplot(data = FullTimeIntStudentsInNS, aes(x = `Report year`, y = Yearly_Total, colour = `Gender`, group=`Gender`)) + 
  geom_line(size = 1.3) +
  geom_point() +
  ggtitle(label = "International Students enrolled in NS by Year and Gender")
