library(ggplot2)
library(stringr)
library(gridExtra)
library(ggpubr)

source("code/9_matching_analysis.R")

all.pop.combined.no.missing <- readRDS("data/entire.population.no.missing.rds")
fresh.pop.combined.no.missing <- readRDS("data/freshmen.population.no.missing.RDS")

table_for_bars <- function(sample_name, partdf, completedf, outcome)
  
{
  # participated
  
  participated <- subset(cbind.data.frame(outvar=partdf$outvar, cem=partdf$cem),outvar==outcome)
  print(participated)
  participated$se <- str_extract(participated$cem, "[(][0-9][\\.][0-9]{1,2}")
  print(participated$se)
  participated$se <- str_extract(participated$se, "[0-9][\\.][0-9]*")
  print((participated$se))
  participated$se <- as.numeric(participated$se)
  participated$mean_diff <- as.numeric(str_extract(participated$cem, "[-]?[0-9][\\.][0-9]*\\s*"))
  
  participated$treatment <- "Participants"
  participated$Sample <- sample_name
  participated$outvar <- NULL
  participated$cem <- NULL
  participated$n <- paste0("n=",as.numeric(as.character(partdf$cem[4])))
  
  #coached 
  
  completed <- subset(cbind.data.frame(outvar=completedf$outvar, cem=completedf$cem),outvar==outcome)
  completed$se <- str_extract(completed$cem, "[(][0-9][\\.][0-9]{1,2}")
  completed$se <- str_extract(completed$se, "[0-9][\\.][0-9]{1,2}")
  completed$se <- as.numeric(completed$se)
  completed$mean_diff <- as.numeric(str_extract(completed$cem, "[-]?[0-9][\\.][0-9]{1,2}\\s*"))

  
 
  
  completed$treatment <- "Completers"
  completed$Sample <- sample_name
  completed$outvar <- NULL
  completed$cem <- NULL
  completed$n <- paste0("n=",as.numeric(as.character(completedf$cem[4])))
  
  final <- rbind(participated, completed)
  final$outcome <- outcome
  return(final)
}



##########################
# ENROLLED NEXT SEMESTER 
##########################

all_students <- table_for_bars("Invited Students", results.invited.coached, results.invited.completed, "enrolled.next.semester")
high.gpa <- table_for_bars("GPA >= 1.0", results.high.coached, results.high.completed, "enrolled.next.semester")
low.gpa <- table_for_bars("GPA < 1.0", results.low.coached, results.low.completed, "enrolled.next.semester")

enrolled <- rbind(all_students, high.gpa, low.gpa)
enrolled$Sample<- factor(enrolled$Sample, levels=c("Invited Students","GPA >= 1.0","GPA < 1.0"))
enrolled$treatment <- factor(enrolled$treatment, levels=c("Participants", "Completers"))

# Default bar plot
p_enrolled<- ggplot(enrolled, aes(x=Sample, y=mean_diff, fill=treatment)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_diff-2*se, ymax=mean_diff+2*se), width=.5,
                position=position_dodge(.9)) 

# Finished bar plot
p_enrolled <- p_enrolled+labs( y = "Difference in Next Semester Enrollment")+
  theme_classic() +
  scale_fill_manual(values=c('#FFFFFF', '#999999'))+
  theme(legend.title=element_blank())+
  geom_text(data = enrolled,aes(y=-.02,label = n),
            size = 3.5,position = position_dodge(width = 0.9))



##########################
# Invited Term GPA 
##########################

all_students <- table_for_bars("Invited Students", results.invited.coached, results.invited.completed, "invited.term.gpa")
high.gpa <- table_for_bars("GPA >= 1.0", results.high.coached, results.high.completed, "invited.term.gpa")
low.gpa <- table_for_bars("GPA < 1.0", results.low.coached, results.low.completed, "invited.term.gpa")

enrolled <- rbind(all_students, high.gpa, low.gpa)
enrolled$Sample<- factor(enrolled$Sample, levels=c("Invited Students","GPA >= 1.0","GPA < 1.0"))
enrolled$treatment <- factor(enrolled$treatment, levels=c("Participants", "Completers"))



# Default bar plot
p_gpa<- ggplot(enrolled, aes(x=Sample, y=mean_diff, fill=treatment)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_diff-2*se, ymax=mean_diff+2*se), width=.5,
                position=position_dodge(.9)) 

# Finished bar plot
p_gpa<- p_gpa+labs( y = "Difference in Coaching Semester GPA")+
  theme_classic() +
  scale_fill_manual(values=c('#FFFFFF', '#999999'))+
  theme(legend.title=element_blank())+
  geom_text(data = enrolled,aes(y=-.05,label = n),
            size = 3.5,position = position_dodge(width = 0.9))


##########################
# next.semester.credits
##########################

all_students <- table_for_bars("Invited Students", results.invited.coached, results.invited.completed, "next.semester.credits")
high.gpa <- table_for_bars("GPA >= 1.0", results.high.coached, results.high.completed, "next.semester.credits")
low.gpa <- table_for_bars("GPA < 1.0", results.low.coached, results.low.completed, "next.semester.credits")

enrolled <- rbind(all_students, high.gpa, low.gpa)
enrolled$Sample<- factor(enrolled$Sample, levels=c("Invited Students","GPA >= 1.0","GPA < 1.0"))
enrolled$treatment <- factor(enrolled$treatment, levels=c("Participants", "Completers"))
#enrolled$n<-c("n=750","n=566","n=587","n=417","n=55","n=37")

# Default bar plot
p_credit<- ggplot(enrolled, aes(x=Sample, y=mean_diff, fill=treatment)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_diff-2*se, ymax=mean_diff+2*se), width=.5,
                position=position_dodge(.9)) 

# Finished bar plot
p_credit <- p_credit+labs( y = "Difference in Next Semester Completed Credits")+
  theme_classic() +
  scale_fill_manual(values=c('#FFFFFF', '#999999'))+
  theme(legend.title=element_blank())+
  geom_text(data = enrolled,aes(y=-.2,label = n),
            size = 3.5,position = position_dodge(width = 0.9))
