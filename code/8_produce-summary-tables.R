source("code/7_compute-descriptive-analysis.R")
library(dplyr)


#############################
# Clean up dataframe names 
#############################


cleandf <- function(df, vartype, fresh)
{
  
  #' Cleans up descriptive table 
  #' 
  #' This cleans up the descriptive tables for both the invited and freshman populations
  #' @param df the dataframe either invited or freshman, and either continuous or binary
  #' @param either continuous or binary
  #' @param indicator if the dataframe is from the freshman population 
  rownames(df) <- NULL
  df <- as.data.frame(df)
  df <- df[-1,]

  if (vartype == "continuous")
  {
    df$variable <- recode(df$variable, 
                 invited.term.gpa = "Invited Term GPA",
                 invited.term.gpa.std = "Invited Term GPA (standardized)",
                 next.semester.credits = "Next Semester Credits",
                 next.semester.credits.std  = "Next Semester Credits (standardized)",
                 Credits_at_Entry = "Credits at Entry", 
                 First_Sem_GPA = "First Semester GPA", 
                 prior.sem.gpa = "Prior Semester GPA",
                 coaching.sessions = "Total Coaching Meetings Attended"
                 )
    if (fresh == 1)
      
    {
      df$variable <- recode(df$variable, 
                            first_year_total_credits = "Total Credit by End of First Year", 
                            first_year_total_credits.std = "Total Credit by End of First Year (standardized)",
                            second_sem_total_credits = "Total Credits Earned in Spring Semester",
                            second_sem_total_credits.std = "Total Credits Earned in Spring Semester (standardized)")
    }
  }
  
  if (vartype == "binary")
  {
    
    df$variable <- recode(df$variable,
                          enrolled.next.semester = "Enrolled Subsequent Semester",
                          enrolled.next.semester.std = "Enrolled Subsequent Semester (standardized)",
                          student_of_color = "Student of Color", 
                          female = "Female", 
                          FirstGen = "First Generation Student", 
                          transfer = "Transfer Student", 
                          Pell_Grant = "Pell Grant Recipient",
                          Rcv_Fin_Aid = "Received Financial Aid",
                          spring = "Recieved Coaching in the Spring")
  }
 return(df)
}

all.pop.combined.no.missing_all_demos <- cleandf(all.pop.combined.no.missing_all_demos, "binary", 0)
all.pop.combined.no.missing_all_cont <- cleandf(all.pop.combined.no.missing_all_cont, "continuous", 0)
fresh.pop.combined.no.missing_all_demos <- cleandf(fresh.pop.combined.no.missing_all_demos,"binary", 0)
fresh.pop.combined.no.missing_all_cont <- cleandf(fresh.pop.combined.no.missing_all_cont, "continuous", 1)

#remove(list = c("all", "all.pop", "coached", "completed.coached", "con", "fall.17", "fall.18", "fall.19", "hubs", "hubs.no.dup", "in.not.coached", "invites", "invites_2077", 
                #"invites_2081", "invites_2087", "invites_2091", "invites.no.dup", "not.complete.coached", "spr.18", "spr.19", "tmp", "temp", 
                #"coached.in.pop", "coached.not.in.pop", "coached.not.invited", "cohort", "cohorts", 
                #"covars_binary", "covars_cont", "demo", "freshman_cont", "invited", "invited.coached", 
                #"invited.not.coached"))