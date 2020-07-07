library(MatchIt)
library(cem)
library(dplyr)
library(ggplot2)
library(kableExtra)

#source("6_identify-analytic-samples.R")

all.pop.combined.no.missing <- readRDS("data/entire.population.no.missing.rds")
fresh.pop.combined.no.missing <- readRDS("data/freshmen.population.no.missing.RDS")


cov <- c("spring", 
         "student_of_color", 
         "female",
         "FirstGen", 
         "transfer", 
         "Pell_Grant", 
         "Rcv_Fin_Aid", 
         "First_Sem_GPA", 
         "prior.sem.gpa",
         "Credits_at_Entry")

invited <- subset(all.pop.combined.no.missing, all.pop.combined.no.missing$invited==1) 

nomiss <- invited %>%  # MatchIt does not allow missing values
  select(invited.term.gpa, next.semester.credits, invited_and_coached, st.id, all_of(cov)) %>%
  na.omit()

cem_match <- matchit(invited_and_coached ~ 
                       spring + 
                       student_of_color + 
                       female + 
                       FirstGen + 
                       transfer + 
                       Pell_Grant +
                       Rcv_Fin_Aid +
                       First_Sem_GPA + 
                       prior.sem.gpa + 
                       Credits_at_Entry, method = "cem", data = nomiss)

cem_dta <- match.data(cem_match)

# testing for differences in students who were dropped

cov <- c("spring", 
         "student_of_color", 
         "female",
         "FirstGen", 
         "transfer", 
         "Pell_Grant", 
         "Rcv_Fin_Aid", 
         "First_Sem_GPA", 
         "prior.sem.gpa",
         "Credits_at_Entry")

a <- cem_dta %>%
  group_by(invited_and_coached) %>%
  select(one_of(cov)) %>%
  summarise_all(funs(round(mean(., na.rm = T),2)))


nomatch_invited <- invited[ !invited$st.id %in% unique(cem_dta$st.id) , ]

# list of continuous and binary variables that will be iterated over 

covars_binary <- c(
  "spring",
  "student_of_color",
  "female",
  "FirstGen",
  "transfer",
  "Pell_Grant",
  "Rcv_Fin_Aid")


all <- NA
  
for (demo in covars_binary)
    
  {
    
    print(demo)
    tmp <- NA 
    tmp[1] <- demo
    tmp[2] <- round(mean(cem_dta[[demo]], na.rm=TRUE),2)
    tmp[3] <- round(mean(nomatch_invited[[demo]], na.rm=TRUE),2)

    
    # compare each of the two groups
    
    tmp[3] <-ifelse(t.test(cem_dta[[demo]], nomatch_invited[[demo]])$p.value <=.0001, paste(tmp[3],"***"), tmp[3])  
    
    tmp[3] <-ifelse(t.test(cem_dta[[demo]], nomatch_invited[[demo]])$p.value <=.01 & t.test(cem_dta[[demo]], nomatch_invited[[demo]])$p.value >= .0001, paste(tmp[3],"**"), tmp[3])
    
    tmp[3] <-ifelse(t.test(cem_dta[[demo]], nomatch_invited[[demo]])$p.value <=.05 & t.test(cem_dta[[demo]], nomatch_invited[[demo]])$p.value >= .01, paste(tmp[3],"*"), tmp[3])
    
  
    all <- rbind(all, tmp)
    
}

covars_cont <- c("Credits_at_Entry","First_Sem_GPA","prior.sem.gpa")

for (demo in covars_cont)
  
{
  
  print(demo)
  tmp <- NA 
  tmp[1] <- demo
  tmp[2] <- paste0(round(mean(cem_dta[[demo]], na.rm=TRUE),2), " (",round(sd(cem_dta[[demo]],na.rm = TRUE),2),")")
  tmp[3] <- paste0(round(mean(nomatch_invited[[demo]], na.rm=TRUE),2), " (",round(sd(nomatch_invited[[demo]],na.rm = TRUE),2),")")
  
  
  
  # compare each of the two groups
  
  tmp[3] <-ifelse(t.test(cem_dta[[demo]], nomatch_invited[[demo]])$p.value <=.0001, paste(tmp[3],"***"), tmp[3])  
  
  tmp[3] <-ifelse(t.test(cem_dta[[demo]], nomatch_invited[[demo]])$p.value <=.01 & t.test(cem_dta[[demo]], nomatch_invited[[demo]])$p.value >= .0001, paste(tmp[3],"**"), tmp[3])
  
  tmp[3] <-ifelse(t.test(cem_dta[[demo]], nomatch_invited[[demo]])$p.value <=.05 & t.test(cem_dta[[demo]], nomatch_invited[[demo]])$p.value >= .01, paste(tmp[3],"*"), tmp[3])
  
  
  all <- rbind(all, tmp)
  
}

  colnames(all) <- c("variable",
                     paste0("CEM Matched \n Sample (n=", nrow(cem_dta), ")"),
                     paste0("CEM Dropped \n Sample (n=", nrow(nomatch_invited), ")"))

  assign("matched_covars", all)
  
cleandf.cem <- function(df)
  {
  #' Cleans CEM analysis
  #' 
  #' @description Returns a clean file that is then used in teh Rmarkdown file
  #' @param df indicates the dataframe that will be cleaned
    rownames(df) <- NULL
    df <- as.data.frame(df)
    df <- df[-1,]
    
    df$variable <- recode(df$variable,
                            Credits_at_Entry = "Credits at Entry", 
                            First_Sem_GPA = "First Semester GPA", 
                            prior.sem.gpa = "Prior Semester GPA",
                            student_of_color = "Student of Color", 
                            female = "Female", 
                            FirstGen = "First Generation", 
                            transfer = "Transfer Student", 
                            Pell_Grant = "Pell Grant Recipient",
                            Rcv_Fin_Aid = "Received Financial Aid",
                            spring = "Spring Coaching")
    
    return(df)
  }
  

matched.covars <- cleandf.cem(matched_covars)


