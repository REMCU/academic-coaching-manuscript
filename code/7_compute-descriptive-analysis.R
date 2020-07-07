library(kableExtra)
#source("6_identify-analytic-samples.R")

all.pop.combined.no.missing <- readRDS("data/entire.population.no.missing.rds")
fresh.pop.combined.no.missing <- readRDS("data/freshmen.population.no.missing.rds")

# list of continuous and binary variables that will be iterated over 

covars_binary <- c(
  "enrolled.next.semester",
  "enrolled.next.semester.std",
  "spring",
  "student_of_color",
  "female",
  "FirstGen",
  "transfer",
  "Pell_Grant",
  "Rcv_Fin_Aid"
)

freshman_cont <-  c(
  "first_year_total_credits",
  "first_year_total_credits.std",
  "second_sem_total_credits",
  "second_sem_total_credits.std"
)

covars_cont =  c( 
  "invited.term.gpa",
  "invited.term.gpa.std",
  "next.semester.credits",
  "next.semester.credits.std",
  "Credits_at_Entry",
  "First_Sem_GPA",
  "prior.sem.gpa",
  "coaching.sessions",
  "Credits_at_Entry")

#######
## FOR ALL STUDENTS (INCLUDING FRESHMAN) 
#######

cohorts = c("all.pop.combined.no.missing")

#cohorts = c("all.pop.combined", "all.pop.combined.no.missing", "all.pop.combined.missing")

for (cohort in cohorts)
{
  temp <- eval(parse(text=cohort))
  coached <- subset(temp, temp$invited_and_coached==1)
  in.not.coached <-  subset(temp, temp$invited_not_coached_EVER==1)
  completed.coached <- subset(temp, temp$inv.completed == 1)
  not.complete.coached <- subset(temp, temp$inv.completed == 0 & temp$invited_and_coached==1)
  all <- NA
  
  for (demo in covars_binary)
    
  {
    
    print(demo)
    tmp <- NA 
    tmp[1] <- demo
    tmp[2] <- round(mean(in.not.coached[[demo]], na.rm=TRUE),2)
    tmp[3] <- round(mean(coached[[demo]], na.rm=TRUE),2)
    tmp[4] <- round(mean(not.complete.coached[[demo]], na.rm=TRUE),2)
    tmp[5] <- round(mean(completed.coached[[demo]], na.rm=TRUE),2)
    
    
    # compare each of the two groups
    
    tmp[2] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.001, paste(tmp[2],"***"), tmp[2])
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.001, paste(tmp[3],"***"), tmp[3])  
    
    tmp[2] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.01 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .001, paste(tmp[2],"**"), tmp[2])
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.01 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .001, paste(tmp[3],"**"), tmp[3])
    
    tmp[2] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.05 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .01, paste(tmp[2],"*"), tmp[2])
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.05 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .01, paste(tmp[3],"*"), tmp[3])
    
    tmp[4] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.001, paste(tmp[4],"***"), tmp[4])
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.001, paste(tmp[5],"***"), tmp[5])
    
    tmp[4] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.01 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .001, paste(tmp[4],"**"), tmp[4])
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.01 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .001, paste(tmp[5],"**"), tmp[5])
    
    tmp[4] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.05 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .01, paste(tmp[4],"*"), tmp[4])
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.05 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .01, paste(tmp[5],"*"), tmp[5])
    
    all <- rbind(all, tmp)
    
  }
  colnames(all) <- c("variable",
                     paste0("invited but never coached \n n = (", length(subset(temp$st.id, temp$invited_not_coached_EVER==1)), ")"),
                     paste0("invited and coached \n n = (", length(subset(temp$st.id, temp$invited_and_coached==1)), ")"), 
                     paste0("not completed \n n = (", length(subset(temp$st.id, temp$inv.completed == 0 & temp$invited_and_coached==1)), ")"),
                     paste0("completed \n n = (", length(subset(temp$st.id, temp$inv.completed==1)), ")"))
  
  assign(paste0(cohort,"_all_demos"), all)
  
}



for (cohort in cohorts)
{
  temp <- eval(parse(text=cohort))
  coached <- subset(temp, temp$invited_and_coached==1)
  in.not.coached <-  subset(temp, temp$invited_not_coached_EVER==1)
  completed.coached <- subset(temp, temp$inv.completed == 1)
  not.complete.coached <- subset(temp, temp$inv.completed == 0 & temp$invited_and_coached==1)
  all <- NA
  for (demo in covars_cont)
  {
    
    tmp <- NA 
    tmp[1] <- demo
    tmp[2] <- paste0(round(mean(in.not.coached[[demo]], na.rm=TRUE),2), " (",round(sd(in.not.coached[[demo]],na.rm = TRUE),2),")")
    tmp[3] <- paste0(round(mean(coached[[demo]], na.rm=TRUE),2), " (",round(sd(coached[[demo]],na.rm = TRUE),2),")")
    tmp[4] <- paste0(round(mean(not.complete.coached[[demo]], na.rm=TRUE),2), " (",round(sd(not.complete.coached[[demo]],na.rm = TRUE),2),")")
    tmp[5] <- paste0(round(mean(completed.coached[[demo]], na.rm=TRUE),2)," (",round(sd(completed.coached[[demo]],na.rm = TRUE),2),")")
    
    
    
    tmp[2] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.001, paste(tmp[2],"***"), tmp[2])
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.001, paste(tmp[3],"***"), tmp[3])  
    
    tmp[2] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.01 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .001, paste(tmp[2],"**"), tmp[2])
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.01 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .001, paste(tmp[3],"**"), tmp[3])
    
    tmp[2] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.05 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .01, paste(tmp[2],"*"), tmp[2])
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.05 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .01, paste(tmp[3],"*"), tmp[3])
    
    
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.001, paste(tmp[5],"***"), tmp[5])
    tmp[4] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.001, paste(tmp[4],"***"), tmp[4])
    
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.01 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .001, paste(tmp[5],"**"), tmp[5])
    tmp[4] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.01 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .001, paste(tmp[4],"**"), tmp[4])
    
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.05 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .01, paste(tmp[5],"*"), tmp[5])
    tmp[4] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.05 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .01, paste(tmp[4],"*"), tmp[4])
    
    all <- rbind(all, tmp)
    
  }
  colnames(all) <- c("variable",
                     paste0("invited but never coached \n n = (", length(subset(temp$st.id, temp$invited_not_coached_EVER==1)), ")"),
                     paste0("invited and coached \n n = (", length(subset(temp$st.id, temp$invited_and_coached==1)), ")"), 
                     paste0("not completed \n n = (", length(subset(temp$st.id, temp$inv.completed == 0 & temp$invited_and_coached==1)), ")"),
                     paste0("completed \n n = (", length(subset(temp$st.id, temp$inv.completed==1)), ")"))
  
  assign(paste0(cohort,"_all_cont"), all)
  
  
  
}


# missing value anlaysis
for (cohort in cohorts)
{
  all <- NA
  temp <- eval(parse(text=cohort))
  coached <- subset(temp, temp$invited_and_coached==1)
  in.not.coached <-  subset(temp, temp$invited_not_coached_EVER==1)
  completed.coached <- subset(temp, temp$inv.completed == 1 & temp$invited_and_coached==1)
  not.complete.coached <- subset(temp, temp$inv.completed == 0 & temp$invited_and_coached==1)
  
  for (demo in covars_binary)
  {
    tmp <- NA 
    tmp[1] <- demo
    tmp[2] <- ifelse(is.na(table(in.not.coached[[demo]], useNA = "ifany")[3]), "No missing", table(in.not.coached[[demo]], useNA = "ifany")[3])
    tmp[3] <- ifelse(is.na(table(coached[[demo]], useNA = "ifany")[3]), "No missing", table(coached[[demo]], useNA = "ifany")[3])
    tmp[4] <- ifelse(is.na(table(not.complete.coached[[demo]], useNA = "ifany")[3]), "No missing", table(not.complete.coached[[demo]], useNA = "ifany")[3])
    tmp[5] <- ifelse(is.na(table(completed.coached[[demo]], useNA = "ifany")[3]), "No missing", table(completed.coached[[demo]], useNA = "ifany")[3])
    
    
    
    all <- rbind(all, tmp)
    
  }  
  colnames(all) <- c("variable", 
                     paste0("invited but never coached \n n = (", length(subset(temp$st.id, temp$invited_not_coached_EVER==1)), ")"),
                     paste0("invited and coached \n n = (", length(subset(temp$st.id, temp$invited_and_coached==1)), ")"), 
                     paste0("not completed \n n = (", length(subset(temp$st.id, temp$inv.completed == 0 & temp$invited_and_coached==1)), ")"),
                     paste0("completed \n n = (", length(subset(temp$st.id, temp$inv.completed==1 & temp$invited_and_coached==1)), ")"))
  
  assign(paste0(cohort,"_count_missing"), all)
}


# missing value anlaysis
for (cohort in cohorts)
{
  all <- NA
  temp <- eval(parse(text=cohort))
  coached <- subset(temp, temp$invited_and_coached==1)
  in.not.coached <-  subset(temp, temp$invited_not_coached_EVER ==1)
  completed.coached <- subset(temp, temp$inv.completed == 1 & temp$invited_and_coached==1)
  not.complete.coached <- subset(temp, temp$inv.completed == 0 & temp$invited_and_coached==1)
  print(cohort)
  for (demo in covars_cont)
  {
    tmp <- NA 
    tmp[1] <- demo
    tmp[2] <- nrow(subset(in.not.coached, is.na(in.not.coached[[demo]])))
    tmp[3] <- nrow(subset(coached, is.na(coached[[demo]])))
    tmp[4] <- nrow(subset(not.complete.coached, is.na(not.complete.coached[[demo]])))
    tmp[5] <- nrow(subset(completed.coached, is.na(completed.coached[[demo]])))
    
    
    all <- rbind(all, tmp)
  }  
  
  colnames(all) <- c("variable",  
                     paste0("invited but never coached \n n = (", length(subset(temp$st.id, temp$invited_not_coached_EVER==1)), ")"),
                     paste0("invited and coached \n n = (", length(subset(temp$st.id, temp$invited_and_coached==1)), ")"), 
                     paste0("not completed \n n = (", length(subset(temp$st.id, temp$inv.completed == 0 & temp$invited_and_coached==1)), ")"),
                     paste0("completed \n n = (", length(subset(temp$st.id, temp$inv.completed==1 & temp$invited_and_coached==1)), ")"))
  
  assign(paste0(cohort,"_count_cont_missing"), all)
}


#######
## FRESHMAN ANALYTIC SAMPLE ONLY 
#######

cohorts = c("fresh.pop.combined.no.missing")

#cohorts = c("fresh.pop.combined", "fresh.pop.combined.no.missing", "fresh.pop.combined.missing")

covars_cont <- c(freshman_cont, covars_cont)
covars_binary <- covars_binary[!covars_binary %in% "spring"]

for (cohort in cohorts)
{
  temp <- eval(parse(text=cohort))
  coached <- subset(temp, temp$invited_and_coached==1)
  in.not.coached <-  subset(temp, temp$invited_not_coached_EVER==1)
  completed.coached <- subset(temp, temp$inv.completed == 1)
  not.complete.coached <- subset(temp, temp$inv.completed == 0 & temp$invited_and_coached==1)
  all <- NA
  
  for (demo in covars_binary)
    
  {
    
    print(demo)
    tmp <- NA 
    tmp[1] <- demo
    tmp[2] <- round(mean(in.not.coached[[demo]], na.rm=TRUE),2)
    tmp[3] <- round(mean(coached[[demo]], na.rm=TRUE),2)
    tmp[4] <- round(mean(not.complete.coached[[demo]], na.rm=TRUE),2)
    tmp[5] <- round(mean(completed.coached[[demo]], na.rm=TRUE),2)
    
    
    # compare each of the two groups
    
    tmp[2] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.001, paste(tmp[2],"***"), tmp[2])
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.001, paste(tmp[3],"***"), tmp[3])  
    
    tmp[2] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.01 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .001, paste(tmp[2],"**"), tmp[2])
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.01 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .001, paste(tmp[3],"**"), tmp[3])
    
    tmp[2] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.05 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .01, paste(tmp[2],"*"), tmp[2])
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.05 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .01, paste(tmp[3],"*"), tmp[3])
    
    tmp[4] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.001, paste(tmp[4],"***"), tmp[4])
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.001, paste(tmp[5],"***"), tmp[5])
    
    tmp[4] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.01 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .001, paste(tmp[4],"**"), tmp[4])
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.01 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .001, paste(tmp[5],"**"), tmp[5])
    
    tmp[4] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.05 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .01, paste(tmp[4],"*"), tmp[4])
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.05 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .01, paste(tmp[5],"*"), tmp[5])
    
    all <- rbind(all, tmp)
    
  }
  colnames(all) <- c("variable",
                     paste0("invited but never coached \n n = (", length(subset(temp$st.id, temp$invited_not_coached_EVER==1)), ")"),
                     paste0("invited and coached \n n = (", length(subset(temp$st.id, temp$invited_and_coached==1)), ")"), 
                     paste0("not completed \n n = (", length(subset(temp$st.id, temp$inv.completed == 0 & temp$invited_and_coached==1)), ")"),
                     paste0("completed \n n = (", length(subset(temp$st.id, temp$inv.completed==1)), ")"))
  
  assign(paste0(cohort,"_all_demos"), all)
  
}



for (cohort in cohorts)
{
  temp <- eval(parse(text=cohort))
  coached <- subset(temp, temp$invited_and_coached==1)
  in.not.coached <-  subset(temp, temp$invited_not_coached_EVER==1)
  completed.coached <- subset(temp, temp$inv.completed == 1)
  not.complete.coached <- subset(temp, temp$inv.completed == 0 & temp$invited_and_coached==1)
  all <- NA
  for (demo in covars_cont)
  {
    
    tmp <- NA 
    tmp[1] <- demo
    tmp[2] <- paste0(round(mean(in.not.coached[[demo]], na.rm=TRUE),2), " (",round(sd(in.not.coached[[demo]],na.rm = TRUE),2),")")
    tmp[3] <- paste0(round(mean(coached[[demo]], na.rm=TRUE),2), " (",round(sd(coached[[demo]],na.rm = TRUE),2),")")
    tmp[4] <- paste0(round(mean(not.complete.coached[[demo]], na.rm=TRUE),2), " (",round(sd(not.complete.coached[[demo]],na.rm = TRUE),2),")")
    tmp[5] <- paste0(round(mean(completed.coached[[demo]], na.rm=TRUE),2)," (",round(sd(completed.coached[[demo]],na.rm = TRUE),2),")")
    
    
    
    tmp[2] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.001, paste(tmp[2],"***"), tmp[2])
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.001, paste(tmp[3],"***"), tmp[3])  
    
    tmp[2] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.01 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .001, paste(tmp[2],"**"), tmp[2])
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.01 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .001, paste(tmp[3],"**"), tmp[3])
    
    tmp[2] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.05 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .01, paste(tmp[2],"*"), tmp[2])
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.05 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .01, paste(tmp[3],"*"), tmp[3])
    
    
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.001, paste(tmp[5],"***"), tmp[5])
    tmp[4] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.001, paste(tmp[4],"***"), tmp[4])
    
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.01 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .001, paste(tmp[5],"**"), tmp[5])
    tmp[4] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.01 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .001, paste(tmp[4],"**"), tmp[4])
    
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.05 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .01, paste(tmp[5],"*"), tmp[5])
    tmp[4] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.05 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .01, paste(tmp[4],"*"), tmp[4])
    
    all <- rbind(all, tmp)
    
  }
  colnames(all) <- c("variable",
                     paste0("invited but never coached \n n = (", length(subset(temp$st.id, temp$invited_not_coached_EVER==1)), ")"),
                     paste0("invited and coached \n n = (", length(subset(temp$st.id, temp$invited_and_coached==1)), ")"), 
                     paste0("not completed \n n = (", length(subset(temp$st.id, temp$inv.completed == 0 & temp$invited_and_coached==1)), ")"),
                     paste0("completed \n n = (", length(subset(temp$st.id, temp$inv.completed==1)), ")"))
  
  assign(paste0(cohort,"_all_cont"), all)
  
  
  
}
# missing value anlaysis
for (cohort in cohorts)
{
  all <- NA
  temp <- eval(parse(text=cohort))
  coached <- subset(temp, temp$invited_and_coached==1)
  in.not.coached <-  subset(temp, temp$invited_not_coached_EVER==1)
  completed.coached <- subset(temp, temp$inv.completed == 1 & temp$invited_and_coached==1)
  not.complete.coached <- subset(temp, temp$inv.completed == 0 & temp$invited_and_coached==1)
  print(cohort)
  for (demo in covars_binary)
  {
    tmp <- NA 
    tmp[1] <- demo
    tmp[2] <- ifelse(is.na(table(in.not.coached[[demo]], useNA = "ifany")[3]), "No missing", table(in.not.coached[[demo]], useNA = "ifany")[3])
    tmp[3] <- ifelse(is.na(table(coached[[demo]], useNA = "ifany")[3]), "No missing", table(coached[[demo]], useNA = "ifany")[3])
    tmp[4] <- ifelse(is.na(table(not.complete.coached[[demo]], useNA = "ifany")[3]), "No missing", table(not.complete.coached[[demo]], useNA = "ifany")[3])
    tmp[5] <- ifelse(is.na(table(completed.coached[[demo]], useNA = "ifany")[3]), "No missing", table(completed.coached[[demo]], useNA = "ifany")[3])
    
    
    all <- rbind(all, tmp)
    
  }  
  colnames(all) <- c("variable", 
                     paste0("invited but never coached \n n = (", length(subset(temp$st.id, temp$invited_not_coached_EVER==1)), ")"),
                     paste0("invited and coached \n n = (", length(subset(temp$st.id, temp$invited_and_coached==1)), ")"), 
                     paste0("not completed \n n = (", length(subset(temp$st.id, temp$inv.completed == 0 & temp$invited_and_coached==1)), ")"),
                     paste0("completed \n n = (", length(subset(temp$st.id, temp$inv.completed==1 & temp$invited_and_coached==1)), ")"))
  
  assign(paste0(cohort,"_count_missing"), all)
}

for (cohort in cohorts)
{
  all <- NA
  temp <- eval(parse(text=cohort))
  coached <- subset(temp, temp$invited_and_coached==1)
  in.not.coached <-  subset(temp, temp$invited_not_coached_EVER ==1)
  completed.coached <- subset(temp, temp$inv.completed == 1 & temp$invited_and_coached==1)
  not.complete.coached <- subset(temp, temp$inv.completed == 0 & temp$invited_and_coached==1)
  for (demo in covars_cont)
  {
    tmp <- NA 
    tmp[1] <- demo
    tmp[2] <- nrow(subset(in.not.coached, is.na(in.not.coached[[demo]])))
    tmp[3] <- nrow(subset(coached, is.na(coached[[demo]])))
    tmp[4] <- nrow(subset(not.complete.coached, is.na(not.complete.coached[[demo]])))
    tmp[5] <- nrow(subset(completed.coached, is.na(completed.coached[[demo]])))
    
    
    all <- rbind(all, tmp)
  }  
  
  colnames(all) <- c("variable",  
                     paste0("invited but never coached \n n = (", length(subset(temp$st.id, temp$invited_not_coached_EVER==1)), ")"),
                     paste0("invited and coached \n n = (", length(subset(temp$st.id, temp$invited_and_coached==1)), ")"), 
                     paste0("not completed \n n = (", length(subset(temp$st.id, temp$inv.completed == 0 & temp$invited_and_coached==1)), ")"),
                     paste0("completed \n n = (", length(subset(temp$st.id, temp$inv.completed==1 & temp$invited_and_coached==1)), ")"))
  
  assign(paste0(cohort,"_count_cont_missing"), all)
}