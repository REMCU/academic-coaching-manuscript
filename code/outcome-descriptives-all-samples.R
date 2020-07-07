library(kableExtra)

all.pop.combined.no.missing <- readRDS("data/entire.population.no.missing.rds")
fresh.pop.combined.no.missing <- readRDS("data/freshmen.population.no.missing.RDS")

# list of continuous and binary variables that will be iterated over 

covars_binary <- c(
  "enrolled.next.semester"
)

freshman_cont <-  c(
  "first_year_total_credits",
  "second_sem_total_credits"
)

covars_cont =  c( 
  "invited.term.gpa",
  "next.semester.credits")

#######
## FOR ALL STUDENTS (INCLUDING FRESHMAN) 
#######

cohorts = c("all.pop.combined.no.missing")

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
    

    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.0001, paste(tmp[3],"***"), tmp[3])  
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.01 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .0001, paste(tmp[3],"**"), tmp[3])
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.05 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .01, paste(tmp[3],"*"), tmp[3])
    
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.0001, paste(tmp[5],"***"), tmp[5])
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.01 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .0001, paste(tmp[5],"**"), tmp[5])
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.05 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .01, paste(tmp[5],"*"), tmp[5])
    
    all <- rbind(all, tmp)
    
  }
  colnames(all) <- c("variable",
                     paste0("Non-participants \n n = (", length(subset(temp$st.id, temp$invited_not_coached_EVER==1)), ")"),
                     paste0("Participants \n n = (", length(subset(temp$st.id, temp$invited_and_coached==1)), ")"), 
                     paste0("Non-completers \n n = (", length(subset(temp$st.id, temp$inv.completed == 0 & temp$invited_and_coached==1)), ")"),
                     paste0("Completers \n n = (", length(subset(temp$st.id, temp$inv.completed==1)), ")"))
  
  assign("invited.binary", all)
  
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
    
    
    
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.0001, paste(tmp[3],"***"), tmp[3])  
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.01 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .0001, paste(tmp[3],"**"), tmp[3])
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.05 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .01, paste(tmp[3],"*"), tmp[3])
    
    
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.0001, paste(tmp[5],"***"), tmp[5])
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.01 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .0001, paste(tmp[5],"**"), tmp[5])
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.05 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .01, paste(tmp[5],"*"), tmp[5])

    all <- rbind(all, tmp)
    
  }
  colnames(all) <- c("variable",
                     paste0("Non-participants \n n = (", length(subset(temp$st.id, temp$invited_not_coached_EVER==1)), ")"),
                     paste0("Participants \n n = (", length(subset(temp$st.id, temp$invited_and_coached==1)), ")"), 
                     paste0("Non-completers \n n = (", length(subset(temp$st.id, temp$inv.completed == 0 & temp$invited_and_coached==1)), ")"),
                     paste0("Completeers\n n = (", length(subset(temp$st.id, temp$inv.completed==1)), ")"))
  
  assign("invited.continuous", all)
  
  
  
}


#######
## FRESHMAN ANALYTIC SAMPLE ONLY 
#######

cohorts = c("fresh.pop.combined.no.missing")

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
    
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.0001, paste(tmp[3],"***"), tmp[3])  
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.01 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .0001, paste(tmp[3],"**"), tmp[3])
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.05 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .01, paste(tmp[3],"*"), tmp[3])
    
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.0001, paste(tmp[5],"***"), tmp[5])
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.01 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .0001, paste(tmp[5],"**"), tmp[5])
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.05 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .01, paste(tmp[5],"*"), tmp[5])
    
    all <- rbind(all, tmp)
    
  }
  colnames(all) <- c("variable",
                     paste0("Non-participants \n n = (", length(subset(temp$st.id, temp$invited_not_coached_EVER==1)), ")"),
                     paste0("Participants \n n = (", length(subset(temp$st.id, temp$invited_and_coached==1)), ")"), 
                     paste0("Non-completers \n n = (", length(subset(temp$st.id, temp$inv.completed == 0 & temp$invited_and_coached==1)), ")"),
                     paste0("Completers \n n = (", length(subset(temp$st.id, temp$inv.completed==1)), ")"))
  
  assign("freshman.binary", all)
  
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
    

    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.0001, paste(tmp[3],"***"), tmp[3])  
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.01 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .0001, paste(tmp[3],"**"), tmp[3])
    tmp[3] <-ifelse(t.test(in.not.coached[[demo]], coached[[demo]])$p.value <=.05 & t.test(in.not.coached[[demo]], coached[[demo]])$p.value >= .01, paste(tmp[3],"*"), tmp[3])
    
    
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.0001, paste(tmp[5],"***"), tmp[5])
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.01 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .0001, paste(tmp[5],"**"), tmp[5])
    tmp[5] <-ifelse(t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value <=.05 & t.test(completed.coached[[demo]], not.complete.coached[[demo]])$p.value >= .01, paste(tmp[5],"*"), tmp[5])

    all <- rbind(all, tmp)
    
  }
  colnames(all) <- c("variable",
                     paste0("Non-participants \n n = (", length(subset(temp$st.id, temp$invited_not_coached_EVER==1)), ")"),
                     paste0("Participants \n n = (", length(subset(temp$st.id, temp$invited_and_coached==1)), ")"), 
                     paste0("Non-completers \n n = (", length(subset(temp$st.id, temp$inv.completed == 0 & temp$invited_and_coached==1)), ")"),
                     paste0("Completers \n n = (", length(subset(temp$st.id, temp$inv.completed==1)), ")"))
  
  assign("freshman.continuous", all)
  
  
  
}

###
# Combine all dataframes
###

cleandf <- function(df, vartype, fresh)
{
  rownames(df) <- NULL
  df <- as.data.frame(df)
  df <- df[-1,]
  
  if (vartype == "continuous")
  {
    df$variable <- recode(df$variable, 
                          invited.term.gpa = "Invited Term GPA Mean (SD)",
                          next.semester.credits = "Next Semester Credits Mean (SD)")
                       
    
    if (fresh == 1)
      
    {
      df$variable <- recode(df$variable, 
                            first_year_total_credits = "First Year Credits Mean (SD)",
                            second_sem_total_credits = "Invited Term Credits Mean SD)")
    }
  }
  
  if (vartype == "binary")
  {
    
    df$variable <- recode(df$variable,
                          enrolled.next.semester = "% Enrolled Next Semester")
  }
  return(df)
}

invited.binary<- cleandf(invited.binary, "binary", 0)
invited.continuous <- cleandf(invited.continuous, "continuous", 0)
freshman.binary <- cleandf(freshman.binary,"binary", 1)
freshman.continuous <- cleandf(freshman.continuous, "continuous", 1)


