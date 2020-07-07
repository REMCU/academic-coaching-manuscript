library(MatchIt)
library(cem)
library(optmatch)
library(dplyr)
library(ggplot2)

all.pop.combined.no.missing <- readRDS("data/entire.population.no.missing.rds")
fresh.pop.combined.no.missing <- readRDS("data/freshmen.population.no.missing.RDS")

gpa_cut <- 1 


# Four populations: Invited, High prior GPA, Low prior GPA, Freshman 
invited <- subset(all.pop.combined.no.missing, all.pop.combined.no.missing$invited==1) 
low.gpa <- subset(invited, prior.sem.gpa < gpa_cut)
high.gpa <-  subset(invited, prior.sem.gpa >= gpa_cut)
fresh.invited <- subset(fresh.pop.combined.no.missing, fresh.pop.combined.no.missing$invited==1)


get.matches <- function(outcome, df, treatment, matching)
  #' Creates matched groups
  #' 
  #' @description  Returns the treatment coefficent from linear regression, based on the matched samples.
  #' @param outcome specifies the outcome variable
  #' @param df specifies the sample, such as low gpa, or high gpa, or invited, or freshmen
  #' @param treatment specifies the treatment which is "invited and coached", "invited" or "invited and completed"
  #' @param matching specifies the matching technique, which includes all data, or one of the three maching appraoches
  {
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
    
    nomiss <- df %>%  # MatchIt does not allow missing values
      select(outcome, treatment, one_of(cov)) %>%
      na.omit()
    
    if(matching != "all")
    {
      mod_match <- matchit(nomiss[[treatment]] ~ 
                             spring + 
                             student_of_color + 
                             female + 
                             FirstGen + 
                             transfer + 
                             Pell_Grant +
                             Rcv_Fin_Aid +
                             First_Sem_GPA + 
                             prior.sem.gpa + 
                             Credits_at_Entry, method = matching, data = nomiss)
      
      
      dta_m <- match.data(mod_match)


    }
    
    if(matching == "all")
    {
      dta_m <- df
    }
    
    #print(matching)
    #print(table(dta_m[[treatment]]))
    t_test <- lm(dta_m[[outcome]] ~ dta_m[[treatment]]) 
    return(cbind(nrow(dta_m), summary(t_test)$coefficients))
  }


check_sig <- function(results)
#' Checks signifances level of coefficent from linear regression (t-test)
#' 
#' @description Checks the significants of the pvalue from the coefficients computed in the get.matches function
#' @param results are the results from the get.matches function
{
  p.value <- results[2,5]
  estimate <- paste0(round(results[2,2],2)," (",round(results[2,3],2),")")
  
  results1 <- ifelse(p.value <= .0001, paste(estimate,"***"), estimate)
  results1 <- ifelse(p.value > .0001 & p.value <= .001, paste(results1,"**"), results1)
  results1 <- ifelse(p.value > .001 & p.value <= .05, paste(results1,"*"), results1)
  return(results1)
}


get_pop <- function(sample, outvars, treatment)
#' Main function to facilitate matching and t-tests for all samples and treatments
#' 
#' @description Returns the mean different between two groups, as well as statistical testing for all samples, outcomes, and treatments. 
#' @param sample indicated the sample that is being used, either invited, or invited freshmen.
#' @param is a list of the different outcome variables for the sample
#' @param is the treatment variable, which is either invited, coached, or completed 
{
  all <- NA
  for(outvar in outvars)
    {

      rawcoefs <- get.matches(outvar, sample, treatment, "all")
      nncoefs <- get.matches(outvar, sample, treatment, "nearest")
      optimalcoefs <- get.matches(outvar, sample, treatment, "optimal") #this throws a warning message that seems to be a documented problem 
      cemcoefs <- get.matches(outvar, sample, treatment, "cem")
      
      raw <- check_sig(rawcoefs)
      nn <- check_sig(nncoefs)
      opt <- check_sig(optimalcoefs)
      cem <- check_sig(cemcoefs)
      
      tmp <- cbind(raw, nn, opt, cem)
      tmp <- cbind(outvar,tmp)
      tmp1 <- cbind(rawcoefs[1],nncoefs[1], optimalcoefs[1],cemcoefs[1])
      
      all <- rbind(tmp,all)
      

      
    }
  tmp1 <- cbind("n",rawcoefs[1],nncoefs[1], optimalcoefs[1],cemcoefs[1])
  all <- all %>%  # MatchIt does not allow missing values
    na.omit()
  all <- rbind(all, tmp1)
  
  return(all)
}


orderrows <- function(df, sample)
  {
    if (sample != "fresh")
     {
      reorder <- c(3,2,1,4) 
    }
  
    if (sample == "fresh")
    {
      reorder <- c(4,5,3,2,1,6)
    }
    tmp <- cbind.data.frame(df, reorder)
    tmp <- tmp[order(tmp$reorder),]
    tmp$reorder <- NULL
    return(tmp)
  }

###################
# Not Standardized
###################


all.outvars <- c('enrolled.next.semester', 'invited.term.gpa', 'next.semester.credits')
fresh.outvars <- c('Year2_Retention','invited.term.gpa', 'next.semester.credits','first_year_total_credits','second_sem_total_credits')

results.invited.coached <- get_pop(invited, all.outvars, "invited_and_coached")
results.low.coached <- get_pop(low.gpa, all.outvars, "invited_and_coached")
results.high.coached <- get_pop(high.gpa, all.outvars, "invited_and_coached")
results.fresh.coached <- get_pop(fresh.invited, fresh.outvars, "invited_and_coached")

results.invited.completed <- get_pop(invited, all.outvars, "inv.completed")
results.low.completed <- get_pop(low.gpa, all.outvars, "inv.completed")
results.high.completed <- get_pop(high.gpa, all.outvars, "inv.completed")
results.fresh.completed <- get_pop(fresh.invited, fresh.outvars, "inv.completed")


results.invited.coached <- orderrows(results.invited.coached, "invited")
results.low.coached <- orderrows(results.low.coached, "invited")
results.high.coached <- orderrows(results.high.coached, "invited")
results.fresh.coached <- orderrows(results.fresh.coached, "fresh")
results.invited.completed <- orderrows(results.invited.completed, "invited")
results.low.completed <- orderrows(results.low.completed, "invited")
results.high.completed <- orderrows(results.high.completed, "invited")
results.fresh.completed<- orderrows(results.fresh.completed, "fresh")


####################
# Standardized
####################

all.outvars.std <- c('enrolled.next.semester.std', 'invited.term.gpa.std', 'next.semester.credits.std')
fresh.outvars.std <- c('enrolled.next.semester.std','invited.term.gpa.std', 'next.semester.credits.std','first_year_total_credits.std','second_sem_total_credits.std')

results.invited.coached.std <- get_pop(invited, all.outvars.std, "invited_and_coached")
results.low.coached.std <- get_pop(low.gpa, all.outvars.std, "invited_and_coached")
results.high.coached.std <- get_pop(high.gpa, all.outvars.std, "invited_and_coached")
results.fresh.coached.std <- get_pop(fresh.invited, fresh.outvars.std, "invited_and_coached")

results.invited.completed.std <- get_pop(invited, all.outvars.std, "inv.completed")
results.low.completed.std <- get_pop(low.gpa, all.outvars.std, "inv.completed")
results.high.completed.std <- get_pop(high.gpa, all.outvars.std, "inv.completed")
results.fresh.completed.std <- get_pop(fresh.invited, fresh.outvars.std, "inv.completed")

results.invited.coached.std <- orderrows(results.invited.coached.std, "invited")
results.low.coached.std <- orderrows(results.low.coached.std, "invited")
results.high.coached.std <- orderrows(results.high.coached.std, "invited")
results.fresh.coached.std <- orderrows(results.fresh.coached.std, "fresh")
results.invited.completed.std <- orderrows(results.invited.completed.std, "invited")
results.low.completed.std <- orderrows(results.low.completed.std, "invited")
results.high.competed.std <- orderrows(results.high.completed.std, "invited")
results.fresh.completed.std <- orderrows(results.fresh.completed.std, "fresh")
