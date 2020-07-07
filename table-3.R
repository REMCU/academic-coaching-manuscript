library(MatchIt)
library(cem)
library(dplyr)
library(ggplot2)

all.pop.combined.no.missing <- readRDS("data/entire.population.no.missing.rds")
fresh.pop.combined.no.missing <- readRDS("data/freshmen.population.no.missing.RDS")


#############################
# Identify outcome variable 
##############################


outcome <- c('enrolled.next.semester')

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
tr <- which(invited$invited_and_coached==1)
ct <- which(invited$invited_and_coached==0)
ntr <- length(tr)
nct <- length(ct)

# the (unadjusted and therefore likely biased) differences in means is then

mean(invited[[outcome]][tr]) - mean(invited[[outcome]][ct])

# the overall imbalance is given by the L1 statistics

imbalance(group = invited$invited_and_coached, data = invited[cov])


# apply cem 
mat <- cem(treatment = "invited_and_coached", data = invited[c("invited_and_coached",cov)])

#SATT point estimate is 
est <- att(mat, enrolled.next.semester ~ invited_and_coached, data = invited[c("enrolled.next.semester","invited_and_coached",cov)])


##
# Balance
## 

nomiss <- invited %>%  # MatchIt does not allow missing values
  select(outcome, invited_and_coached, all_of(cov)) %>%
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


# get L1 for unmatched sample
imbalance(group = invited$invited_and_coached, data = invited[cov])

# CEM
cem_dta <- match.data(cem_match)
imbalance(group = cem_dta$invited_and_coached, data =cem_dta[cov] )


# Nearest Neighbor 
nn_match <- matchit(invited_and_coached ~ 
                       spring + 
                       student_of_color + 
                       female + 
                       FirstGen + 
                       transfer + 
                       Pell_Grant +
                       Rcv_Fin_Aid +
                       First_Sem_GPA + 
                       prior.sem.gpa + 
                       Credits_at_Entry, method = "nearest", data = nomiss)

nn_dta <- match.data(nn_match)
imbalance(group = nn_dta$invited_and_coached, data =nn_dta[cov] )


# Optimal 
optimal_match <- matchit(invited_and_coached ~ 
                      spring + 
                      student_of_color + 
                      female + 
                      FirstGen + 
                      transfer + 
                      Pell_Grant +
                      Rcv_Fin_Aid +
                      First_Sem_GPA + 
                      prior.sem.gpa + 
                      Credits_at_Entry, method = "optimal", data = nomiss)

om_dta<- match.data(optimal_match)
imbalance(group = om_dta$invited_and_coached, data =om_dta[cov] )
