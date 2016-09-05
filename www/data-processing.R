# Data Prep:
# Use this file to prepare the SPRING CDF data visualizations
#   0 - LOAD all data frames that are needed to transform data
#
# Three main data tasks are accomplished in this file
#
#   1 - MELT data from CSV into a usable R format (prefix "melt")
#   2 - GENERATE base data frames for any graphical rollup (prefix "gen")
#   3 - ROLLUP data for graphs (prefix "roll")

# setwd("~/../Dropbox/github/shiny_projects/shinydashboard-map/") # Development

####  LOAD ####
library(plyr)
library(dplyr)
library(tidyr)

# test_cdf <- read.csv("test_data/ComboStudentAssessment OH.csv")  # CSV for development
status_norms <- readRDS("www/status_norms.Rda")         # NWEA Percentile Norms
growth_norms <- readRDS("www/growth_norms.Rda")         # NWEA Growth Norms
multipliers <- readRDS("www/multipliers.Rda")           # KIPP Foundation Tiered Target Multipliers

#### MELT ####
melt_cdf <- function(combo_cdf) {
  processed_data <- combo_cdf %>%
    filter(TestType == "Survey With Goals") %>%
    transmute(Fall = TestRITScore - FallToSpringObservedGrowth,
              Winter = Fall + FallToWinterObservedGrowth,
              Priorspring = TestRITScore - SpringToSpringObservedGrowth,
              Spring = TestRITScore,
              Subject = as.character(Discipline),
              School = as.character(SchoolName),
              Grade = as.integer(revalue(as.character(Grade), c('K' = '0'))),
              ID = StudentID,
              First = StudentFirstName,
              Last = StudentLastName) %>%
    filter(Grade <= 8) %>%
    gather(Season, RIT, -School, -Grade, -Subject, -ID, -First, -Last,
           na.rm = TRUE, convert = TRUE) %>%
    mutate(prior_spring = ifelse(Season == "Priorspring", 1,0),
           Grade = ifelse(prior_spring == 1, Grade - 1, Grade),
           Season = ifelse(Season == "Priorspring","Spring", Season)) %>%
    left_join(y = status_norms, by = c("Grade","Subject","RIT","Season")) 
}


####  GENERATE  ####
# Tidy Status DF: Student/Subject/Season
gen_status <- function(base){
  status <- base %>% 
    mutate(Grade = ifelse(prior_spring == 1, Grade + 1, Grade),
           Season = ifelse(prior_spring == 1, "Prior Spring",Season),
           Quartile = ceiling(NPR/25)) %>%
    group_by(Grade, Subject, Season) %>%
    mutate(n_tested = sum(n())) %>%
    filter(n_tested >= 15)
}

# Tidy Growth DF: Student/Subject/Growth Season
gen_growth <- function(molten_status) {
  growth <- molten_status %>%
    filter(Season == "Fall" | prior_spring == 1) %>%
    left_join(y = growth_norms %>% filter(End_Season == "Spring") %>% select(Subject, Grade, Start_Season, RIT, Growth_Goal),
              by = c("Subject","Grade","Season"="Start_Season","RIT")) %>%
    mutate(Grade = ifelse(prior_spring == 1, Grade + 1, Grade)) %>%
    left_join(y = molten_status %>% filter(Season == "Spring"), by = c("Subject","School","Grade","ID")) %>%
    transmute(Subject = Subject,School = School,Grade = Grade,Id = ID,
              Growth_Season = ifelse(Season.x == "Fall", "Fall-to-Spring",
                                     ifelse(Season.x == "Spring","Spring-to-Spring",Season.x)),
              Start_RIT = RIT.x, End_RIT = RIT.y, 
              Start_Q = ceiling(NPR.x/25),
              End_Q = ceiling(NPR.y/25),
              Typical_Goal = Growth_Goal,
              Start_Season = Season.x,
              End_Season = Season.y) %>% 
    left_join(y = multipliers, by = c("Grade","Start_Q")) %>%
    mutate(Tiered_Goal = ceiling(Typical_Goal * multiplier),
           Growth = End_RIT - Start_RIT,
           Met_Typical = ifelse(Growth >= Typical_Goal, 1, 0),
           Met_Tiered = ifelse(Growth >= Tiered_Goal, 1, 0),
           Typ_not_Tiered = ifelse(Met_Typical == 1 & Met_Tiered == 0, 1, 0)) %>%
    group_by(Grade, Subject, Growth_Season) %>%
    mutate(n_tested = sum(n())) %>%
    filter(n_tested >= 15)
}


#### ROLLUP ####
# "Overall" Page: % Top Q for oldest grade
rollup_status_overall <- function(status_df) {
  grade_level <-  status_df %>%
    filter(Season == "Spring") %>%
    group_by(Subject, School, Grade, Quartile) %>%
    summarise(n = n()) %>%
    group_by(Subject, School, Grade) %>%
    mutate(total = sum(n)) %>%
    filter(total >=15,
           Quartile >= 3) %>%
    mutate(percent = n / total,
           label_loc = cumsum(percent) - (.5 * percent),
           label = paste0(as.character(round(percent,2)*100),"%")) %>%
    group_by(Subject, School) %>%
    filter(Grade == max(Grade))
}

# "Overall" Page: % of students meeting tiered targets
rollup_growth_overall <- function(growth_df){
  growth_df %>%
  filter((Grade %in% c(0,1,2,5) & Growth_Season == "Fall-to-Spring" ) | 
           (Grade %in% c(3,4,6,7,8)) & Growth_Season == "Spring-to-Spring") %>%
  group_by(Subject, School) %>%
  summarise(Tiered = mean(Met_Tiered),
            Typ_not = mean(Typ_not_Tiered),
            n = n()) %>%
  gather(growth, total, -n, -School,-Subject, convert = TRUE) %>%
  ungroup() %>%
  mutate(total_label = ifelse(abs(total) > .1, paste(as.character(round(abs(total),2)*100),"%"),"")) %>%
  group_by(School, Subject) %>%
  mutate(label_loc = cumsum(total) - .5 * total) %>%
  filter(n >= 15)
}

# "Growth" Page: % of students meeting targets
rollup_growth_targets <- function(growth_df) {
  growth_df %>%
    group_by(School, Subject, Grade, Growth_Season) %>%
    summarise(Tiered = mean(Met_Tiered),
              Typ_not = mean(Typ_not_Tiered),
              n = n()) %>%
    gather(growth, total, -n, -School,-Subject,-Grade,-Growth_Season, convert = TRUE) %>%
    ungroup() %>%
    mutate(total_label = ifelse(abs(total) > .1, paste(as.character(round(abs(total),2)*100),"%"),"")) %>%
    group_by(School, Subject, Grade, Growth_Season) %>%
    mutate(label_loc = cumsum(total) - .5 * total) %>%
    filter(n >= 15)
}

# "Growth by Starting Q" Page: % of students meeting targets by starting quartile
rollup_growth_targets_by_q <- function(growth_df){ 
  growth_df %>%
    group_by(Subject, School, Grade, Growth_Season, Start_Q) %>%
    summarise(Typical = mean(Met_Typical),
              N = n()) %>%
    group_by(Subject, School, Grade, Growth_Season) %>%
    mutate(N = sum(N)) %>%
    filter(N >= 15) %>%
    gather(growth_type, value, -Subject, -School, -Grade, -Start_Q, -Growth_Season, -N) %>%
    group_by(Subject, School, Grade, Growth_Season, Start_Q) %>%
    mutate(label_loc = value + .05) %>%
    ungroup() %>%
    mutate(label = ifelse(abs(value)>=.1, paste0(round(abs(value),2)*100, "%"),""))
}

# "Quartile" Page: Shift in quartile distribution from season to season
rollup_growth_quartile <- function(growth_df) {
  growth_df %>%
  select(School, Grade, Subject, Id, Growth_Season, Start_Q, End_Q, Start_Season, End_Season) %>%
  mutate(Start_Season = ifelse(Growth_Season == "Spring-to-Spring", "Prior Spring", Start_Season)) %>%
  unite(Start, Start_Q, Start_Season, sep = "_") %>%
  unite(End, End_Q, End_Season, sep = "_") %>%
  gather(Season, value, -Subject, -School, -Grade, -Id, -Growth_Season) %>%
  separate(col = value, sep = "_", into = c("Quartile", "display"), convert = TRUE) %>%
  group_by(School, Grade, Subject, Growth_Season, Quartile, display) %>%
  summarise(total = n()) %>%
  group_by(School, Grade, Subject, Growth_Season, display) %>%
  mutate(total = total / sum(total)) %>%
  mutate(total = ifelse(Quartile <= 2, total * -1 , total)) %>%
  ungroup() %>%
  mutate(display = paste0("Grade ", as.character(Grade), "<br>", display)) %>%
  spread(Quartile, total)
}

# "Quartile by Grade" page: Sankey chart of 
rollup_growth_quartile_sankey <- function(growth_df) {
  growth_df %>%
  filter(Grade <= 8) %>%
  group_by(Subject, School, Grade, Growth_Season, Start_Q, End_Q) %>%
  summarise(Total = n()) %>%
  group_by(Subject, School, Grade, Growth_Season) %>%
  mutate(cumtotal = sum(Total),
         rank = Start_Q + End_Q) %>%
  ungroup() %>%   
  filter(cumtotal >= 15) %>%
  mutate(Start_Q = paste0("Q", as.character(Start_Q), " Start"),
         End_Q = paste0("Q", as.character(End_Q), " End")) %>%
  arrange(desc(rank), desc(Start_Q), desc(End_Q))
}

#### TABLES ####
export_molten <- function(status_df) {
  status_df %>%
  select(School, Grade, Subject, ID, First, Last, Season, RIT, NPR, Quartile)
}

export_growth <- function(status_df) {
  status_df %>%
  ungroup() %>%
  select(Subject, School, Grade, ID, First, Last, Season, RIT, NPR, Quartile) %>%
  unite(instance, -School, -Grade, -ID, -Season, -Subject, -First, -Last) %>%
  spread(Season, instance, convert = TRUE) %>%
  separate(Fall, c("F_RIT","F_NPR","F_Q"), sep = "_", convert = TRUE,remove = FALSE) %>%
  separate(Spring, c("S_RIT","S_NPR","S_Q"), sep = "_", convert = TRUE,remove = FALSE) %>%   
  separate(`Prior Spring`, c("PS_RIT","PS_NPR","PS_Q"), sep = "_", convert = TRUE,remove = FALSE) %>%
  select(School, Grade, Subject, ID, First, Last, PS_RIT, PS_NPR, F_RIT, F_NPR, S_RIT, S_NPR)  
}

multiplier_table <- function() {
  k4    <- multipliers %>% 
           arrange(Grade, Start_Q) %>% 
           filter(Grade == 4) %>% 
           transmute(`Starting Quartile` = Start_Q,
                     `K-4th` = multiplier)
  
  final <- multipliers %>% 
           arrange(Grade, Start_Q) %>% 
           filter(Grade == 3) %>% 
           transmute(`Starting Quartile` = Start_Q,
                     `K-3rd` = multiplier) %>%
           left_join(k4)
  return(final)
}

