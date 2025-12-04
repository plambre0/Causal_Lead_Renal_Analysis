library(dagitty)
library(ggdag)
library(haven)
library(readr)
library(dplyr)
library(vcd)
library(ggplot2)
library(naniar)
library(mice)
library(FactoMineR)
library(yacca)
library(ltmle)

set.seed(1643)

#specify DAG for Causal Hypothesis
#Lead Exposure -> Renal Damage -> Death
dag <- dagitty("dag{Lead_Exposure -> Renal_Damage -> Death}")
tidy_dagitty(dag)
ggdag(dag, node = FALSE, text_col = "black", text_size = 3) + theme_dag_blank()

#Uses 2005-2006 NHANES Survey Data and 2019 Mortality Data
#Albumin & Creatinine - Urine
ALB_CR_D <- read_xpt("C:/Users/Paolo/Desktop/Personal Research/lead_kidneys/nhanes_data/ALB_CR_D.xpt")
BIOPRO_D <- read_xpt("C:/Users/Paolo/Desktop/Personal Research/lead_kidneys/nhanes_data/BIOPRO_D.xpt")
#Alcohol Use
#ALQ_D <- read_xpt("C:/Users/Paolo/Desktop/Personal Research/lead_kidneys/nhanes_data/ALQ_D.xpt")
#Body Measures
BMX_D <- read_xpt("C:/Users/Paolo/Desktop/Personal Research/lead_kidneys/nhanes_data/BMX_D.xpt")
#Blood Pressure
#BPX_D <- read_xpt("C:/Users/Paolo/Desktop/Personal Research/lead_kidneys/nhanes_data/BPX_D.xpt")
#C-Reactive Protein (CRP)
CRP_D <- read_xpt("C:/Users/Paolo/Desktop/Personal Research/lead_kidneys/nhanes_data/CRP_D.xpt")
#Demographic Data
DEMO_D <- read_xpt("C:/Users/Paolo/Desktop/Personal Research/lead_kidneys/nhanes_data/DEMO_D.xpt")
#Diabetes
DIQ_D <- read_xpt("C:/Users/Paolo/Desktop/Personal Research/lead_kidneys/nhanes_data/DIQ_D.xpt")
#Mental Health - Depression Screener	
DPQ_D <- read_xpt("C:/Users/Paolo/Desktop/Personal Research/lead_kidneys/nhanes_data/DPQ_D.xpt")
#Current Health Status
HSQ_D <- read_xpt("C:/Users/Paolo/Desktop/Personal Research/lead_kidneys/nhanes_data/HSQ_D.xpt")
#Kidney Conditions - Urology	
#KIQ_U_D <- read_xpt("C:/Users/Paolo/Desktop/Personal Research/lead_kidneys/nhanes_data/KIQ_U_D.xpt")
#Medical Conditions	
MCQ_D <- read_xpt("C:/Users/Paolo/Desktop/Personal Research/lead_kidneys/nhanes_data/MCQ_D.xpt")
#Physical Activity Monitor	
#paxraw_d <- read_xpt("C:/Users/Paolo/Desktop/Personal Research/lead_kidneys/nhanes_data/paxraw_d.xpt")
#Cadmium, Lead, & Total Mercury - Blood	
PBCD_D <- read_xpt("C:/Users/Paolo/Desktop/Personal Research/lead_kidneys/nhanes_data/PBCD_D.xpt")
#Smoking - Recent Tobacco Use	
SMQRTU_D <- read_xpt("C:/Users/Paolo/Desktop/Personal Research/lead_kidneys/nhanes_data/SMQRTU_D.xpt")
#Weight History
#WHQ_D <- read_xpt("C:/Users/Paolo/Desktop/Personal Research/lead_kidneys/nhanes_data/WHQ_D.xpt")
#Mortality Data 2019
MORT <- read_fwf("C:/Users/Paolo/Desktop/Personal Research/lead_kidneys/nhanes_data/NHANES_2005_2006_MORT_2019_PUBLIC.dat",
                 col_types = "ciiiiiiidd",
                 fwf_cols(publicid = c(1,14),
                          eligstat = c(15,15),
                          mortstat = c(16,16),
                          ucod_leading = c(17,19),
                          diabetes = c(20,20),
                          hyperten = c(21,21),
                          dodqtr = c(22,22),
                          dodyear = c(23,26),
                          wgt_new = c(27,34),
                          sa_wgt_new = c(35,42)
                 ),
                 na = c("", ".")
)
names(MORT)[names(MORT) == "publicid"] <- "SEQN"
MORT$SEQN <- as.double(MORT$SEQN)


#assemble dataframe
DF0 <- BIOPRO_D[, c("SEQN", "LBXSCR")]
Df1 <- ALB_CR_D[, c("SEQN", "URXUMA", "URXUCR")]
Df2 <- BMX_D[, c("SEQN", "BMXBMI", "BMXWAIST")]
Df3 <- CRP_D[, c("SEQN", "LBXCRP")]
Df4 <- DEMO_D[, c("SEQN", "RIDAGEYR", "RIAGENDR", "RIDRETH1", "INDFMPIR", "DMDEDUC2", "WTMEC2YR", "SDMVPSU", "SDMVSTRA")]
Df5 <- DIQ_D[, c("SEQN", "DIQ010", "DIQ160")]
Df6 <- DPQ_D[, c("SEQN", "DPQ020")]
Df7 <- HSQ_D[, c("SEQN", "HSD010")]
Df8 <- MCQ_D[, c("SEQN", "MCQ220")]
Df9 <- PBCD_D[, c("SEQN", "LBXBPB")]
Df10 <- SMQRTU_D[, c("SEQN", "SMQ680")]
Df11 <- MORT[, c("SEQN", "mortstat", "ucod_leading")]

cohort <-
  DF0 %>% 
  full_join(Df1, by = "SEQN") %>% 
  full_join(Df2, by = "SEQN") %>% 
  full_join(Df3, by = "SEQN") %>% 
  full_join(Df4, by = "SEQN") %>% 
  full_join(Df5, by = "SEQN") %>% 
  full_join(Df6, by = "SEQN") %>% 
  full_join(Df7, by = "SEQN") %>% 
  full_join(Df8, by = "SEQN") %>% 
  full_join(Df9, by = "SEQN") %>% 
  full_join(Df10, by = "SEQN") %>%
  full_join(Df11, by = "SEQN")

#rename for interpretability
sapply(cohort, attributes)
names(cohort) <- c("id", "creatinine_serum", "albumin_urine", "creatinine_urine", 
                   "bmi", "waist_circ", "c-reactive_prot", "age_at_screen", 
                   "gender", "race", "family_pir", "education_lev", 
                   "exam_weight", "psu", "masked_var_psuedo_strat", "diabetes", 
                   "prediabetes", "depression", "gen_health", "cancer", "lead", 
                   "tobacco", "dead_2019", "death_cause")
cohort$death_cause <- ifelse(is.na(cohort$death_cause), 0, cohort$death_cause)
cohort$death_renal <- ifelse(cohort$death_cause == 9, 1, 0)

#drop participants younger than 20
cohort <- cohort[cohort$age_at_screen >= 20,]

#initial missingness check
vis_miss(cohort)
miss_var_summary(cohort)
mcar_test(cohort)

cohort[, c("id", "gender", "race", "education_lev", "psu", "masked_var_psuedo_strat", 
           "diabetes", "prediabetes", "depression", "gen_health", 
           "cancer", "tobacco", "dead_2019", "death_cause", "death_renal")] <- 
  lapply(cohort[, c("id", "gender", "race", "education_lev", "psu", 
                    "masked_var_psuedo_strat", "diabetes", "prediabetes", 
                    "depression", "gen_health", "cancer", "tobacco", "dead_2019",
                    "death_cause", "death_renal")], 
         as.factor)

#recoding vars
cohort$gender <- as.factor(ifelse(cohort$gender == 1, "Male", "Female"))
race_labels <- c(
  "1" = "Mexican_American",
  "2" = "Other_Hispanic",
  "3" = "Non_Hispanic_White",
  "4" = "Non_Hispanic_Black",
  "5" = "Other_Race"
)
cohort$race <- as.factor(race_labels[as.character(cohort$race)])
diabetes_labels <- c(
  "1" = "Yes",
  "2" = "No",
  "3" = "Pre-Diabetes",
  "9" = "Doesn't_Know"
)
cohort$diabetes <- as.factor(diabetes_labels[as.character(cohort$diabetes)])
cohort$education_lev <- factor(cohort$education_lev, 
                               levels = c(1,2,3,4,5,7,9), 
                               ordered = TRUE)
gen_health_labels <- c(
  "1" = "Excellent",
  "2" = "Very good",
  "3" = "Good",
  "4" = "Fair",
  "5" = "Poor",
  "9" = "Doesn't_Know"
)
cohort$gen_health <- as.factor(gen_health_labels[as.character(cohort$gen_health)])
cancer_labels <- c(
  "1" = "Yes",
  "2" = "No",
  "9" = "Doesn't_Know"
)
cohort$cancer <- as.factor(cancer_labels[as.character(cohort$cancer)])
tobacco_labels <- c(
  "1" = "Yes",
  "2" = "No",
  "7" = "Refused"
)
cohort$tobacco <- as.factor(tobacco_labels[as.character(cohort$tobacco)])

#compute egfr
cohort <- cohort %>%
  mutate(
    egfr = case_when(
      gender == "Female" & creatinine_serum <= 0.7 ~ 144 * 
        (creatinine_serum / 0.7) ^ (-0.329) * 
        (0.993 ^ age_at_screen) * ifelse(race == "Non_Hispanic_Black", 1.159, 1),
      gender == "Female" & creatinine_serum > 0.7 ~ 144 * 
        (creatinine_serum / 0.7) ^ (-1.209) * 
        (0.993^age_at_screen) * ifelse(race == "Non_Hispanic_Black", 1.159, 1),
      gender == "Male" & creatinine_serum <= 0.9 ~ 141 * 
        (creatinine_serum / 0.9) ^ (-0.411) * (0.993 ^ age_at_screen) * 
        ifelse(race == "Non_Hispanic_Black", 1.159, 1),
      gender == "Male" & creatinine_serum > 0.9 ~ 141 * 
        (creatinine_serum / 0.9) ^ (-1.209) * (0.993 ^ age_at_screen) * 
        ifelse(race == "Non_Hispanic_Black", 1.159, 1),
      TRUE ~ NA_real_
    )
  )

#code lead poisoning
cohort$lead_poisoning <- as.factor(ifelse(cohort$lead > 3.5, 1, 0))
cohort$renal_damage <- as.factor(ifelse(cohort$egfr < 60, 1, 0))

#use first MICE imputation for exploratory analysis
cohort_imp_quickpred <- quickpred(cohort, mincor = .2)
censor <- c(
  "id", "dead_2019", "lead_poisoning", "egfr", "masked_var_psuedo_strat", 
  "SDMVPSU", "SDMVSTRA", "WTMEC2YR", "WTINT2YR"
)
censor <- intersect(censor, colnames(cohort_imp_quickpred))
cohort_imp_quickpred[, censor] <- 0
cohort_prelim_imp <- mice(cohort, m = 1, pred = cohort_imp_quickpred)
prelim_imp <- complete(cohort_prelim_imp)

#exploratory analysis
summary(cohort)

hist(cohort$creatinine_serum, breaks = 100)
hist(cohort$albumin_urine, breaks = 100)
hist(cohort$creatinine_urine, breaks = 100)
hist(cohort$waist_circ, breaks = 100)
hist(cohort$`c-reactive_prot`, breaks = 100)
hist(cohort$age_at_screen, breaks = 100)
hist(cohort$family_pir, breaks = 100)
hist(cohort$egfr, breaks = 100)

hist(prelim_imp$creatinine_serum, breaks = 100)
hist(prelim_imp$albumin_urine, breaks = 100)
hist(prelim_imp$creatinine_urine, breaks = 100)
hist(prelim_imp$waist_circ, breaks = 100)
hist(prelim_imp$`c-reactive_prot`, breaks = 100)
hist(prelim_imp$age_at_screen, breaks = 100)
hist(prelim_imp$family_pir, breaks = 100)
hist(prelim_imp$egfr, breaks = 100)

#preliminary chi-square tests
vcd::mosaic(renal_damage ~ lead_poisoning, data = prelim_imp)
chisq.test(table(prelim_imp$renal_damage, prelim_imp$lead_poisoning))

vcd::mosaic(dead_2019 ~ renal_damage, data = prelim_imp)
chisq.test(table(prelim_imp$dead_2019, prelim_imp$renal_damage))

vcd::mosaic(dead_2019 ~ lead_poisoning, data = prelim_imp)
chisq.test(table(prelim_imp$dead_2019, prelim_imp$lead_poisoning))

#Multiple Correspondence Analysis
cohort_mca <- MCA(prelim_imp[, c("gender", "race", "education_lev", "diabetes", 
                                 "depression", "gen_health", "cancer", "tobacco", 
                                 "dead_2019")])

#Canonical Correlation Analysis
cohort_cc <- cca(as.matrix(prelim_imp[, c("albumin_urine", 
                                          "creatinine_urine", 
                                          "egfr", 
                                          "c-reactive_prot")]), 
                        as.matrix(prelim_imp[, c("bmi", 
                                                 "waist_circ", 
                                                 "age_at_screen", 
                                                 "family_pir")]))
helio.plot(cohort_cc, lab.cex = .8, name.cex = .9, 
           x.name = "Biomarkers", 
           y.name = "Physical and Demographic \n Measurements", 
           main = "Biomarkers Vs. Physical and Demographic Measurements") 

#prelim mediation analyses
des <- svydesign(
  id = ~ psu,
  strata = ~ masked_var_psuedo_strat,
  weights = ~ exam_weight,
  nest = TRUE,
  data = prelim_imp
)

#mediation analysis for death in general
med_mod <- svyglm(egfr ~ lead + age_at_screen + gender + race + family_pir + bmi, design = des)
out_mod <- svyglm(dead_2019 ~ lead + egfr + age_at_screen + gender + race + family_pir + bmi, family = quasibinomial(), design = des)
med_mod_w <- lm(egfr ~ lead + age_at_screen + gender + race + family_pir + bmi,
                data = prelim_imp)
out_mod_w <- glm(dead_2019 ~ lead + egfr + age_at_screen + gender + race + family_pir + bmi,
                 data = prelim_imp, family = binomial(link="logit"))
med <- mediate(med_mod_w, out_mod_w, treat="lead", mediator="egfr", sims=1000)
summary(med)

#mediation analysis for death where leading cause was kidney related
med_mod <- svyglm(egfr ~ lead + age_at_screen + gender + race + family_pir + bmi, design = des)
out_mod <- svyglm(death_renal ~ lead + egfr + age_at_screen + gender + race + family_pir + bmi, family = quasibinomial(), design = des)
med_mod_w <- lm(egfr ~ lead + age_at_screen + gender + race + family_pir + bmi,
                data = prelim_imp)
out_mod_w <- glm(death_renal ~ lead + egfr + age_at_screen + gender + race + family_pir + bmi,
                 data = prelim_imp, family = binomial(link="logit"))
med <- mediate(med_mod_w, out_mod_w, treat="lead", mediator="egfr", sims=1000)
summary(med)
