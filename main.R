library(haven)
library(readr)
library(dplyr)
library(naniar)
library(FactoMineR)
library(yacca)

set.seed(1643)

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
Df4 <- DEMO_D[, c("SEQN", "RIDAGEYR", "RIAGENDR", "RIDRETH1", "INDFMPIR", "DMDEDUC2", "SDMVSTRA")]
Df5 <- DIQ_D[, c("SEQN", "DIQ010", "DIQ160")]
Df6 <- DPQ_D[, c("SEQN", "DPQ020")]
Df7 <- HSQ_D[, c("SEQN", "HSD010")]
Df8 <- MCQ_D[, c("SEQN", "MCQ220")]
Df9 <- PBCD_D[, c("SEQN", "LBXBPB")]
Df10 <- SMQRTU_D[, c("SEQN", "SMQ680")]
Df11 <- MORT[, c("SEQN", "mortstat")]

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
                   "masked_var_psuedo_strat", "diabetes", "prediabetes",
                   "depression", "gen_health", "cancer", "lead", "tobacco", 
                   "dead_2019")

#initial missingness check
vis_miss(cohort)
miss_var_summary(cohort)
mcar_test(cohort)

cohort[, c("id", "gender", "race", "education_lev", "masked_var_psuedo_strat", 
           "diabetes", "prediabetes", "depression", "gen_health", 
           "cancer", "tobacco", "dead_2019")] <- 
  lapply(cohort[, c("id", "gender", "race", "education_lev", 
                    "masked_var_psuedo_strat", "diabetes", "prediabetes", 
                    "depression", "gen_health", "cancer", "tobacco", "dead_2019")], 
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

cohort <- cohort %>%
  mutate(
    egfr = case_when(
      gender == "Female" & creatinine_serum <= 0.7 ~ 144 * (creatinine_serum / 0.7)^(-0.329) * (0.993^age_at_screen) * ifelse(race == "Non_Hispanic_Black", 1.159, 1),
      gender == "Female" & creatinine_serum > 0.7 ~ 144 * (creatinine_serum / 0.7)^(-1.209) * (0.993^age_at_screen) * ifelse(race == "Non_Hispanic_Black", 1.159, 1),
      gender == "Male" & creatinine_serum <= 0.9 ~ 141 * (creatinine_serum / 0.9)^(-0.411) * (0.993^age_at_screen) * ifelse(race == "Non_Hispanic_Black", 1.159, 1),
      gender == "Male" & creatinine_serum > 0.9 ~ 141 * (creatinine_serum / 0.9)^(-1.209) * (0.993^age_at_screen) * ifelse(race == "Non_Hispanic_Black", 1.159, 1),
      TRUE ~ NA_real_
    )
  )


#exploratory analysis
summary(cohort)
cohort_complete <- na.exclude(cohort)
cohort_mca <- MCA(cohort[, c("gender", "race", "education_lev", 
                             "diabetes", "depression", "gen_health", 
                             "cancer", "tobacco", "dead_2019")])
cohort_cc <- cca(as.matrix(cohort_complete[, c("albumin_urine", "creatinine_urine", "egfr")]), 
                        as.matrix(cohort_complete[, c("bmi", "waist_circ", "c-reactive_prot", "age_at_screen",
                                   "family_pir")]))
helio.plot(cohort_cc)    
