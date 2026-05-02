################################################################################
#Paolo Lambre, paololambre0@gmail.com, 2025 ####################################
################################################################################
set.seed(1643)

library(readr)
library(haven)
library(dplyr)
library(tidyr)

################################################################################
#Load Data #####################################################################
################################################################################
setwd("C:/Users/Paolo/Desktop/Personal Research/lead_kidneys/nhanes_data/")
#Uses 2005-2006 NHANES Survey Data and 2019 Mortality Data
#Albumin & Creatinine - Urine
ALB_CR_D <- read_xpt("ALB_CR_D.xpt")
BIOPRO_D <- read_xpt("BIOPRO_D.xpt")
#Alcohol Use
#ALQ_D <- read_xpt("ALQ_D.xpt")
#Body Measures
BMX_D <- read_xpt("BMX_D.xpt")
#Blood Pressure
BPX_D <- read_xpt("BPX_D.xpt")
#Complete Blood Count
CBC_D <- read_xpt("CBC_D.xpt")
#C-Reactive Protein (CRP)
CRP_D <- read_xpt("CRP_D.xpt")
#Demographic Data
DEMO_D <- read_xpt("DEMO_D.xpt")
#Diabetes
DIQ_D <- read_xpt("DIQ_D.xpt")
#Mental Health - Depression Screener	
DPQ_D <- read_xpt("DPQ_D.xpt")
#Current Health Status
HSQ_D <- read_xpt("HSQ_D.xpt")
#Kidney Conditions - Urology	
#KIQ_U_D <- read_xpt("KIQ_U_D.xpt")
#Medical Conditions	
MCQ_D <- read_xpt("MCQ_D.xpt")
#Physical Activity Monitor	
#paxraw_d <- read_xpt("paxraw_d.xpt")
#Cadmium, Lead, & Total Mercury - Blood	
PBCD_D <- read_xpt("PBCD_D.xpt")
#Smoking - Recent Tobacco Use	
SMQRTU_D <- read_xpt("SMQRTU_D.xpt")
#Weight History
#WHQ_D <- read_xpt("WHQ_D.xpt")
#Mortality Data 2019
MORT <- read_fwf("NHANES_2005_2006_MORT_2019_PUBLIC.dat",
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

################################################################################
#Clean Data ####################################################################
################################################################################
DF0 <- BIOPRO_D[, c("SEQN", "LBXSCR")]
Df1 <- ALB_CR_D[, c("SEQN", "URXUMA", "URXUCR")]
Df2 <- BMX_D[, c("SEQN", "BMXBMI", "BMXWAIST")]
Df3 <- CRP_D[, c("SEQN", "LBXCRP")]
Df4 <- DEMO_D[, c("SEQN", "RIDAGEYR", "RIAGENDR", "RIDRETH1", "INDFMPIR", 
                  "DMDEDUC2", "WTMEC2YR", "SDMVPSU", "SDMVSTRA")]
Df5 <- DIQ_D[, c("SEQN", "DIQ010", "DIQ160")]
Df6 <- DPQ_D[, c("SEQN", "DPQ020")]
Df7 <- HSQ_D[, c("SEQN", "HSD010")]
Df8 <- MCQ_D[, c("SEQN", "MCQ220")]
Df9 <- PBCD_D[, c("SEQN", "LBXBPB")]
Df10 <- SMQRTU_D[, c("SEQN", "SMQ680")]
Df11 <- CBC_D[, c("SEQN", "LBXHGB")]
Df12 <- MORT[, c("SEQN", "mortstat", "ucod_leading")]

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
  full_join(Df11, by = "SEQN") %>%
  full_join(Df12, by = "SEQN")

cohort[, c("RIAGENDR", 
           "RIDRETH1", 
           "DMDEDUC2", 
           "DIQ010", 
           "DIQ160", 
           "DPQ020", 
           "HSD010", 
           "MCQ220", 
           "SMQ680", 
           "mortstat",
           "ucod_leading")] <- 
  lapply(cohort[, c("RIAGENDR", 
                    "RIDRETH1",
                    "DMDEDUC2", 
                    "DIQ010", 
                    "DIQ160", 
                    "DPQ020", 
                    "HSD010", 
                    "MCQ220", 
                    "SMQ680", 
                    "mortstat",
                    "ucod_leading")], as.factor)

cohort <- cohort %>%
  mutate(
    kappa = ifelse(RIAGENDR == 2, 0.7, 0.9),
    alpha = ifelse(RIAGENDR == 2, -0.241, -0.302),
    sex_mult = ifelse(RIAGENDR == 2, 1.012, 1),
    
    egfr = 142 *
      (pmin(LBXSCR / kappa, 1) ^ alpha) *
      (pmax(LBXSCR / kappa, 1) ^ -1.200) *
      (0.9938 ^ RIDAGEYR) *
      sex_mult
  ) %>%
  select(-kappa, -alpha, -sex_mult)

names(cohort) <- c(
  "SEQN",
  "serum_creatinine_mg_dl",      #LBXSCR
  "urine_albumin_mg_l",          #URXUMA
  "urine_creatinine_mg_dl",      #URXUCR
  "bmi",                         #BMXBMI
  "waist_circumference_cm",      #BMXWAIST
  "c_reactive_protein_mg_l",     #LBXCRP
  "age_years",                   #RIDAGEYR
  "sex",                         #RIAGENDR
  "race_ethnicity",              #RIDRETH1
  "income_poverty_ratio",        #INDFMPIR
  "education_level",             #DMDEDUC2
  "exam_weight_2yr",             #WTMEC2YR
  "psu",                         #SDMVPSU
  "strata",                      #SDMVSTRA
  "diabetes_dx",                 #DIQ010
  "diabetes_insulin_use",        #DIQ160
  "depression_score_phq2",       #DPQ020
  "general_health_status",       #HSD010
  "cancer_dx",                   #MCQ220
  "blood_lead_ug_dl",            #LBXBPB
  "smoking_recent",              #SMQ680
  "hemoglobin_g_dl",             #LBXHGB
  "mortality_status",            #mortstat
  "leading_cause_of_death",      #ucod_leading
  "egfr"                         #egfr
)

cohort <- as.data.frame(cohort)

cohort <- cohort %>%
  mutate(race_ethnicity = case_match(race_ethnicity,
                                     "1" ~ "Mexican_American",
                                     "2" ~ "Other_Hispanic",
                                     "3" ~ "White",
                                     "4" ~ "Black",
                                     "5" ~ "Other",
                                     .default = race_ethnicity
  ))

cohort$sex <- ifelse(cohort$sex == "1", "male", "female")

cohort <- cohort %>%
  mutate(diabetes_dx = case_match(diabetes_dx,
                                  "1" ~ "Yes",
                                  "2" ~ "No",
                                  "3" ~ "Borderline",
                                  "9" ~ "Unknown",
                                  .default = diabetes_dx
  ))
cohort <- cohort %>%
  mutate(diabetes_insulin_use = case_match(diabetes_insulin_use,
                                           "1" ~ "Yes",
                                           "2" ~ "No",
                                           "9" ~ "Unknown",
                                           .default = diabetes_insulin_use
  ))
cohort <- cohort %>%
  mutate(cancer_dx = case_match(cancer_dx,
                                "1" ~ "Yes",
                                "2" ~ "No",
                                "9" ~ "Unknown",
                                .default = cancer_dx
  ))
cohort <- cohort %>%
  mutate(smoking_recent = case_match(smoking_recent,
                                     "1" ~ "Yes",
                                     "2" ~ "No",
                                     "7" ~ "Refused",
                                     .default = smoking_recent
  ))
cohort <- cohort %>%
  mutate(mortality_status = case_match(mortality_status,
                                       "0" ~ "No",
                                       "1" ~ "Yes",
                                       .default = mortality_status))

cohort[c("sex", "race_ethnicity", "diabetes_dx", "diabetes_insulin_use", 
         "cancer_dx", "smoking_recent", "mortality_status", "psu")] <- 
  lapply(cohort[c("sex", "race_ethnicity", "diabetes_dx", 
                  "diabetes_insulin_use", "cancer_dx", "smoking_recent", 
                  "mortality_status", "psu")], as.factor)

################################################################################
#Transformations, Missingness, & Imputation ####################################
################################################################################
library(naniar)
library(VIM)

naniar::vis_miss(cohort)
cohort$SEQN <- NULL
cohort$leading_cause_of_death <- NULL
cohort_adult <- cohort[cohort$age_years>=18,]
miss_pattern <- naniar::as_shadow_upset(cohort_adult)
naniar::vis_miss(cohort_adult, cluster = TRUE)
naniar::gg_miss_upset(cohort_adult)
cohort_adult_t <- cohort
cohort_adult_t[, c("serum_creatinine_mg_dl",
                   "urine_albumin_mg_l",
                   "urine_creatinine_mg_dl",
                   "bmi")] <- 
  sapply(cohort_adult_t[, c("serum_creatinine_mg_dl",
                            "urine_albumin_mg_l",
                            "urine_creatinine_mg_dl",
                            "bmi")], log)
cohort_adult_t_hd <- hotdeck(cohort_adult_t)
cohort_adult_t_i <- cohort_adult_t_hd[, c(1:24)]
cohort_adult_t_i

################################################################################
#Exploratory Analysis ##########################################################
################################################################################
library(psych)
library(FactoMineR)
library(yacca)
library(rcompanion)

numeric_vars <- c("serum_creatinine_mg_dl", "urine_albumin_mg_l", 
                  "urine_creatinine_mg_dl", 
                  "bmi", "waist_circumference_cm", "age_years", 
                  "income_poverty_ratio", "c_reactive_protein_mg_l", 
                  "egfr", "blood_lead_ug_dl", "hemoglobin_g_dl", 
                  "exam_weight_2yr")
cat_vars <- setdiff(names(cohort_adult_t_i), numeric_vars)
describe(cohort_adult_t_i[, numeric_vars])
summary(cohort_adult_t_i[, cat_vars])

cohort_adult_t_i_pearson <- cor(cohort_adult_t_i[, numeric_vars], 
                                method = "pearson")
cohort_adult_t_i_kendall <- cor(cohort_adult_t_i[, numeric_vars], 
                                method = "kendall")

cohort_transf_mi <- cohort_adult_transf_mi

cohort_adult_t_i <- cohort_adult_t_i %>% 
  mutate(age_group = case_when(
    age_years >= 28 ~ "Born_Pre-1978",
    TRUE ~ "Born_1978-1988",
  ))

cohort_Born_Pre_1978 <- 
  cohort_adult_t_i[cohort_adult_t_i$age_group == "Born_Pre-1978",]
cohort_Born_1978_1988 <- 
  cohort_adult_t_i[cohort_adult_t_i$age_group == "Born_1978-1988",]

describe(cohort_Born_Pre_1978[, numeric_vars])
summary(cohort_Born_Pre_1978[, cat_vars])
describe(cohort_Born_1978_1988[, numeric_vars])
summary(cohort_Born_Pre_1978[, cat_vars])

cohort_adult_t_i_norm <- cohort_adult_t_i
cohort_adult_t_i_norm[, numeric_vars[c(1:11)]] <- 
  sapply(cohort_adult_t_i_norm[, numeric_vars[c(1:11)]], scale)

cohort_adult_t_i_norm_famd <- FAMD(cohort_adult_t_i_norm)

cohort_adult_t_i_norm_cca <- 
  cca(as.matrix(cohort_adult_t_i_norm[, c("income_poverty_ratio",
                                          "age_years", 
                                          "waist_circumference_cm")]),
      as.matrix(cohort_adult_t_i_norm[, c("serum_creatinine_mg_dl", 
                                          "urine_albumin_mg_l", 
                                          "urine_creatinine_mg_dl",
                                          "blood_lead_ug_dl",
                                          "hemoglobin_g_dl",
                                          "egfr")]))
yacca::helio.plot(cohort_adult_t_i_norm_cca, 
                  x.name = "Social Determinants of Health",
                  y.name = "Metabolic Health", lab.cex = .7, main = "")

cohort_adult_t_i$elevated_lead <- cohort_adult_t_i$blood_lead_ug_dl>3.5

table_lead_mortality_all <- table(cohort_adult_t_i$elevated_lead, 
                                  cohort_adult_t_i$mortality_status)

chisq.test(table(na.omit(cohort_adult_t_i[, c("mortality_status", 
                                              "elevated_lead")])))
mantelhaen.test(table(cohort_adult_t_i$mortality_status, 
                      cohort_adult_t_i$blood_lead_ug_dl>3.5, 
                      cohort_adult_t_i$age_years))
mantelhaen.test(table(cohort_adult_t_i$mortality_status, 
                      cohort_adult_t_i$blood_lead_ug_dl>3.5, 
                      cohort_adult_t_i$race_ethnicity))
mantelhaen.test(table(cohort_adult_t_i$mortality_status, 
                      cohort_adult_t_i$blood_lead_ug_dl>3.5, 
                      cohort_adult_t_i$sex))

perc_test_med_lead_age <- percentileTest(blood_lead_ug_dl ~ age_group,
                                         data = cohort_adult_t_i,
                                         test = "median",
                                         r    = 10000)
perc_test_med_hem_age <- percentileTest(hemoglobin_g_dl ~ age_group,
                                        data = cohort_adult_t_i,
                                        test = "median",
                                        r    = 10000)
perc_test_med_egfr_age <- percentileTest(egfr ~ age_group,
                                         data = cohort_adult_t_i,
                                         test = "median",
                                         r    = 10000)
perc_test_mean_lead_age <- percentileTest(blood_lead_ug_dl ~ age_group,
                                          data = cohort_adult_t_i,
                                          test = "mean",
                                          r    = 10000)
perc_test_mean_hem_age <- percentileTest(hemoglobin_g_dl ~ age_group,
                                         data = cohort_adult_t_i,
                                         test = "mean",
                                         r    = 10000)
perc_test_mean_egfr_age <- percentileTest(egfr ~ age_group,
                                          data = cohort_adult_t_i,
                                          test = "mean",
                                          r    = 10000)

ks.test(cohort_adult_t_i[cohort_adult_t_i$age_group == "Born_Pre-1978", 
                         c("blood_lead_ug_dl")],
        cohort_adult_t_i[cohort_adult_t_i$age_group == "Born_1978-1988", 
                         c("blood_lead_ug_dl")])
ks.test(cohort_adult_t_i[cohort_adult_t_i$age_group == "Born_Pre-1978", 
                         c("hemoglobin_g_dl")],
        cohort_adult_t_i[cohort_adult_t_i$age_group == "Born_1978-1988", 
                         c("hemoglobin_g_dl")])
ks.test(cohort_adult_t_i[cohort_adult_t_i$age_group == "Born_Pre-1978", 
                         c("egfr")],
        cohort_adult_t_i[cohort_adult_t_i$age_group == "Born_1978-1988", 
                         c("egfr")])
################################################################################
#Visualizations#################################################################
################################################################################
library(ggplot2)
library(pheatmap)
library(car)
library(vioplot)
library(vcd)

cohort_adult %>%
  select(serum_creatinine_mg_dl,
         urine_albumin_mg_l,
         urine_creatinine_mg_dl,
         bmi,
         waist_circumference_cm,
         age_years,
         income_poverty_ratio,
         egfr,
         blood_lead_ug_dl,
         hemoglobin_g_dl) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 100) +
  facet_wrap(~name, scales = "free")

pheatmap(cohort_adult_t_i_pearson)
pheatmap(cohort_adult_t_i_kendall)

vioplot(blood_lead_ug_dl ~ age_group, data = cohort_adult_t_i,
        xlab = "Age Group", ylab = "Blood Lead ug/dl")
vioplot(hemoglobin_g_dl ~ age_group, data = cohort_adult_t_i,
        xlab = "Age Group", ylab = "Hemoglobin g/dl")
vioplot(egfr ~ age_group, data = cohort_adult_t_i,
        xlab = "Age Group", ylab = "eGFR")

scatterplot(cohort_adult_t_i$income_poverty_ratio, 
            cohort_adult_t_i$hemoglobin_g_dl, 
            smooth = FALSE,
            regLine = FALSE,
            xlab = "Income Poverty Ratio",
            ylab = "Hemoglobin g/dl")
scatterplot(cohort_adult_t_i$age_years, 
            cohort_adult_t_i$blood_lead_ug_dl, 
            smooth = FALSE,
            regLine = FALSE,
            xlab = "Age",
            ylab = "Blood Lead ug/dl")
scatterplot(cohort_adult_t_i$income_poverty_ratio,
            cohort_adult_t_i$blood_lead_ug_dl, 
            smooth = FALSE,
            regLine = FALSE,
            xlab = "Income Poverty Ratio",
            ylab = "Blood Lead ug/dl")
scatterplot(cohort_adult_t_i$blood_lead_ug_dl, 
            cohort_adult_t_i$hemoglobin_g_dl, 
            smooth = FALSE,
            regLine = FALSE,
            xlab = "Blood Lead ug/dl",
            ylab = "Hemoglobin g/dl")
scatterplot(cohort_adult_t_i$blood_lead_ug_dl,
            cohort_adult_t_i$income_poverty_ratio, 
            smooth = FALSE,
            regLine = FALSE,
            xlab = "Blood Lead ug/dl",
            ylab = "Income Poverty Ratio")
scatterplot(cohort_adult_t_i$income_poverty_ratio, 
            cohort_adult_t_i$egfr,
            smooth = FALSE,
            regLine = FALSE,
            xlab = "Income Poverty Ratio",
            ylab = "eGFR")

mosaic(table(na.omit(cohort_adult_t_i[, c("mortality_status", 
                                          "elevated_lead")])),
       shade = TRUE, 
       main = "Mortality Status by Elevated Blood Lead (ug/dl)>3.5")

################################################################################
#Preliminary Regression Analysis ###############################################
################################################################################
#Initial Univariate Models######################################################
model_blood_lead <- glm(blood_lead_ug_dl ~ age_group, 
                        data = cohort_adult_t_i,
                        family = inverse.gaussian(link = "inverse"))
summary(model_blood_lead)
plot(model_blood_lead)

model_hemoglobin <- glm(hemoglobin_g_dl ~ age_group, 
                        data = cohort_adult_t_i)
summary(model_blood_lead)
plot(model_blood_lead)

model_death <- glm(mortality_status ~ blood_lead_ug_dl, 
                   data = cohort_adult_t_i,
                   family = binomial(link = "logit"))
summary(model_death)
plot(model_death)
#Initial Covariate Controlled Models############################################
model_blood_lead_cov <- glm(blood_lead_ug_dl ~ age_group + 
                              income_poverty_ratio + smoking_recent + 
                              waist_circumference_cm + diabetes_dx + 
                              race_ethnicity + sex, 
                            data = cohort_adult_t_i,
                            family = inverse.gaussian(link = "inverse"))
summary(model_blood_lead_cov)
plot(model_blood_lead_cov)

model_hemoglobin_cov <- glm(hemoglobin_g_dl ~ blood_lead_ug_dl * age_group + 
                              income_poverty_ratio + smoking_recent + 
                              waist_circumference_cm + diabetes_dx + 
                              race_ethnicity + sex, 
                            data = cohort_adult_t_i)
summary(model_hemoglobin_cov)
plot(model_hemoglobin_cov)

model_death_cov <- glm(mortality_status ~ blood_lead_ug_dl + age_group + 
                         income_poverty_ratio + smoking_recent + 
                         waist_circumference_cm + diabetes_dx + 
                         race_ethnicity + sex, 
                       data = cohort_adult_t_i,
                       family = binomial(link = "logit"))
summary(model_death_cov)
plot(model_death_cov)
################################################################################
#Survey Weighed Regression Analysis ############################################
################################################################################
library(survey)

design <- svydesign(
  ids    = ~psu,
  strata = ~strata,
  weights = ~exam_weight_2yr,
  data   = cohort_adult_t_i,
  nest   = TRUE
)
#Univariate Models##############################################################
model_blood_lead_surv <- svyglm(blood_lead_ug_dl ~ age_group, 
                                family = inverse.gaussian(link = "inverse"),
                                design = design,
)
summary(model_blood_lead_surv)

model_hemoglobin_surv <- svyglm(hemoglobin_g_dl ~ age_group, 
         design = design, 
         family = inverse.gaussian(link = "inverse")
  )
summary(model_hemoglobin_surv)

model_death_cov_surv <- svyglm(mortality_status ~ blood_lead_ug_dl, 
                               design = design,
                               family = binomial(link = "logit"))
summary(model_death_cov_surv)

#Covariate Controlled Models####################################################

model_blood_lead_cov_surv <- svyglm(blood_lead_ug_dl ~ 
                                      age_group + income_poverty_ratio + 
                                      smoking_recent + waist_circumference_cm + 
                                      diabetes_dx + race_ethnicity + sex, 
         design = design,
         family = inverse.gaussian(link = "inverse"))
summary(model_blood_lead_cov_surv)

model_hemoglobin_cov_surv <- svyglm(hemoglobin_g_dl ~ 
                                      blood_lead_ug_dl * age_group + 
                                      income_poverty_ratio + smoking_recent + 
                                      waist_circumference_cm + diabetes_dx + sex, 
                                    design = design, 
                                    family = inverse.gaussian(link = "inverse"))
summary(model_hemoglobin_cov_surv)

model_death_cov_surv <- svyglm(mortality_status ~ 
                                 blood_lead_ug_dl + age_group + 
                                 income_poverty_ratio + smoking_recent + 
                                 waist_circumference_cm + diabetes_dx + 
                                 race_ethnicity + sex, 
                               design = design,
                               family = binomial(link = "logit"))
summary(model_death_cov_surv)
