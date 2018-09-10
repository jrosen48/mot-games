##install.packages("lme4")

library(lme4)
library(tidyverse)
# install.packages("here")
library(here)
# library(readr) # loaded by tidyverse already
# install.packages("sjstats")
library(sjstats)

teya_analysis_file_for_r <- read_csv(here("teya_analysis_file_for_r.csv")) # here constructs file paths in a cool way & makes it easy to work cross-platforms
# View(teya_analysis_file_for_r)

##test with a simpler model
mbasic <- glm(persist ~ c_black + c_hisp + c_otherrace + c_male + c_gr4 +
                c_ell_binary + c_frl_binary + 
                c_mathse1 + c_mathut1 + c_mathimp1 + c_intmath1 + stud_c_levelscore +
                ave_c_faillevelscore + c_fsamathscalescore + 
                factor(objective_index), family = "binomial", data = teya_analysis_file_for_r)
summary(mbasic)

## null model for ICC

m0 <- glmer(persist ~ 1 +
            (1|student_entity_id) +
            (1|teacher_entity_id),
            family = "binomial", 
            control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)),
            data = teya_analysis_file_for_r)

icc(m0) # from sjstats package

##m1 <- glmer(persist ~ c_black + c_hisp + c_otherrace + (1|student_entity_id), family = "binomial", data = teya_analysis_file_for_r)

m1 <- glmer(persist ~ c_black + c_hisp + c_otherrace + c_male + c_gr4 +
              c_ell_binary + c_frl_binary + 
              c_mathse1 + c_mathut1 + c_mathimp1 + c_intmath1 + stud_c_levelscore +
              ave_c_faillevelscore + c_fsamathscalescore + factor(objective_index)
            + (1|student_entity_id), family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)), data = teya_analysis_file_for_r)
summary(m1)