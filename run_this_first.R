library(fst)
library(socsci)
library(car)
source("D://theme.R")


cces16 <- read.fst("C://cces16.fst")
cces18 <- read.fst("C://cces18.fst")

source("D://measuring_evangelicals/reltrad16.R")
source("D://measuring_evangelicals/reltrad18.R")

cc16 <- cces16 %>% 
  mutate(group = frcode(religpew_protestant == 7 ~ "Episcopalian",
                        religpew == 5 ~ "Jewish",
                        Hispanic_origin_4 == 1~ "Puerto Rican")) %>% 
  mutate(age = 2016 - birthyr) %>% 
  select(group, pew_churatd, educ, age, gender, pew_religimp, pew_bornagain, vote = CC16_410a, race, weight = commonweight_vv, income = faminc, evangelical) %>% 
  mutate(year = 2016)

cc18 <- cces18 %>% 
  mutate(group = frcode(religpew_protestant == 7 ~ "Episcopalian",
                        religpew == 5 ~ "Jewish",
                        CC18_354a_4 == 1~ "Puerto Rican")) %>% 
  mutate(age = 2018 - birthyr) %>% 
  select(group, pew_churatd, educ, age, gender, pew_religimp, pew_bornagain, vote = CC18_317, race, weight = commonpostweight, income = faminc_new, evangelical) %>% 
  mutate(year = 2018)

cc <- bind_rows(cc16, cc18) %>% 
  as_tibble()

rm(cc16, cc18, cces16, cces18)