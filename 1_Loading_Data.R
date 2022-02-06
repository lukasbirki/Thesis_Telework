# Loading Data / Joining ----
library(foreign)
library(xtable)
library(haven)
library(tidyverse)
library(lavaan)

#Loading and merging Data Sets

#Crosssectional Data
df_t0 <- read_sav("data/Home Office Studie_T0_Rohdaten_OhneDoppelteTeilnehmer.sav") %>% 
  as_tibble() %>% 
  drop_na(EE01_01_T0) %>% 
  dplyr::rename(person_id = ID02_01) %>% 
  dplyr::select(person_id,
                gender = QC01_T0, 
                age = QC02_01_T0,
                occupation_area = DG09_T0,
                tenure = DG06_01_T0,
         starts_with("DF"), #Digital Fluency 
         starts_with("CS"),
         starts_with("DG12_"),
         starts_with("DG13_")) #Coworker Support

#Multilevel Data
df_t1_t8 <- readxl::read_excel("data/Home Office Studie_Rohdaten_Tag_08.xlsx") %>% 
  as_tibble() %>% 
  dplyr::rename(time_id = QUESTNNR, person_id = ID02_01) %>% 
  dplyr::select(time_id, 
                person_id,
                starts_with("HO"), #Telework Share
                starts_with("EE"), #exhaustion
                starts_with("PF")) #Performance


#Merging Dataframes

left_join(df_t1_t8, df_t0, by = "person_id") %>% 
  drop_na(EE01_01) -> df

df %>% group_by(person_id) %>% count()

#Filtering
#a) All obervations from TX with missing obervations for TX+1 (except T8)
#b) Participants with less than three survey periods
#c) All obervations from T8 (no lagged variables available)

#a
df %>% 
  mutate(day_id = case_when(
    time_id == "Tag01" ~ 1,
    time_id == "Tag02" ~ 2,
    time_id == "Tag03" ~ 3,
    time_id == "Tag04" ~ 4,
    time_id == "Tag05" ~ 5,
    time_id == "Tag06" ~ 6,
    time_id == "Tag07" ~ 7,
    time_id == "Tag08" ~ 8
  )) %>% arrange(person_id, day_id) %>% 
  group_by(person_id) %>% 
  filter(dplyr::lead(day_id) == day_id +1 | dplyr::lag(day_id) == day_id -1) -> df_a

#b
df_a %>% 
  group_by(person_id) %>% 
  count() %>% 
  filter(n < 5) -> df_antijoin  #removing particpants with less than three survey waves

anti_join(df_a, df_antijoin, by = c("person_id")) -> df_b

#c (and creating lagged variable for Next-Day Performance)

df_b %>% 
  group_by(person_id) %>% 
  mutate(across(contains("PF"), .fns = list(lagged = ~dplyr::lead(.))),
         telework = HO01_01) %>% 
  drop_na(-DG13_01_T0) %>% 
  mutate(tenure = case_when(
    (tenure == 0) ~ 0.001,
    T ~ tenure)) %>% 
  mutate(friday = case_when(#Adding control variable for "Holiday Effect"
      day_id == 3 ~ 1, #Thank God its Friday Effect
      T ~ 0)) %>% 
  mutate(living_situation = case_when( #!! Coding might be wrong 2 = yes, 1 = no
      (DG12_01_T0 == 2) ~ 1, #Living Alone
      (DG12_01_T0 == 1) & (DG12_02_T0 == 2) & (DG12_03_T0 == 1) ~ 2, #Living with a partner (without children)
      (DG12_03_T0 == 2) ~ 3)) %>% 
  filter(PF01_01_lagged > 0) %>% 
  filter(PF01_02_lagged > 0) %>% 
  filter(PF01_03_lagged > 0) %>% 
  filter(EE01_01 > 0) -> df_final  #Living with children

#Delta for b
paste("Delta Level 1:",nrow(df_a) - nrow(df_b))
paste("Delta Level 2:", nrow(df_a %>% group_by(person_id) %>% count()) - nrow(df_b %>% group_by(person_id) %>% count()))

#Delta for a + c

paste("Delta Level 1:",(nrow(df) - nrow(df_a)) +  (nrow(df_b) - nrow(df_final)))
paste("Delta Level 2:",(nrow(df %>% group_by(person_id) %>% count()) - nrow(df_a %>% group_by(person_id) %>% count())) + 
        (nrow(df_b %>% group_by(person_id) %>% count()) - nrow(df_final %>% group_by(person_id) %>% count())))

#Descriptive Statistics ----

nrow(df_final)
nrow(df_final %>% group_by(person_id) %>% count())

df_final %>% mutate(day_id = case_when(
  (day_id == 1) ~ "T1", 
  (day_id == 2) ~ "T2",
  (day_id == 3) ~ "T3",
  (day_id == 4) ~ "T4",
  (day_id == 5) ~ "T5",
  (day_id == 6) ~ "T6",
  (day_id == 7) ~ "T7")) %>% 
  mutate(gender = gender-1) %>% 
  group_by(day_id) %>% 
  summarize(n = n(),
            `Mean Age` = mean(age),
            `Mean Tenure` = mean(tenure),
            `Percent Male` = sum(gender)/n) %>% xtable() %>% 
  xtable::print.xtable(.,include.rownames=FALSE, file = "./output/table_days.txt")

table(df_manifest$gender, df_manifest$day_id)


### Mean survey participation
df_final %>%  group_by(person_id) %>% count() -> count_df
mean(count_df$n)

# Demographics

df_final %>% dplyr::select(person_id, age, gender,occupation_area, tenure) %>%  group_by(person_id) %>% unique() -> des_df

table(des_df$age) #Age
sd(des_df$age)

table(des_df$tenure) #Tenure
sd(des_df$tenure)

des_df %>%
  group_by(occupation_area) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 3)) %>% 
  arrange(desc(freq))

des_df %>%
  group_by(gender) %>%
  summarise(cnt = n()) %>%
  mutate(freq = round(cnt / sum(cnt), 3)) %>% 
  arrange(desc(freq))



rm(list = setdiff(ls(), ls() %>% str_subset("final")))

