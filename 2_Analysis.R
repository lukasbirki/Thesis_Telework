library(lme4)
library(numDeriv)
library(sjPlot)
library(sjmisc)
library(psych)
library(simr)
theme_set(theme_sjplot())

# Preprocessing ----

## Generating manifest Variables ----
df_final %>% 
  rowwise() %>% 
  mutate(exhaustion = sum(EE01_01, EE01_02, EE01_03)/3,
         performance_lagged = sum(PF01_01_lagged, PF01_02, PF01_03_lagged)/3,
         dfluency = sum(DF01_01_T0, 
                        DF01_02_T0, 
                        DF01_03_T0,
                        DF02_01_T0, 
                        DF02_02_T0, 
                        DF02_03_T0)/6,
         support = sum(CS01_01_T0, 
                       CS01_02_T0, 
                       CS01_03_T0, 
                       CS02_01_T0, 
                       CS02_02_T0, 
                       CS02_03_T0)/6) %>%
  select(day_id, person_id, exhaustion, performance_lagged, dfluency, support) %>% 
  left_join(df_final, ., by = c("person_id", "day_id")) %>% 
  dplyr::select(!matches('[0-9]$')) %>% ungroup-> df_manifest_temp

## Centering ----

### https://philippmasur.de/2018/05/23/how-to-center-in-multilevel-models/
### https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html

df_manifest <- df_manifest_temp %>%
  mutate(telework_logged = log(telework)) %>% 
  # Grand mean centering (CMC)
  mutate(gmc.fluency = dfluency - mean(dfluency),
         gmc.support = support - mean(support),
         gmc.age = age - mean(age),
         gmc.tenure = tenure - mean(tenure)) %>% 
  # Person mean centering (more generally, centering within cluster)
  group_by(person_id) %>% 
  mutate(pm.performance = mean(performance_lagged),
         pm.exhaustion = mean(exhaustion),
         pm.telework = mean(telework),
         pm.telework_logged = mean(telework_logged),
         pmc.performance_lagged =  performance_lagged - pm.performance,
         pmc.exhaustion =  exhaustion - pm.exhaustion,
         pmc.telework =  (telework - pm.telework)/100,
         pmc.telework_logged = telework_logged - pm.telework_logged
         ) %>%
  ungroup() 

# Descriptive Statistics ----

## Correlations ----
df_manifest %>% 
  dplyr::select(person_id, exhaustion, performance_lagged, telework, 
                dfluency, support) %>% 
psych::statsBy(., group = "person_id") -> descriptive_statistics

### variances
df_manifest %>% 
  pivot_longer(cols = c(exhaustion,telework) , names_to = "Variable", values_to = "Werte") %>% 
  select(person_id, Variable, Werte) %>% 
  group_by(person_id, Variable) %>% 
  summarise(GroupVariance=var(Werte), TotalCount=n()) %>% 
  ggplot() +
  facet_wrap(.~Variable, scales="free") +
  geom_density(aes(x = GroupVariance)) 


### Between Correlations

descriptive_statistics$mean %>%  
  as_tibble() %>% select(-person_id) %>%
  colMeans() -> means_ds

descriptive_statistics$mean %>%  
  as_tibble() %>% select(-person_id) %>% as.matrix() %>% 
  matrixStats::colSds() -> sd_ds

round(descriptive_statistics$rbg, 2) -> mcor
mcor -> upper
upper[upper.tri(mcor, diag = T)]<-""
as.data.frame(upper) -> correlations_between

### Achtung: Hier die Wahrscheinlichkeiten vielleicht noch händisch einfügen
round(descriptive_statistics$pbg, 2) -> prp_cor
prp_cor -> sd_upper
sd_upper[upper.tri(prp_cor, diag = T)]<-""
as.data.frame(sd_upper) -> prop_correlations_between

cbind(means_ds, sd_ds, correlations_between) %>%  #Zusammenfügen und Export Latex
  xtable::xtable()

### #Within Correlations

df_manifest %>% 
  dplyr::select(person_id, exhaustion, performance_lagged, telework, 
                dfluency, support)  ->df_within_correlation

rmcorr::rmcorr(telework, as.numeric(exhaustion), "person_id", df_within_correlation)
rmcorr::rmcorr(telework, performance_lagged, "person_id", df_within_correlation)
rmcorr::rmcorr(exhaustion, performance_lagged, "person_id", df_within_correlation)

##Plots 

df_manifest %>%
  mutate(telework_exp = scale(exp(telework))) %>% 
  select(telework_exp, telework) %>% 
  pivot_longer(telework_exp:telework, names_to = "Variables", values_to = "Values") %>%
  ggplot(aes(x = Values)) +
  geom_histogram(binwidth=.5)+
  facet_wrap(~Variables, scales = "free") +
  labs(x = "Response (on a 1 to 5 scale)", y = "Number of respondents")

df_manifest %>%
  ggplot(aes(x = telework_exp)) +
  geom_density()+
  labs(x = "Amount of Telework", y = "Density Telework") +
  theme(plot.title = element_text("Density Distribution Telework")) 

ggsave("./plots/density_telework.png", width = 10, height = 7)



## ICCs and Null Modell ----

## Null Model
fit_tw <- lme4::lmer(telework ~ 1 + (1 | person_id), data = df_manifest)
fit_ex <- lme4::lmer(exhaustion ~ 1 + (1 | person_id), data = df_manifest)
fit_pf <- lme4::lmer(performance_lagged ~ 1 + (1 | person_id), data = df_manifest)

##ICCs
performance::icc(fit_tw)[1]
print(lme4::VarCorr(fit_tw),comp="Variance")
performance::icc(fit_ex)[1]
print(lme4::VarCorr(fit_ex),comp="Variance")
performance::icc(fit_pf)[1]
print(lme4::VarCorr(fit_pf),comp="Variance")

## Power Analysis

model <- lme4::lmer(exhaustion ~ performance_lagged + (1 | person_id), data = df_manifest)

#power <- simr::powerSim(model, nsim = 100)
#print(power)

# Analysis ----

## Telework auf Exhaustion

#Simple Relationsship between the models ----

### Simple Direct Effect
fit.01 <- lme4::lmer(exhaustion ~ pmc.telework +  (1 | person_id), 
                           data = df_manifest)

### Simple Direct Effect + Adding Controls within + Interaction 

fit.02 <- lme4::lmer(exhaustion ~ pmc.telework + pmc.telework*gmc.fluency  + pmc.telework*gmc.support  + time_id+(1 | person_id), 
                      data = df_manifest)

fit.03 <- lme4::lmer(exhaustion ~ pmc.telework + pmc.telework*gmc.fluency  + pmc.telework*gmc.support  +time_id+ (1 + pmc.telework | person_id), 
                     data = df_manifest)


texreg::texreg(list(fit.01, fit.02, fit.03), 
               omit.coef = "time_id", 
               custom.gof.rows = list("Time Dummies" = c("No",  "Yes", "Yes"), 
                                          "Random Slopes" = c("No",  "No", "Yes")),
                custom.coef.names = c("Intercept", "Telework", 
                                      "Digital Fluency", "Support",
                                       "Telework*Digital Fluency",
                                      "Telework*Support"), 
               caption = "Multilevel Model predicting Emotional Exhaustion",bold = T,
               file = "./output/table_exhaustion.txt") 

texreg::htmlreg(list(fit.01, fit.02, fit.03), 
                  omit.coef = "time_id",
                  custom.gof.rows = list("Time Dummies" = c("No",  "Yes", "Yes"),
                                         "Random Slopes" = c("No",  "No", "Yes")),
               custom.coef.names = c("Intercept", "Telework",
                                     "Digital Fluency", "Support",
                                      "Telework*Digital Fluency",
                                     "Telework*Support"),
               caption = "Multilevel Model predicting Emotional Exhaustion",bold = T,
               file = "./output/table_exhaustion.html") 

#Model Fit and Mode Selection

AIC(fit.02) - AIC(fit.03)
as.numeric(cAIC4::cAIC(fit.02)[1]) - as.numeric(cAIC4::cAIC(fit.03)[1])


sjPlot::plot_models(fit.01, fit.02, fit.03, 
                    legend.title = "Models",m.labels = c("Model 1", "Model 2",
                                                         "Model 3"),
                    axis.labels = c("Telework*Support", "Telework*Digital Fluency",
                                    "Day 7 (Dummy)", "Day 6 (Dummy)", "Day 5 (Dummy)", "Day 4 (Dummy)",
                                    "Day 3 (Dummy)", "Day 2 (Dummy)","Coworker Support",  "Digital Fluency","Telework"),
                    title = "Multilevel Model predicting Emotional Exhaustion",spacing = 0.5)

ggsave("./figures/visualised_models_exhaustion.png", width = 10, height = 7)

## Displaying Interactions

pint <- plot_model(fit.03, type = "int")
p1 <- pint[[1]]
p2 <- pint[[2]]

p1 <- p1 + theme(plot.title = element_text(hjust = 0.5),
                 legend.position = "bottom")
p1$labels$title <- "Interaction of Telework and Digital Fluency"
p1$labels$colour <- "Digital Fluency"
p1$labels$x <- "Telework"
p1$labels$y <- "Emotional Exhaustion"

p2 <- p2 + theme(plot.title = element_text(hjust = 0.5),
                 legend.position = "bottom")
p2$labels$title <- "Interaction of Telework and Support"
p2$labels$colour <- "Support"
p2$labels$x <- "Telework"
p2$labels$y <- "Emotional Exhaustion"

cowplot::plot_grid(p1, p2)

ggsave("./figures/int_Combined.png", width = 10, height = 4)

## Displaying Interactions Johnson Neyman

p1<- interactions::johnson_neyman(model = fit.02, pred = pmc.telework,
                             modx = gmc.support, 
                             title = "Telework x Support", 
                             sig.color = "blue", insig.color = "black")

p1$plot <- p1$plot + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5))
p1$plot$labels$x <- "Support"
p1$plot$labels$y <- "Telework"
p2 <- interactions::johnson_neyman(model = fit.03, pred = pmc.telework,
                             modx = gmc.fluency,
                             title = "Telework x Digital Fluency",
                             sig.color = "blue", insig.color = "black")
p2$plot <- p2$plot + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5))
p2$plot$labels$x <- "Digital Fluency"
p2$plot$labels$y <- "Telework"

plot_grid(p1$plot, p2$plot)
ggsave("./figures/Neyman_Combined.png", width = 10, height = 4)

### Simple Direct Effect
fit.04 <- lme4::lmer(performance_lagged ~ pmc.exhaustion +  (1 | person_id), 
                     data = df_manifest)

### Simple Direct Effect + Adding Controls within 

fit.05 <- lme4::lmer(performance_lagged ~ pmc.exhaustion + gmc.fluency + gmc.support + time_id +  (1 | person_id), 
                     data = df_manifest)

fit.06 <- lme4::lmer(performance_lagged ~ pmc.exhaustion + gmc.fluency + gmc.support + time_id +  (1 + pmc.exhaustion| person_id), 
                     data = df_manifest, REML = F)

texreg::texreg(list(fit.05, fit.06, fit.07), 
               caption = "Multilevel Model predicting Next-Day Working Performance",
               omit.coef = "time_id", 
               custom.gof.rows = list("Time Dummies" = c("No", "Yes", "Yes"), 
                                      "Random Slopes" = c("No", "No", "Yes")),
               custom.coef.names = c("Intercept", "Exhaustion", 
                                     "Digital Fluency", "Support"), 
               file = "./output/table_performance.txt") 


cAIC4::cAIC(fit.04)
cAIC4::cAIC(fit.05)
cAIC4::cAIC(fit.06)


library(AICcmodavg)
models_exhaustion <- list(fit.01,fit.02,fit.03)
models_performance <- list(fit.04,fit.05,fit.06)

model.names.ex <- c('Model 1', 'Model 2', 'Model 3', 'Model 4')
model.names.per <- c('Model 5', 'Model 6', 'Model 7')

aictab(cand.set = models_exhaustion, modnames = model.names.ex)
aictab(cand.set = models_performance, modnames = model.names.per)

sjPlot::plot_models(fit.05, fit.06, fit.07, 
                    legend.title = "Models",m.labels = c("Model 5", "Model 6",
                                                         "Model 7"),
                    # axis.labels = c("Telework*Support", "Telework*Digital Fluency",
                    #                 "Coworker Support", "Digital Fluency",
                    #                 "Day 7 (Dummy)", "Day 6 (Dummy)", "Day 5 (Dummy)", "Day 4 (Dummy)",
                    #                 "Day 3 (Dummy)", "Day 2 (Dummy)", "Telework"),
                    title = "Multilevel Model predicting Next Day Performance",spacing = 0.5)

ggsave("./figures/visualised_models_performance.png", width = 10, height = 7)

#Checking for Mediation

fit.mediator <- lme4::lmer(performance_lagged ~ pmc.exhaustion + pmc.telework  + (1 | person_id), 
                           data = df_manifest)

fit.dv <- lme4::lmer(performance_lagged ~ pmc.exhaustion + pmc.telework  + (1 | person_id), 
                     data = df_manifest)


results <- mediation::mediate(fit.mediator, 
                              fit.dv, treat='pmc.telework', 
                              mediator='pmc.exhaustion')

source("./scripts/helpers.R")

extract_mediation_summary(summary(results)) -> results_mediation
print(xtable(results_mediation), file = "./output/mediation.txt")

#Getting Loaded Packages

sessionInfo() -> info
info$otherPkgs -> list3

names(info$otherPkgs) %>% 
  data.frame() -> info2
map(list3, ~.['Description']) %>% 
  bind_rows() %>% 
  cbind(info2, .) -> final

print(xtable(final), file = "./output/packages.txt")

rm(list = ls())

