library("dplyr")
library("broom")
library("stargazer")
library("xtable")
library("ggplot2")
library("patchwork")
library("texreg")

source("./preprocessing.R")

# ------------------------------------------------------------------------------
# Descriptive Analysis ---------------------------------------------------------
# ------------------------------------------------------------------------------

## |Table A2| Summary statistics -----------------------------------------------
stargazer(raw, type = "text",
          out = "summary.html")

## |Table 1| obj_decile mean misperception, proportion of overest and underest 
## misperception
prop <- raw %>% 
  group_by(obj_decile) %>% 
  count(obj_decile, mispercept2) %>% 
  mutate(prop = n / sum(n))
pos_prop <- prop[prop$mispercept2 == 1, c(1,4)]
names(pos_prop)[2] <- "pos_prop"
neg_prop <- prop[prop$mispercept2 == -1, c(1,4)]
names(neg_prop)[2] <- "neg_prop"

mean_dif <- raw %>% 
  group_by(obj_decile) %>% 
  summarise(sub_mean = mean(sub_decile)) %>% 
  mutate(mean_dif = sub_mean - obj_decile)

description <- merge(mean_dif, pos_prop, by = "obj_decile", all.x = T)
description <- merge(description, neg_prop, by = "obj_decile", all.x = T)

pd <- ggplot(data = description) + 
  geom_bar(aes(y = mean_dif, x = obj_decile), stat = "identity") +
  theme_minimal() + 
  scale_fill_brewer(palette = "Set2") +
  xlab("Objective Decile") +
  ylab("Average Misperception")

ggsave(pd, filename = "../figures/average_misperception.pdf")

description[is.na(description)] <- "-"
names(description) <- c("Obj Income", "Mean Subj Income", "Mean Diff", "\\% Over", "\\% Under")
tab1 <- xtable(description, digits = 2, include.rownames = FALSE)
print(tab1, file = "../tables/table1.tex", include.rownames = FALSE)

## |Figure 5| Histograms -------------------------------------------------------
p1 <- ggplot(data = raw) + 
  geom_bar(aes(x = obj_decile), stat = "count") +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  ylim(0,400) + 
  labs(x = "Objective Income", y = "Count") +
  theme_minimal()

p2 <- ggplot(data = raw) +
  geom_bar(aes(x = sub_decile), stat = "count") +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  ylim(0,400) +
  labs(x = "Subjective Income", y = "Count") +
  theme_minimal()

ggsave((p1 + p2), file = "../figures/fig5_Histograms.pdf", width = 10, height = 5)

# ------------------------------------------------------------------------------
# Balance Tests ----------------------------------------------------------------
# ------------------------------------------------------------------------------

# |Table A3 & A4| balance test for the whole sample ----------------------------
### define user function
controlvarlist <- c("sub_decile", "obj_decile", "age", "female", "marital",
                    "educ", "house", "employ1", "ideo5", "minju", "kukhim", 
                    "trust_gov", "merito", "gen_mobile", "concern_pol",
                    "ineq_percept")

dvlist <- c("redistribute", "duty_gov", "tax_pro")

tidy_t.test <- function(target, varlist){ # {{{
  output <- data.frame()
  for(i in varlist){
    temp <- eval(parse(text = sub("k", i, "t.test(k ~ group, data = target)")))
    temp2 <- tidy(temp)[1:5]
    names(temp2) <- c("mean_dif", "control_mean", "t1_mean", "t-stat", "p-value")
    temp2$varname <- i
    temp2$se <- temp$stderr
    temp2 <- temp2[c(6,1,7,2:5)]
    assign("output", rbind(output, temp2)) 
  }
  return(output)
}
# }}}

# Table A3 all control + t1
t1_control <- tidy_t.test(controlt1, controlvarlist)
t1_control$varname <- c(
  "Subjective Income", "Objective Income", "Age", "Female", "Marital Status", 
  "Education", "Home Ownership", "Employment", "Ideology", "Liberal Party",
  "Conservative Party", "Government Trust", "Meritocratic Belief", "Generational Mobility",
  "Concern Politics", "Inequality Perception"
)
names(t1_control) <- c("Variable", "Mean Diff", "SE", "Control Mean", "T1 Mean", "T-Stat", "p-value")
tabA3 <- xtable(t1_control[1:11, c(1, 4, 5, 7)], digits = 2)
print(tabA3, file = "../tables/tabA3_balance_T1.tex", include.rownames=F)

# manipulation checked
t1_control2 <- tidy_t.test(controlt1[controlt1$manipulation == 1, ], controlvarlist)

xtable(t1_control2, digits = 2)

# Tabel A4 all control + t2
t2_control <- tidy_t.test(controlt2, controlvarlist)
t2_control$varname <- c(
  "Subjective Income", "Objective Income", "Age", "Female", "Marital Status", 
  "Education", "Home Ownership", "Employment", "Ideology", "Liberal Party",
  "Conservative Party", "Government Trust", "Meritocratic Belief", "Generational Mobility",
  "Concern Politics", "Inequality Perception"
)
names(t2_control) <- c("Variable", "Mean Diff", "SE", "Control Mean", "T2 Mean", "T-Stat", "p-value")
tabA4 <- xtable(t2_control[1:11, c(1, 4, 5, 7)], digits = 2)
print(tabA3, file = "../tables/tabA4_balance_T2.tex", include.rownames=F)

# Balance Test for Subgroups --------------------------------------------------
## Overestimate mean difference -----------------------------------------------

# all control + t1
t1_overest2_control <- tidy_t.test(overest2, controlvarlist)

xtable(t1_overest2_control, digits = 2)

# manipulation checked
t1_overest2m_control <- tidy_t.test(overest2m, controlvarlist)

print(xtable(t1_overest2m_control), include.rownames=F)

## Underestimate mean difference --------------------------------------------
# all control + t1
t1_underest2_control <- tidy_t.test(underest2, controlvarlist)

xtable(t1_underest2_control, digits = 2)

# manipulation checked
t1_underest2m_control <- tidy_t.test(underest2m, controlvarlist)

xtable(t1_underest2m_control, digits = 2)

## No misperception mean difference --------------------------------------------
# all control + t1
t1_nomis2_control <- tidy_t.test(nomis2, controlvarlist)

xtable(t1_nomis2_control, digits = 2)

# manipulation checked
t1_nomis2m_control <- tidy_t.test(nomis2m, controlvarlist)

xtable(t1_nomis2m_control, digits = 2)

# -----------------------------------------------------------------------------
# Falsification Test ----------------------------------------------------------
# -----------------------------------------------------------------------------

datalist <- c("overest2", "underest2", "nomis2", "controlt1")
falsification <- data.frame()
for(i in datalist){
  temp <- eval(parse(text = sub("k", i, "t.test(donation ~ group, data = k)")))
  temp2 <- tidy(temp)[1:5]
  names(temp2) <- c("mean_dif", "control_mean", "t1_mean", "t-stat", "p-value")
  temp2$varname <- i
  temp2$se <- temp$stderr
  temp2 <- temp2[c(6,1,7,2:5)]
  assign("falsification", rbind(falsification, temp2)) 
}

falsification$varname <- c("Overestimate", "Underestimate", "No Misperception", "Whole")
names(falsification) <- c("Sample", "Mean Difference", "SE", "Control Mean", "T1 Mean", "T-Stat", "p-value")

tab_falsification <- xtable(falsification, digits = 2)
print(tab_falsification, file = "../tables/tab_falsification.tex", include.rownames = FALSE)

# |Figure 6 - 8| -------------------------------------------------------------------
# {{{
pdf(file = "../figures/fig_6_gov_responsibility.pdf", height = 5, width = 6)
controlt1 %>% 
  group_by(treatment, misgroup2) %>% 
  summarize(mean = mean(duty_gov),
            se = sd(duty_gov)/sqrt(length(duty_gov)),
            misgroup2 = misgroup2) %>% 
  ggplot(aes(x = treatment, color = treatment)
  ) +
  facet_wrap( ~ misgroup2) +
  geom_point(aes(y = mean)) +
  geom_errorbar(aes(ymin = mean - se , ymax = mean + se),
                width = 0.3) +
  ylab("Redistribution Preference -- Government Responsibility") +
  scale_x_discrete("", labels = c("control" = "Control", "t1" = "T1")) + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  scale_color_brewer(palette = "Set2")
dev.off()

pdf(file = "../figures/fig_7_progressive_tax.pdf", height = 5, width = 6)
controlt1 %>% 
  group_by(treatment, misgroup2) %>% 
  summarize(mean = mean(tax_pro),
            se = sd(tax_pro)/sqrt(length(tax_pro)),
            ) %>% 
  ggplot(aes(x = treatment, color = treatment)
  ) +
  facet_wrap( ~ misgroup2) +
  geom_point(aes(y = mean)) +
  geom_errorbar(aes(ymin = mean - se , ymax = mean + se),
                width = 0.3) +
  scale_x_discrete("", labels = c("control" = "Control", "t1" = "T1")) + 
  ylab("Redistribution Preference -- Progressive Tax") +
  theme_minimal() +
  theme(legend.position = "none") + 
  scale_color_brewer(palette = "Set2")
dev.off()

pdf(file = "../figures/fig_8_income_transfer.pdf", height = 5, width = 6)
controlt1 %>% 
  group_by(treatment, misgroup2) %>% 
  summarize(mean = mean(redistribute),
            se = sd(redistribute)/sqrt(length(redistribute)),
            ) %>% 
  ggplot(aes(x = treatment, color = treatment)
         ) +
  facet_wrap( ~ misgroup2) +
  geom_point(aes(y = mean)) +
  geom_errorbar(aes(ymin = mean - se , ymax = mean + se),
                width = 0.3) +
  scale_x_discrete("", labels = c("control" = "Control", "t1" = "T1")) + 
  ylab("Redistribution Preference -- Income Transfer") +
  theme_minimal() +
  theme(legend.position = "none") + 
  scale_color_brewer(palette = "Set2")
dev.off()
# }}}

# ------------------------------------------------------------------------------
# OLS Regression Analysis ------------------------------------------------------
# ------------------------------------------------------------------------------

# |Table 2 & A5 & A6| OLS for T1 -----------------------------------------------
t1_inc <- lm(redistribute ~ treatment + misgroup2 + treatment * misgroup2 + obj_decile + age + female + employ1 + marital + educ + house + minju + kukhim + ideo5 + merito + trust_gov + factor(province),  data = controlt1)

t1_gov <- lm(duty_gov ~ treatment + misgroup2 + treatment * misgroup2 + obj_decile + age + female + employ1 + marital + educ + house + minju + kukhim + ideo5 + merito + trust_gov + factor(province),  data = controlt1)

t1_tax <- lm(tax_pro ~ treatment + misgroup2 + treatment * misgroup2 + obj_decile + age + female + employ1 + marital + educ + house + minju + kukhim + ideo5 + merito + trust_gov + factor(province),  data = controlt1)

# Output to tex files
# {{{
models <- list(
    "Income Transfer" = t1_inc, 
    "Gov. Responsibility" = t1_gov, 
    "Progressive Tax" = t1_tax
    )

custom_list <- list(
  `Provine FE` = c("\\checkmark", "\\checkmark", "\\checkmark")
)

coef_map <- list(
  treatmentt1 = "Treatment", misgroup2Overestimate = "Overestimate",
  misgroup2Underestimate = "Underestimate", 
  `treatmentt1:misgroup2Underestimate` = "Treatment \\times Underestimate",
  `treatmentt1:misgroup2Overestimate` = "Treatment \\times Overestimate",
  obj_decile = "Income", age = "Age", female = "Female", employ1 = "Employment",
  marital = "Marital Status", educ = "Education", house = "Home Ownership",
  minju = "Liberal Party", kukhim = "Conservative Party", ideo5 = "Ideology",
  merito = "Meritocratic Belief", trust_gov = "Trust Government"
)

# Table A6
texreg(
  models,
  file = "../tables/tabA6.tex",
  custom.coef.map = coef_map,
  include.rsquared = FALSE,
  custom.gof.rows = custom_list,
  custom.title = "Full Regression Table",
  caption = "",
  label = "tabA6:T1full",
)

coef_map2 <- list(
  treatmentt1 = "Treatment", misgroup2Overestimate = "Overestimate",
  misgroup2Underestimate = "Underestimate", 
  `treatmentt1:misgroup2Underestimate` = "Treatment \\times Underestimate",
  `treatmentt1:misgroup2Overestimate` = "Treatment \\times Overestimate"
)
custom_list2 <- list(
  `Provine FE` = c("\\checkmark", "\\checkmark", "\\checkmark"),
  `Controls` = c("\\checkmark", "\\checkmark", "\\checkmark")
)

# Table 2
texreg(
  models,
  file = "../tables/tab2.tex",
  custom.coef.map = coef_map2,
  include.rsquared = FALSE,
  custom.gof.rows = custom_list2,
  custom.title = "Full Regression Table",
  caption = "",
  label = "tab2:T1main",
)
# }}}

# Misperception 1
t1_inc_mis1 <- lm(redistribute ~ treatment + misgroup2 + treatment * misgroup1 + obj_decile + age + female + employ1 + marital + educ + house + minju + kukhim + ideo5 + merito + trust_gov + factor(province),  data = controlt1)

t1_gov_mis1 <- lm(duty_gov ~ treatment + misgroup2 + treatment * misgroup1 + obj_decile + age + female + employ1 + marital + educ + house + minju + kukhim + ideo5 + merito + trust_gov + factor(province),  data = controlt1)

t1_tax_mis1 <- lm(tax_pro ~ treatment + misgroup2 + treatment * misgroup1 + obj_decile + age + female + employ1 + marital + educ + house + minju + kukhim + ideo5 + merito + trust_gov + factor(province),  data = controlt1)

# Output to tex files
# {{{
models <- list(
    "Income Transfer" = t1_inc_mis1, 
    "Gov. Responsibility" = t1_gov_mis1, 
    "Progressive Tax" = t1_tax_mis1
    )

custom_list <- list(
  `Provine FE` = c("\\checkmark", "\\checkmark", "\\checkmark")
)

coef_map <- list(
  treatmentt1 = "Treatment", misgroup2Overestimate = "Overestimate",
  misgroup2Underestimate = "Underestimate", 
  `treatmentt1:misgroup1Overestimate` = "Treatment \\times Overestimate",
  `treatmentt1:misgroup1Underestimate` = "Treatment \\times Underestimate",
  obj_decile = "Income", age = "Age", female = "Female", employ1 = "Employment",
  marital = "Marital Status", educ = "Education", house = "Home Ownership",
  minju = "Liberal Party", kukhim = "Conservative Party", ideo5 = "Ideology",
  merito = "Meritocratic Belief", trust_gov = "Trust Government"
)

# Table A??
texreg(
  models,
  file = "../tables/tab_mispercept1",
  custom.coef.map = coef_map,
  include.rsquared = FALSE,
  custom.gof.rows = custom_list,
  custom.title = "Full Regression Table",
  caption = "",
  label = "tab:mispeception1",
)
# }}}

# Interaction
t1_inc_con <- lm(redistribute ~ treatment + misgroup2 + treatment * mispercept + obj_decile + age + female + employ1 + marital + educ + house + minju + kukhim + ideo5 + merito + trust_gov + factor(province),  data = controlt1)

t1_gov_con <- lm(duty_gov ~ treatment + misgroup2 + treatment * mispercept + obj_decile + age + female + employ1 + marital + educ + house + minju + kukhim + ideo5 + merito + trust_gov + factor(province),  data = controlt1)

t1_tax_con <- lm(tax_pro ~ treatment + misgroup2 + treatment * mispercept + obj_decile + age + female + employ1 + marital + educ + house + minju + kukhim + ideo5 + merito + trust_gov + factor(province),  data = controlt1)

# Output to tex files
# {{{
models <- list(
    "Income Transfer" = t1_inc_con, 
    "Gov. Responsibility" = t1_gov_con, 
    "Progressive Tax" = t1_tax_con
    )

custom_list <- list(
  `Provine FE` = c("\\checkmark", "\\checkmark", "\\checkmark")
)

coef_map <- list(
  treatmentt1 = "Treatment", misgroup2Overestimate = "Overestimate",
  misgroup2Underestimate = "Underestimate", 
  `treatmentt1:mispercept` = "Treatment \\times Misperception",
  obj_decile = "Income", age = "Age", female = "Female", employ1 = "Employment",
  marital = "Marital Status", educ = "Education", house = "Home Ownership",
  minju = "Liberal Party", kukhim = "Conservative Party", ideo5 = "Ideology",
  merito = "Meritocratic Belief", trust_gov = "Trust Government"
)

# Table A??
texreg(
  models,
  file = "../tables/tab_continuous.tex",
  custom.coef.map = coef_map,
  include.rsquared = FALSE,
  custom.gof.rows = custom_list,
  custom.title = "Full Regression Table",
  caption = "",
  label = "tab:Continous_mispeception",
)
# }}}

# Treatment 2 ------------------------------------------------------------------


