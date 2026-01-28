library("dplyr")
library("broom")
library("stargazer")
library("xtable")
library("ggplot2")
library("patchwork")
library("modelsummary")

source("./preprocessing.R")

# ------------------------------------------------------------------------------
# Descriptive Analysis ---------------------------------------------------------
# ------------------------------------------------------------------------------

## |Table A2| Summary statistics -----------------------------------------------
stargazer(raw, type = "text",
          out = "summary.html")

## |Table 1| obj_decile mean misperception, proportion of positive and negative 
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

write.table(description, file = "../data/misperception.csv", 
            sep = ",", quote = F, row.names = F)
description[is.na(description)] <- "-"
names(description) <- c("Obj Income", "Mean Subj Income", "Mean Diff", "\\% Over", "\\% Under")
tab1 <- xtable(description, digits = 2)
print(tab1, file = "../tables/table1.tex")

## |Figure 5| Histograms -------------------------------------------------------
p1 <- ggplot(data = raw) + 
  geom_bar(aes(x = obj_decile), stat = "count") +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  ylim(0,400) + 
  labs(x = "Objective Income Decile", y = "Count") +
  theme_minimal()

p2 <- ggplot(data = raw) +
  geom_bar(aes(x = sub_decile), stat = "count") +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  ylim(0,400) +
  labs(x = "Subjective Income Decile", y = "Count") +
  theme_minimal()

ggsave((p1 + p2), file = "../figures/fig5.pdf", width = 10, height = 5)

# |Table A3 & A4| balance test control var ------------------------------------------
### define user function
controlvarlist <- c("sub_decile", "obj_decile", "age", "female", "marital",
                    "educ", "house", "employ1", "ideo5", "minju", "kukhim", 
                    "trust_gov", "merito", "gen_mobile", "concern_pol",
                    "ineq_percept")

dvlist <- c("redistribute", "duty_gov", "tax_pro")

tidy_t.test <- function(target, varlist){
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

# all control + t1
t1_control <- tidy_t.test(controlt1, controlvarlist)
t1_control$varname <- c(
  "Subjective Income", "Objective Income", "Age", "Female", "Marital Status", 
  "Education", "Home Ownership", "Employment", "Ideology", "Liberal Party",
  "Conservative Party", "Government Trust", "Meritocracy", "Generational Mobility",
  "Concern Politics", "Inequality Perception"
)
names(t1_control) <- c("Variable", "Mean Diff", "SE", "Control Mean", "T1 Mean", "T-Stat", "p-value")
tabA3 <- xtable(t1_control[1:11, c(1, 4, 5, 7)], digits = 2)
print(tabA3, file = "../tables/tabA3_balance_T1.tex")

# all control + t2
t2_control <- tidy_t.test(controlt2, controlvarlist)
t2_control$varname <- c(
  "Subjective Income", "Objective Income", "Age", "Female", "Marital Status", 
  "Education", "Home Ownership", "Employment", "Ideology", "Liberal Party",
  "Conservative Party", "Government Trust", "Meritocracy", "Generational Mobility",
  "Concern Politics", "Inequality Perception"
)
names(t2_control) <- c("Variable", "Mean Diff", "SE", "Control Mean", "T2 Mean", "T-Stat", "p-value")
tabA4 <- xtable(t2_control[1:11, c(1, 4, 5, 7)], digits = 2)
print(tabA3, file = "../tables/tabA4_balance_T2.tex")

# # manipulation checked
# t_control2 <- tidy_t.test(controlt1[controlt1$manipulation == 1, ], controlvarlist)
#
# xtable(t_control2, digits = 2)
# write.table(t_control2[c(1,4:7)], file = "../data/balance_con3.csv", sep = ",", quote = F, row.names = F)

# |Table 3| --------------------------------------------------------------------
## Positive misperception mean difference --------------------------------------
### Control vars t-tests -------------------------------------------------------

# all control + t1
t_positive_control <- tidy_t.test(positive, controlvarlist)

write.table(t_positive_control, file = "../data/t_positive_control.csv", sep = ",", row.names = F, col.names = T)
xtable(t_positive_control, digits = 2)

# manipulation checked
t_positive2_control <- tidy_t.test(positive2, controlvarlist)

print(xtable(t_positive2_control), include.rownames=F)
write.table(t_positive2_control, file = "../data/t_positive_control.csv", sep = ",", row.names = F, col.names = T)
### DV t-tests ------------------------------------------------------------------
t_positive_dv <- tidy_t.test(positive, dvlist)
write.table(t_positive_dv[c(1,4:5,7)], file = "../data/t_positive_dv.csv", sep = ",", row.names = F)
xtable(t_positive_dv, digits = 2)

# Manipulation checked
t_positive2_dv <- tidy_t.test(positive2, dvlist)
write.table(t_positive2_dv[c(1,4:5,7)], file = "../data/t_positive_dv.csv", sep = ",", row.names = F)
xtable(t_positive2_dv, digits = 2)

## Negative misperception mean difference --------------------------------------
### Control vars t-tests --------------------------------------------------------
# all control + t1
t_negative_control <- tidy_t.test(negative, controlvarlist)

write.table(t_negative_control, file = "../data/t_negative_control.csv", sep = ",", row.names = F, col.names = T)
xtable(t_negative_control, digits = 2)

# manipulation checked
t_negative2_control <- tidy_t.test(negative2, controlvarlist)

write.table(t_negative2_control, file = "../data/t_negative2_control.csv", sep = ",", row.names = F, col.names = T)
xtable(t_negative_control, digits = 2)

### DV t-tests ------------------------------------------------------------------
# all control + t1
t_negative_dv <- tidy_t.test(negative, dvlist)

xtable(t_negative_dv, digits = 2)
write.table(t_negative_dv[c(1,4:5,7)], file = "../data/t_neg_dv.csv", sep = ",", row.names = F)

# Manipulation checked
t_negative2_dv <- tidy_t.test(negative2, dvlist)
print(xtable(t_negative2_dv), include.rownames=F)
xtable(t_negative2_dv, digits = 2)

## No misperception mean difference --------------------------------------------
### Control vars t-tests --------------------------------------------------------
# all control + t1
t_nomis_control <- tidy_t.test(nomis, controlvarlist)

write.table(t_nomis_control, file = "../data/t_nomis_control.csv", sep = ",", row.names = F, col.names = T)
xtable(t_nomis_control, digits = 2)

# manipulation checked
t_nomis2_control <- tidy_t.test(nomis2, controlvarlist)

write.table(t_nomis2_control, file = "../data/t_nomis2_control.csv", sep = ",", row.names = F, col.names = T)
xtable(t_nomis2_control, digits = 2)

### DV t-tests -----------------------------------------------------------------
# all control + t1
t_nomis_dv <- tidy_t.test(nomis, dvlist)
xtable(t_nomis_dv, digits = 2)
write.table(t_nomis_dv[c(1, 4:5, 7)], file = "../data/t_no_dv.csv", sep = ",", row.names = F)

# Manipulation checked
t_nomis2_dv <- tidy_t.test(nomis2, dvlist)
xtable(t_nomis2_dv, digits = 2)
write.table(t_nomis2_dv[c(1:5, 7)], file = "../data/t_no2_dv.csv", sep = ",", row.names = F)

## Falsification Test ----------------------------------------------------------
datalist <- c("positive", "negative", "nomis", "controlt1")
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
falsification <- t(falsification)

write.table(falsification, file = "../data/falsification.csv", sep = ",", 
            quote = F, row.names = T)

# |Figure 6 - 8| -------------------------------------------------------------------
controlt1$group3 <- ifelse(controlt1$mispercept2 == 0, "No Misperception", 
                           ifelse(controlt1$mispercept2 == 1, "Positive Misperception", "Negative Misperception")) 

controlt1 %>% 
  group_by(group2, group3) %>% 
  summarize(mean = mean(redistribute),
            se = sd(redistribute)/sqrt(length(redistribute)),
            ) %>% 
  ggplot(aes(x = group2, color = group2)
         ) +
  facet_wrap( ~ factor(group3, levels = c("Negative Misperception", "No Misperception", "Positive Misperception"))) +
  geom_point(aes(y = mean)) +
  geom_errorbar(aes(ymin = mean - se , ymax = mean + se),
                width = 0.3) +
  scale_x_discrete("", labels = c("control" = "Control", "t1" = "T1")) + 
  ylab("Redistribution Preference -- Income Transfer") +
  theme_minimal() +
  theme(legend.position = "none") + 
  scale_color_brewer(palette = "Set2")

ggsave(filename = "../figures/fig_8_income_transfer.pdf",
       height = 5,
       width = 6)

controlt1 %>% 
  group_by(group2, group3) %>% 
  summarize(mean = mean(tax_pro),
            se = sd(tax_pro)/sqrt(length(tax_pro)),
            ) %>% 
  ggplot(aes(x = group2, color = group2)
  ) +
  facet_wrap( ~ factor(group3, levels = c("Negative Misperception", "No Misperception", "Positive Misperception"))) +
  geom_point(aes(y = mean)) +
  geom_errorbar(aes(ymin = mean - se , ymax = mean + se),
                width = 0.3) +
  scale_x_discrete("", labels = c("control" = "Control", "t1" = "T1")) + 
  ylab("Redistribution Preference -- Progressive Tax") +
  theme_minimal() +
  theme(legend.position = "none") + 
  scale_color_brewer(palette = "Set2")

ggsave(filename = "../figures/fig_7_progressive_tax.pdf",
       height = 5,
       width = 6)

controlt1 %>% 
  group_by(group2, group3) %>% 
  summarize(mean = mean(duty_gov),
            se = sd(duty_gov)/sqrt(length(duty_gov)),
            group3 = group3) %>% 
  ggplot(aes(x = group2, color = group2)
  ) +
  facet_wrap( ~ factor(group3, levels = c("Negative Misperception", "No Misperception", "Positive Misperception"))) +
  geom_point(aes(y = mean)) +
  geom_errorbar(aes(ymin = mean - se , ymax = mean + se),
                width = 0.3) +
  ylab("Redistribution Preference -- Government Responsibility") +
  scale_x_discrete("", labels = c("control" = "Control", "t1" = "T1")) + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  scale_color_brewer(palette = "Set2")

ggsave(filename = "../figures/fig_6_gov_responsibility.pdf", 
       height = 5,
       width = 6)
