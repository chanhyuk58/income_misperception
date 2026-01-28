library(dplyr)
library(broom)
library(stargazer)
library(xtable)
library(ggplot2)

# Data preprosessing -----------------------------------------------------------
raw <- read.csv("../data/정치관련 일반 성인 조사 rawdata.csv")

# tidying variables 
raw$female <- raw$female - 1
raw$employ1 <- ifelse(raw$employ < 4, 1, 0)
raw$marital <- ifelse(raw$marital > 1, 0, 1)
raw$house <- 2 - raw$house
# raw$tax_house <- 5 - raw$tax_house
raw <- raw %>% 
  mutate(manipulation = ifelse(group == (4 - check), 1, 0),
         mispercept = (sub_decile - obj_decile))

raw$mispercept1 <- ifelse(raw$mispercept < 0, -1, 
                          ifelse(raw$mispercept > 0, 1, 0)
                          )

raw$mispercept2 <- cut(x = raw$mispercept, 
                       breaks = c(-10, -2, 1, 10), 
                       labels = c(-1, 0, 1))
raw$mispercept2 <- as.character(raw$mispercept2)
raw$mispercept2 <- as.numeric(raw$mispercept2)

raw$minju <- ifelse(raw$party == 1, 1, 0)
raw$kukhim <- ifelse(raw$party == 2, 1, 0)
raw$minju2 <- ifelse(raw$party == 5 & raw$partylean == 1, 1, raw$minju)
raw$kukhim2 <- ifelse(raw$party == 5 & raw$partylean == 2, 1, raw$kukhim)

# subsetting data
## by group
# control <- raw[raw$group2 == "control",]
# t1 <- raw[raw$group2 == "t1", ]
# t2 <- raw[raw$group2 == "t2", ]
controlt1 <- raw[raw$group2 != "t2", ]
controlt2 <- raw[raw$group2 != "t1", ]

## by misperception
positive <- raw[raw$group2 != "t2" & raw$mispercept2 == 1, ]
negative <- raw[raw$group2 != "t2" & raw$mispercept2 == -1, ]
nomis <- raw[raw$group2 != "t2" & raw$mispercept2 == 0, ]

# Manipulation test subsets
negative2 <- negative[negative$manipulation == 1, ]
positive2 <- positive[positive$manipulation == 1, ]
nomis2 <- nomis[nomis$manipulation == 1, ]
# raw2 <- raw[raw$manipulation == 1, ]

# Descriptive Analysis ---------------------------------------------------------
## |Table A1| Summary statistics -----------------------------------------------
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

ggsave(pd, filename = "../figures/average_misperception.jpg")

write.table(description, file = "../data/misperception.csv", 
            sep = ",", quote = F, row.names = F)
xtable(description, digits = 2)

## |Figure 1| Histograms -------------------------------------------------------
p1 <- ggplot(data = raw) + 
  geom_bar(aes(x = obj_decile), stat = "count") +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  ylim(0,400) + 
  labs(x = "Objective Income Decile", y = "Count") +
  theme_minimal()
ggsave(p1, filename = "../figures/obj_decile_count.jpg")

p2 <- ggplot(data = raw) +
  geom_bar(aes(x = sub_decile), stat = "count") +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  ylim(0,400) +
  labs(x = "Subjective Income Decile", y = "Count") +
  theme_minimal()
ggsave(p2, filename = "../figures/sub_decile_count.jpg")

# p3 <- raw %>% 
#   group_by(sub_decile) %>% 
#   summarise(mean = mean(sure), 
#             se = sd(sure)) %>% 
#   ggplot(.) +
#   geom_bar(aes(x = sub_decile, y = mean), stat = "identity") +
#   geom_errorbar( aes(x=sub_decile, ymin=mean-1.65*se, ymax=mean+1.65*se), 
#                  width=0.4, colour="black", alpha=0.9, size=1.3) +
#   scale_x_continuous(breaks = seq(1, 10, by = 1)) +
#   ylim(c(0, 6)) +
#   labs(x = "Subjective Decile", y = "Degree of Certainty") +
#   theme(panel.grid.minor = element_blank(),
#         text = element_text(size=20))
# p3
# 
# ggsave(p3, filename = "sub_decile_sure.jpg")

# What determines sub_decile? -------------------------------------------------
# determinants_sub <- lm(sub_decile ~ obj_decile + factor(female) + factor(employ1)
#                        + factor(house) 
#                        + relevel(factor(party), ref = 5) 
#                        + ideo5 + factor(marital) 
#                        + age + gen_mobile + merito 
#                        , data = raw)
# 
# summary(determinants_sub)
# 
# stargazer(determinants_sub)

# |Table 2| balance test control var ------------------------------------------
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
t_control <- tidy_t.test(controlt1, controlvarlist)

xtable(t_control, digits = 2)
write.table(t_control[c(1,4:7)], 
            file = "../data/balance_con2.csv", 
            sep = ",", 
            quote = F, 
            row.names = F
            )

# manipulation checked
t_control2 <- tidy_t.test(controlt1[controlt1$manipulation == 1, ], controlvarlist)

xtable(t_control2, digits = 2)
write.table(t_control2[c(1,4:7)], file = "../data/balance_con3.csv", sep = ",", quote = F, row.names = F)

# DV t-tests ------------------------------------------------------------------
t_all_dv <- tidy_t.test(controlt1, dvlist)



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

# # |Table 4| & |Table A2| Regression --------------------------------------------
# lm11 <- lm(redistribute ~ 
#              relevel(factor(group2), ref = "control")*relevel(factor(group3), ref = "No Misperception") 
#            # + minju
#            # + kukhim
#            + obj_decile + age + female + marital + educ + house + employ1
#            # + ideo5 + merito + gen_mobile + trust_gov
#            + factor(province)
#            , data = controlt1)
#
# lm12 <- lm(redistribute ~ 
#              relevel(factor(group2), ref = "control")*relevel(factor(group3), ref = "No Misperception") 
#            + minju
#            + kukhim
#            + obj_decile + age + female + marital + educ + house + employ1
#            + ideo5 + merito + gen_mobile + trust_gov
#            + factor(province)
#            , data = controlt1)
#
# stargazer(lm11, lm12,
#           type = "text",
#           out = "../tables/province_fixed.html",
#           omit= "province",
#           order = c(1:3, 33:34, 4:19),
#           covariate.labels = c("treatment group", 
#                                "positive mispercept", 
#                                "negative mispercept",
#                                "treatment group x positive",
#                                "trestment group x negative"),
#           keep.stat = c("n", "rsq"))
