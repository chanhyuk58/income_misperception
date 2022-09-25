library(tidyverse)
library(broom)
library(stargazer)
library(xtable)

theme_set(theme_grey(base_family='NanumGothic'))

# Data preprosessing -----------------------------------------------------------
raw <- read.csv("data/정치관련 일반 성인 조사 rawdata.csv")

# tidying variables 
raw$female <- raw$female - 1
raw$employ1 <- ifelse(raw$employ < 4, 1, 0)
raw$marital <- ifelse(raw$marital > 1, 0, 1)
raw$house <- 2 - raw$house
raw$tax_house <- 5 - raw$tax_house
raw <- raw %>% 
  mutate(manipulation = ifelse(group == (4 - check), 1, 0),
         mispercept = (sub_decile - obj_decile))

raw$mispercept2 <- cut(x = raw$mispercept, 
                       breaks = c(-10, -2, 1, 10), 
                       labels = c(-1, 0, 1))
raw$mispercept2 <- as.character(raw$mispercept2)
raw$mispercept2 <- as.numeric(raw$mispercept2)

raw$minju <- ifelse(raw$party == 1, 1, 0)
raw$kukhim <- ifelse(raw$party == 2, 1, 0)

# subsetting data
control <- raw[raw$group2 == "control",]
t1 <- raw[raw$group2 == "t1", ]
t2 <- raw[raw$group2 == "t2", ]

# Manipulation test subsets
negative2 <- negative[negative$manipulation == 1, ]
positive2 <- positive[positive$manipulation == 1, ]
nomis2 <- nomis[nomis$manipulation == 1, ]
raw2 <- raw[raw$manipulation == 1, ]

# description ------------------------------------------------------------------
## Summary statistics ----------------------------------------------------------
stargazer(raw, type = "text",
          out = "summary.html")
## obj_decile mean misperception, proportion of positive and negative misperception
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
  geom_bar(aes(y = mean_dif, x = obj_decile), stat = "identity")

write.table(description, file = "misperception.csv", 
            sep = ",", quote = FALSE, row.names = F)
xtable(description)
print.xtable(xtable(description, digits = 2))

controlt1 <- rbind(control, t1)

controlt1_aov <- aov(duty_gov ~ factor(group)
                         + obj_decile + sub_decile + age 
                         + female + marital + employ1
                         + educ 
                         + house
                         + ideo5 
                         + relevel(factor(party), ref = 5)
                         + merito 
                         + gen_mobile
                         + trust_gov
                         + ineq_percept,
                         data = controlt1)
summary(controlt1_aov)

## balance test control var ----------------------------------------------------

controlvarlist <- c("sub_decile", "obj_decile", "age", "female", "marital",
                    "educ", "house", "employ1", "party", "ideo5", "trust_gov",
                    "merito", "gen_mobile", "concern_pol",
                    "ineq_percept")

t_control <- data.frame()
for(i in controlvarlist){
  temp <- eval(parse(text = sub("k", i, "t.test(k ~ group, data = controlt1)")))
  temp2 <- tidy(temp)[1:5]
  names(temp2) <- c("mean_dif", "control_mean", "t1_mean", "t-stat", "p-value")
  temp2$varname <- i
  temp2$se <- temp$stderr
  temp2 <- temp2[c(6,1,7,2:5)]
  assign("t_control", rbind(t_control, temp2)) 
}

print(xtable(t_control), include.rownames=FALSE)
write.table(t_control[c(1,4:7)], file = "balance_con2.csv", sep = ",", quote = FALSE, row.names = F)

# manipulated
t_control2 <- data.frame()
for(i in controlvarlist){
  temp <- eval(parse(text = sub("k", i, "t.test(k ~ group, data = controlt1[controlt1$manipulation == 1, ])")))
  temp2 <- tidy(temp)[1:5]
  names(temp2) <- c("mean_dif", "control_mean", "t1_mean", "t-stat", "p-value")
  temp2$varname <- i
  temp2$se <- temp$stderr
  temp2 <- temp2[c(6,1,7,2:5)]
  assign("t_control2", rbind(t_control2, temp2)) 
}

print(xtable(t_control2), include.rownames=FALSE)
write.table(t_control2[c(1,4:7)], file = "balance_con3.csv", sep = ",", quote = FALSE, row.names = F)

## balance test dependent var --------------------------------------------------
dvlist <- c("redistribute", "tax_pro", "duty_gov")

t_all_dv <- data.frame()
for(i in dvlist){
  temp <- eval(parse(text = sub("k", i, "t.test(k ~ group, data = controlt1)")))
  temp2 <- tidy(temp)[1:5]
  names(temp2) <- c("mean_dif", "control_mean", "t1_mean", "t-stat", "p-value")
  temp2$varname <- i
  temp2$se <- temp$stderr
  temp2 <- temp2[c(6,1,7,2:5)]
  assign("t_all_dv", rbind(t_all_dv, temp2)) 
}
write.table(t_all_dv, file = "t_all_dv.csv", sep = ",", row.names = F, col.names = T)
print(xtable(t_all_dv), include.rownames=FALSE)




## Histograms ------------------------------------------------------------------

p1 <- ggplot(data = raw) + 
  geom_bar(aes(x = obj_decile), stat = "count") +
  geom_bar(aes(x = sub_decile, fill = element_blank()), stat = "count") + 
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  ylim(0,400) + 
  labs(x = "소득 위치", y = "빈도") +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size=20))
p1

ggsave(p1, filename = "obj_decile_count.jpg")

p2 <- ggplot(data = raw) +
  geom_bar(aes(x = sub_decile), stat = "count") +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  ylim(0,400) +
  labs(x = "주관적 소득 위치", y = "빈도") +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size=20))
# p2

ggsave(p2, filename = "sub_decile_count.jpg")

p3 <- raw %>% 
  group_by(sub_decile) %>% 
  summarise(mean = mean(sure), 
            se = sd(sure)) %>% 
  ggplot(.) +
  geom_bar(aes(x = sub_decile, y = mean), stat = "identity") +
  geom_errorbar( aes(x=sub_decile, ymin=mean-1.65*se, ymax=mean+1.65*se), 
                 width=0.4, colour="black", alpha=0.9, size=1.3) +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  ylim(c(0, 6)) +
  labs(x = "주관적 소득 위치", y = "주관적 소득 위치 인식에 대해 자신하는 정도") +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size=20))
p3

ggsave(p3, filename = "sub_decile_sure.jpg")

# What determines sub_decile? -------------------------------------------------
determinants_sub <- lm(sub_decile ~ obj_decile + factor(female) + factor(employ1)
                       + factor(house) 
                       + relevel(factor(party), ref = 5) 
                       + ideo5 + factor(marital) 
                       + age + gen_mobile + merito 
                       , data = raw)

summary(determinants_sub)

stargazer(determinants_sub)

# Positive misperception mean difference ---------------------------------------
controlP <- control[control$mispercept2 == 1, ]
t1P <- t1[t1$mispercept2 == 1, ]
positive <- rbind(controlP, t1P)

## Control vars t-tests --------------------------------------------------------
controlvarlist <- c("sub_decile", "obj_decile", "age", "female", "marital",
                       "educ", "house", "employ1", "ideo5", "trust_gov",
                       "merito", "gen_mobile", "concern_pol",
                       "ineq_percept")

t_positive_control <- data.frame()
for(i in controlvarlist){
  temp <- eval(parse(text = sub("k", i, "t.test(k ~ group, data = positive)")))
  temp2 <- tidy(temp)[1:5]
  names(temp2) <- c("mean_dif", "control_mean", "t1_mean", "t-stat", "p-value")
  temp2$varname <- i
  temp2$se <- temp$stderr
  temp2 <- temp2[c(6,1,7,2:5)]
  assign("t_positive_control", rbind(t_positive_control, temp2)) 
  }

write.table(t_positive_control, file = "t_positive_control.csv", sep = ",", row.names = F, col.names = T)
print(xtable(t_positive_control), include.rownames=FALSE)

t_positive2_control <- data.frame()
for(i in controlvarlist){
  temp <- eval(parse(text = sub("k", i, "t.test(k ~ group, data = positive2)")))
  temp2 <- tidy(temp)[1:5]
  names(temp2) <- c("mean_dif", "control_mean", "t1_mean", "t-stat", "p-value")
  temp2$varname <- i
  temp2$se <- temp$stderr
  temp2 <- temp2[c(6,1,7,2:5)]
  assign("t_positive2_control", rbind(t_positive2_control, temp2)) 
  }

print(xtable(t_positive2_control), include.rownames=FALSE)
write.table(t_positive_control, file = "t_positive_control.csv", sep = ",", row.names = F, col.names = T)
## DV t-tests ------------------------------------------------------------------
dvlist <- c("redistribute", "tax_pro", "tax_house", "duty_gov", "welfare_select")

t_positive_dv <- data.frame()
for(i in dvlist){
  temp <- eval(parse(text = sub("k", i, "t.test(k ~ group, data = positive)")))
  temp2 <- tidy(temp)[1:5]
  names(temp2) <- c("mean_dif", "control_mean", "t1_mean", "t-stat", "p-value")
  temp2$varname <- i
  temp2$se <- temp$stderr
  temp2 <- temp2[c(6,1,7,2:5)]
  assign("t_positive_dv", rbind(t_positive_dv, temp2)) 
}
write.table(t_positive_dv[c(1,4:5,7)], file = "t_positive_dv.csv", sep = ",", row.names = F)
print(xtable(t_positive_dv), include.rownames=FALSE)

# Manipulation checked
t_positive2_dv <- data.frame()
for(i in dvlist){
  temp <- eval(parse(text = sub("k", i, "t.test(k ~ group, data = positive2)")))
  temp2 <- tidy(temp)[1:5]
  names(temp2) <- c("mean_dif", "t1_mean", "control_mean", "t-stat", "p-value")
  temp2$varname <- i
  temp2$se <- temp$stderr
  temp2 <- temp2[c(6,1,7,2:5)]
  assign("t_positive2_dv", rbind(t_positive2_dv, temp2)) 
}
print(xtable(t_positive2_dv), include.rownames=FALSE)
write.table(t_positive2_dv[c(1:5,7)], file = "t_positive2_dv.csv", sep = ",", row.names = F)

# Negative misperception mean difference ---------------------------------------
controlN <- control[control$mispercept2 == -1, ]
t1N <- t1[t1$mispercept2 == -1, ]
negative <- rbind(controlN, t1N)

## Control vars t-tests --------------------------------------------------------
controlvarlist <- c("sub_decile", "obj_decile", "age", "female", "marital",
                    "educ", "house", "employ1", "ideo5", "trust_gov",
                    "merito", "gen_mobile", "concern_pol",
                    "ineq_percept")

t_negative_control <- data.frame()
for(i in controlvarlist){
  temp <- eval(parse(text = sub("k", i, "t.test(k ~ group, data = negative)")))
  temp2 <- tidy(temp)[1:5]
  names(temp2) <- c("mean_dif", "control_mean", "t1_mean", "t-stat", "p-value")
  temp2$varname <- i
  temp2$se <- temp$stderr
  temp2 <- temp2[c(6,1,7,2:5)]
  assign("t_negative_control", rbind(t_negative_control, temp2)) 
}

write.table(t_negative_control, file = "t_negative_control.csv", sep = ",", row.names = F, col.names = T)
print(xtable(t_negative_control), include.rownames=FALSE)

t_negative2_control <- data.frame()
for(i in controlvarlist){
  temp <- eval(parse(text = sub("k", i, "t.test(k ~ group, data = negative2)")))
  temp2 <- tidy(temp)[1:5]
  names(temp2) <- c("mean_dif", "t1_mean", "control_mean", "t-stat", "p-value")
  temp2$varname <- i
  temp2$se <- temp$stderr
  temp2 <- temp2[c(6,1,7,2:5)]
  assign("t_negative2_control", rbind(t_negative2_control, temp2)) 
}

write.table(t_negative2_control, file = "t_negative2_control.csv", sep = ",", row.names = F, col.names = T)
print(xtable(t_negative_control), include.rownames=FALSE)

## DV t-tests ------------------------------------------------------------------
dvlist <- c("redistribute", "tax_pro", "tax_house", "duty_gov", "welfare_select")

t.test(duty_gov ~ group, data = positive)

t_negative_dv <- data.frame()
for(i in dvlist){
  temp <- eval(parse(text = sub("k", i, "t.test(k ~ group, data = negative)")))
  temp2 <- tidy(temp)[1:5]
  names(temp2) <- c("mean_dif", "t1_mean", "control_mean", "t-stat", "p-value")
  temp2$varname <- i
  temp2$se <- temp$stderr
  temp2 <- temp2[c(6,1,7,2:5)]
  assign("t_negative_dv", rbind(t_negative_dv, temp2)) 
}
print(xtable(t_negative_dv), include.rownames=FALSE)
write.table(t_negative_dv[c(1,4:5,7)], file = "t_neg_dv.csv", sep = ",", row.names = F)

# Manipulation checked
t_negative2_dv <- data.frame()
for(i in dvlist){
  temp <- eval(parse(text = sub("k", i, "t.test(k ~ group, data = negative2)")))
  temp2 <- tidy(temp)[1:5]
  names(temp2) <- c("mean_dif", "control_mean", "t1_mean", "t-stat", "p-value")
  temp2$varname <- i
  temp2$se <- temp$stderr
  temp2 <- temp2[c(6,1,7,2:5)]
  assign("t_negative2_dv", rbind(t_negative2_dv, temp2)) 
}
print(xtable(t_negative2_dv), include.rownames=FALSE)
write.table(t_negative2_dv[c(1:5,7)], file = "t_neg2_dv.csv", sep = ",", row.names = F)

# No misperception mean difference ---------------------------------------
controlNN <- control[control$mispercept2 == 0, ]
t1NN <- t1[t1$mispercept2 == 0, ]
nomis <- rbind(controlNN, t1NN)

## Control vars t-tests --------------------------------------------------------
controlvarlist <- c("sub_decile", "obj_decile", "age", "female", "marital",
                    "educ", "house", "employ1", "ideo5", "trust_gov",
                    "merito", "gen_mobile", "concern_pol",
                    "ineq_percept")

t_nomis_control <- data.frame()
for(i in controlvarlist){
  temp <- eval(parse(text = sub("k", i, "t.test(k ~ group, data = nomis)")))
  temp2 <- tidy(temp)[1:5]
  names(temp2) <- c("mean_dif", "control_mean", "t1_mean", "t-stat", "p-value")
  temp2$varname <- i
  temp2$se <- temp$stderr
  temp2 <- temp2[c(6,1,7,2:5)]
  assign("t_nomis_control", rbind(t_nomis_control, temp2)) 
}

write.table(t_nomis_control, file = "t_nomis_control.csv", sep = ",", row.names = F, col.names = T)
print(xtable(t_nomis_control), include.rownames=FALSE)

## DV t-tests ------------------------------------------------------------------
dvlist <- c("redistribute", "tax_pro", "tax_house", "duty_gov", "welfare_select")

t_nomis_dv <- data.frame()
for(i in dvlist){
  temp <- eval(parse(text = sub("k", i, "t.test(k ~ group, data = nomis)")))
  temp2 <- tidy(temp)[1:5]
  names(temp2) <- c("mean_dif", "control_mean", "t1_mean", "t-stat", "p-value")
  temp2$varname <- i
  temp2$se <- temp$stderr
  temp2 <- temp2[c(6,1,7,2:5)]
  assign("t_nomis_dv", rbind(t_nomis_dv, temp2)) 
}
print(xtable(t_nomis_dv), include.rownames=FALSE)
write.table(t_nomis_dv[c(1, 4:5, 7)], file = "t_no_dv.csv", sep = ",", row.names = F)

# Manipulation checked
t_nomis2_dv <- data.frame()
for(i in dvlist){
  temp <- eval(parse(text = sub("k", i, "t.test(k ~ group, data = nomis2)")))
  temp2 <- tidy(temp)[1:5]
  names(temp2) <- c("mean_dif", "t1_mean", "control _mean", "t-stat", "p-value")
  temp2$varname <- i
  temp2$se <- temp$stderr
  temp2 <- temp2[c(6,1,7,2:5)]
  assign("t_nomis2_dv", rbind(t_nomis2_dv, temp2)) 
}
print(xtable(t_nomis2_dv), include.rownames=FALSE)
write.table(t_nomis2_dv[c(1:5, 7)], file = "t_no2_dv.csv", sep = ",", row.names = F)



# Falsification Test -----------------------------------------------------------
t.test(donation ~ group, data = positive)
t.test(donation ~ group, data = positive[positive$manipulation == 1, ])$stderr
t.test(donation ~ group, data = negative)
t.test(donation ~ group, data = negative[negative$manipulation == 1, ])$stderr
t.test(donation ~ group, data = nomis)
t.test(donation ~ group, data = nomis[nomis$manipulation == 1, ])$stderr
t.test(donation ~ group, data = controlt1)
t.test(donation ~ group, data = controlt1[controlt1$manipulation == 1, ])

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

write.table(falsification, file = "falsification.csv", sep = ",", 
            quote = FALSE, row.names = T)

# graph -----
positive$group3 <- "상향 오인"
negative$group3 <-  "하향 오인"
nomis$group3 <- "오인 없음"
# positive$group3 <- paste0("pos_", positive$group2)
# negative$group3 <- paste0("neg_", negative$group2)
# nomis$group3 <- paste0("nom_", nomis$group2)

controlt1_2 <- bind_rows(positive, negative, nomis)

controlt1_2[controlt1_2$manipulation == 1, ] %>% 
  group_by(group, group3) %>% 
  summarize(mean = mean(redistribute),
            se = sd(redistribute)/sqrt(length(redistribute)),
            ) %>% 
  ggplot(data =., 
         aes(x = factor(group, levels = c("3", "1")), color = as.factor(group))
         ) +
  facet_wrap( ~ factor(group3, levels = c("하향 오인", "오인 없음", "상향 오인"))) +
  geom_point(aes(y = mean)) +
  # geom_bar(aes(x = factor(group), y = mean), stat = "identity") +
  geom_errorbar(aes(ymin = mean - se , ymax = mean + se),
                width = 0.3) +
  scale_x_discrete("", labels = c("3" = "통제집단", "1" = "처치집단1")) + 
  # ylim(3,6) +
  ylab("재분배선호 - 소득이전") +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "grey97")) + 
  scale_color_manual(values = c("black", "grey60"))
ggsave(filename = "소득이전_비교2.png",
       height = 3,
       width = 6)

controlt1_2[controlt1_2$manipulation == 1, ] %>% 
  group_by(group, group3) %>% 
  summarize(mean = mean(tax_pro),
            se = sd(tax_pro)/sqrt(length(tax_pro)),
            ) %>% 
  ggplot(data =., 
         aes(x = factor(group, levels = c("3", "1")), color = as.factor(group))
  ) +
  facet_wrap( ~ factor(group3, levels = c("하향 오인", "오인 없음", "상향 오인"))) +
  geom_point(aes(y = mean)) +
  # geom_bar(aes(x = factor(group), y = mean), stat = "identity") +
  geom_errorbar(aes(ymin = mean - se , ymax = mean + se),
                width = 0.3) +
  scale_x_discrete("", labels = c("3" = "통제집단", "1" = "처치집단1")) + 
  # ylim(3,4) +
  ylab("재분배선호 - 누진세 강화") +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "grey97")) + 
  scale_color_manual(values = c("black", "grey60"))
ggsave(filename = "누진세강화_비교2.png",
       height = 3,
       width = 6)

controlt1_2[controlt1_2$manipulation == 1, ] %>% 
  group_by(group, group3) %>% 
  summarize(mean = mean(duty_gov),
            se = sd(duty_gov)/sqrt(length(duty_gov)),
            group3 = group3) %>% 
  ggplot(data =., 
         aes(x = factor(group, levels = c("3", "1")), color = as.factor(group))
  ) +
  facet_wrap( ~ factor(group3, levels = c("하향 오인", "오인 없음", "상향 오인"))) +
  geom_point(aes(y = mean)) +
  # geom_bar(aes(x = factor(group), y = mean), stat = "identity") +
  geom_errorbar(aes(ymin = mean - se , ymax = mean + se),
                width = 0.3) +
  scale_x_discrete("", labels = c("3" = "통제집단", "1" = "처치집단1")) + 
  # ylim(3,6) +
  ylab("재분배선호 - 정부책임") +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "grey97")) + 
  scale_color_manual(values = c("black", "grey60"))
ggsave(filename = "정부책입_비교2.png", 
       height = 3,
       width = 6)

# Regression -------------------
lm1 <- lm(duty_gov ~ relevel(factor(group), ref = "3")
          + obj_decile + age + female + marital + educ + house + employ1
          # + ideo5 + merito + gen_mobile
            , data = controlt1_2)

lm2 <- lm(duty_gov ~ relevel(factor(group), ref = "3") 
          + obj_decile + age + female + marital + educ + house + employ1
          + ideo5 + merito + gen_mobile + trust_gov
          , data = controlt1_2)

lm3 <- lm(duty_gov ~ relevel(factor(group), ref = "3") 
          + relevel(factor(group3), ref = "오인 없음") 
          + obj_decile + age + female + marital + educ + house + employ1
          # + ideo5 + merito + gen_mobile
          , data = controlt1_2)

lm4 <- lm(duty_gov ~ relevel(factor(group), ref = "3") 
          + relevel(factor(group3), ref = "오인 없음") 
          + sub_decile + obj_decile + age + female + marital + educ + house 
          + employ1 + ideo5 + merito + gen_mobile + trust_gov
          , data = controlt1_2)

stargazer(lm1, lm2, lm3, lm4,
          out = "11_duty_gov.html",
          covariate.labels = c("treatment group", 
                               "positive mispercept", 
                               "negative mispercept"),
          keep.stat = c("n", "rsq"))



lm5 <- lm(duty_gov ~ 
            relevel(factor(group), ref = "3")*relevel(factor(group3), ref = "오인 없음") 
          + obj_decile + age + female + marital + educ + house + employ1
          # + ideo5 + merito + gen_mobile + trust_gov
          , data = controlt1_2)

lm6 <- lm(duty_gov ~ 
            relevel(factor(group), ref = "3")*relevel(factor(group3), ref = "오인 없음") 
          + obj_decile + age + female + marital + educ + house + employ1
          + ideo5 + merito + gen_mobile + trust_gov
          , data = controlt1_2)

stargazer(lm5, lm6,
          out = "11_int.html",
          order = c(1:3, 15:16, 4:14),
          covariate.labels = c("treatment group", 
                               "positive mispercept", 
                               "negative mispercept",
                               "treatment group x positive",
                               "trestment group x negative"),
          keep.stat = c("n", "rsq"))

controlt1_2$province_yeongnam <- ifelse(controlt1_2$province == 14 | controlt1_2$province == 15, 1, 0)
controlt1_2$province_honam <- ifelse(controlt1_2$province == 12 | controlt1_2$province == 13, 1, 0)

lm7 <- lm(redistribute ~ 
            relevel(factor(group), ref = "3")*relevel(factor(group3), ref = "오인 없음")
          + province_yeongnam
          + obj_decile + age + female + marital + educ + house + employ1
          # + ideo5 + merito + gen_mobile + trust_gov
          , data = controlt1_2)

lm8 <- lm(redistribute ~ 
            relevel(factor(group), ref = "3")*relevel(factor(group3), ref = "오인 없음") 
          + province_honam
          + obj_decile + age + female + marital + educ + house + employ1
          # + ideo5 + merito + gen_mobile + trust_gov
          , data = controlt1_2)

stargazer(lm7, lm8,
          out = "yeongnam_honam_int.html",
          order = c(1:3, 13:14, 4:12),
          covariate.labels = c("treatment group", 
                               "positive mispercept", 
                               "negative mispercept",
                               "treatment group x positive",
                               "trestment group x negative"),
          keep.stat = c("n", "rsq"))

controlt1_2$minju <- ifelse(controlt1_2$party == 1, 1, 0)
controlt1_2$kukhim <- ifelse(controlt1_2$party == 2, 1, 0)

controlt1_2$minju2 <- ifelse(controlt1_2$party == 1 | controlt1_2$partylean == 1, 1, 0)
controlt1_2$kukhim2 <- ifelse(controlt1_2$party == 2 | controlt1_2$partylean == 2, 1, 0)

lm9 <- lm(redistribute ~ 
            relevel(factor(group), ref = "3")*relevel(factor(group3), ref = "오인 없음")
          + minju
          + obj_decile + age + female + marital + educ + house + employ1
          # + ideo5 + merito + gen_mobile + trust_gov
          , data = controlt1_2)

lm10 <- lm(redistribute ~ 
            relevel(factor(group), ref = "3")*relevel(factor(group3), ref = "오인 없음") 
          + kukhim
          + obj_decile + age + female + marital + educ + house + employ1
          # + ideo5 + merito + gen_mobile + trust_gov
          , data = controlt1_2)

stargazer(lm9, lm10,
          out = "minju_kukhim_int.html",
          order = c(1:3, 13:14, 4:12),
          covariate.labels = c("treatment group", 
                               "positive mispercept", 
                               "negative mispercept",
                               "treatment group x positive",
                               "trestment group x negative"),
          keep.stat = c("n", "rsq"))

lm11 <- lm(redistribute ~ 
             relevel(factor(group), ref = "3")*relevel(factor(group3), ref = "오인 없음") 
           # + minju
           # + kukhim
           + obj_decile + age + female + marital + educ + house + employ1
           # + ideo5 + merito + gen_mobile + trust_gov
           , data = controlt1_2[controlt1_2$manipulation == 1, ])

lm12 <- lm(redistribute ~ 
             relevel(factor(group), ref = "3")*relevel(factor(group3), ref = "오인 없음") 
           + minju
           + kukhim
           + obj_decile + age + female + marital + educ + house + employ1
           + ideo5 + merito + gen_mobile + trust_gov
           + factor(province)
           , data = controlt1_2[controlt1_2$manipulation == 1, ])

stargazer(lm11, lm12,
          out = "province_fixed_mani.html",
          omit= "province",
          order = c(1:3, 33:34, 4:19),
          covariate.labels = c("treatment group", 
                               "positive mispercept", 
                               "negative mispercept",
                               "treatment group x positive",
                               "trestment group x negative"),
          keep.stat = c("n", "rsq"))


# T2 ---------------------------------------------------------------------------
## graph -----
controlt2 %>% 
  group_by(group) %>% 
  summarize(mean = mean(redistribute),
            se = sd(redistribute)/sqrt(length(redistribute)),
  ) %>% 
  ggplot(data =., 
         aes(x = factor(group, levels = c("3", "2")), color = as.factor(group))
  ) +
  # facet_wrap( ~ factor(group3, levels = c("하향 오인", "오인 없음", "상향 오인"))) +
  geom_point(aes(y = mean)) +
  # geom_bar(aes(x = factor(group), y = mean), stat = "identity") +
  geom_errorbar(aes(ymin = mean - se , ymax = mean + se),
                width = 0.3) +
  scale_x_discrete("", labels = c("3" = "통제집단", "2" = "처치집단2")) + 
  # ylim(3,6) +
  ylab("재분배선호 - 소득이전") +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "grey97")) + 
  scale_color_manual(values = c("black", "grey60"))
ggsave(filename = "소득이전_비교2.png",
       height = 3,
       width = 6)

## t-test ----
t2_dv <- data.frame()
for(i in dvlist){
  temp <- eval(parse(text = sub("k", i, "t.test(k ~ group, data = controlt2)")))
  temp2 <- tidy(temp)[1:5]
  names(temp2) <- c("mean_dif", "control_mean", "t1_mean", "t-stat", "p-value")
  temp2$varname <- i
  temp2$se <- temp$stderr
  temp2 <- temp2[c(6,1,7,2:5)]
  assign("t2_dv", rbind(t2_dv, temp2)) 
}
print(xtable(t2_dv), include.rownames=FALSE)
write.table(t2_dv[c(1,4:5,7)], file = "t2_dv.csv", sep = ",", quote = FALSE, row.names = F)


t2_neg_dv <- data.frame()
for(i in dvlist){
  temp <- eval(parse(text = sub("k", i, "t.test(k ~ group, data = controlt2[controlt2$mispercept2 == -1,])")))
  temp2 <- tidy(temp)[1:5]
  names(temp2) <- c("mean_dif", "control_mean", "t1_mean", "t-stat", "p-value")
  temp2$varname <- i
  temp2$se <- temp$stderr
  temp2 <- temp2[c(6,1,7,2:5)]
  assign("t2_neg_dv", rbind(t2_neg_dv, temp2)) 
}
write.table(t2_neg_dv[c(1,4:5,7)], file = "t2_neg_dv.csv", sep = ",", quote = FALSE, row.names = F)

t2_pos_dv <- data.frame()
for(i in dvlist){
  temp <- eval(parse(text = sub("k", i, "t.test(k ~ group, data = controlt2[controlt2$mispercept2 == 1,])")))
  temp2 <- tidy(temp)[1:5]
  names(temp2) <- c("mean_dif", "control_mean", "t1_mean", "t-stat", "p-value")
  temp2$varname <- i
  temp2$se <- temp$stderr
  temp2 <- temp2[c(6,1,7,2:5)]
  assign("t2_pos_dv", rbind(t2_pos_dv, temp2)) 
}
write.table(t2_pos_dv[c(1, 4:5, 7)], file = "t2_pos_dv.csv", sep = ",", quote = FALSE, row.names = F)


t2_no_dv <- data.frame()
for(i in dvlist){
  temp <- eval(parse(text = sub("k", i, "t.test(k ~ group, data = controlt2[controlt2$mispercept2 == 0,])")))
  temp2 <- tidy(temp)[1:5]
  names(temp2) <- c("mean_dif", "control_mean", "t1_mean", "t-stat", "p-value")
  temp2$varname <- i
  temp2$se <- temp$stderr
  temp2 <- temp2[c(6,1,7,2:5)]
  assign("t2_no_dv", rbind(t2_no_dv, temp2)) 
}
write.table(t2_no_dv[c(1,4:5,7)], file = "t2_no_dv.csv", sep = ",", quote = FALSE, row.names = F)

