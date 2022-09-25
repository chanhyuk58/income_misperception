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
## by group
# control <- raw[raw$group2 == "control",]
# t1 <- raw[raw$group2 == "t1", ]
# t2 <- raw[raw$group2 == "t2", ]
controlt1 <- raw[raw$group2 != "t2", ]

## by misperception
positive <- raw[raw$group2 != "t2" & raw$mispercept2 == 1, ]
negative <- raw[raw$group2 != "t2" & raw$mispercept2 == -1, ]
nomis <- raw[raw$group2 != "t2" & raw$mispercept2 == 0, ]

# Manipulation test subsets
negative2 <- negative[negative$manipulation == 1, ]
positive2 <- positive[positive$manipulation == 1, ]
nomis2 <- nomis[nomis$manipulation == 1, ]
# raw2 <- raw[raw$manipulation == 1, ]

# descriptive ------------------------------------------------------------------
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

## balance test control var ----------------------------------------------------

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

print(xtable(t_control), include.rownames=FALSE)
write.table(t_control[c(1,4:7)], file = "balance_con2.csv", sep = ",", quote = FALSE, row.names = F)

# manipulation checked
t_control2 <- tidy_t.test(controlt1[controlt1$manipulation == 1, ], controlvarlist)

print(xtable(t_control2), include.rownames=FALSE)
write.table(t_control2[c(1,4:7)], file = "balance_con3.csv", sep = ",", quote = FALSE, row.names = F)

## DV t-tests ------------------------------------------------------------------
t_all_dv <- tidy_t.test(controlt1, dvlist)

## Histograms ------------------------------------------------------------------
p1 <- ggplot(data = raw) + 
  geom_bar(aes(x = obj_decile), stat = "count") +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  ylim(0,400) + 
  labs(x = "소득 위치", y = "빈도") +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size=20))
# p1
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
#   labs(x = "주관적 소득 위치", y = "주관적 소득 위치 인식에 대해 자신하는 정도") +
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

# Positive misperception mean difference ---------------------------------------
## Control vars t-tests --------------------------------------------------------

# all control + t1
t_positive_control <- tidy_t.test(positive, controlvarlist)

write.table(t_positive_control, file = "t_positive_control.csv", sep = ",", row.names = F, col.names = T)
print(xtable(t_positive_control), include.rownames=FALSE)

# manipulation checked
t_positive2_control <- tidy_t.test(positive2, controlvarlist)

print(xtable(t_positive2_control), include.rownames=FALSE)
write.table(t_positive_control, file = "t_positive_control.csv", sep = ",", row.names = F, col.names = T)
## DV t-tests ------------------------------------------------------------------
t_positive_dv <- tidy_t.test(positive, dvlist)
write.table(t_positive_dv[c(1,4:5,7)], file = "t_positive_dv.csv", sep = ",", row.names = F)
print(xtable(t_positive_dv), include.rownames=FALSE)

# Manipulation checked
t_positive2_dv <- tidy_t.test(positive2, dvlist)
print(xtable(t_positive2_dv), include.rownames=FALSE)
write.table(t_positive2_dv[c(1:5,7)], file = "t_positive2_dv.csv", sep = ",", row.names = F)

# Negative misperception mean difference ---------------------------------------
## Control vars t-tests --------------------------------------------------------
# all control + t1
t_negative_control <- tidy_t.test(negative, controlvarlist)

write.table(t_negative_control, file = "t_negative_control.csv", sep = ",", row.names = F, col.names = T)
print(xtable(t_negative_control), include.rownames=FALSE)

# manipulation checked
t_negative2_control <- tidy_t.test(negative2, controlvarlist)

write.table(t_negative2_control, file = "t_negative2_control.csv", sep = ",", row.names = F, col.names = T)
print(xtable(t_negative_control), include.rownames=FALSE)

## DV t-tests ------------------------------------------------------------------
# all control + t1
t_negative_dv <- tidy_t.test(negative, dvlist)

print(xtable(t_negative_dv), include.rownames=FALSE)
write.table(t_negative_dv[c(1,4:5,7)], file = "t_neg_dv.csv", sep = ",", row.names = F)

# Manipulation checked
t_negative2_dv <- tidy_t.test(negative2, dvlist)
print(xtable(t_negative2_dv), include.rownames=FALSE)
write.table(t_negative2_dv[c(1:5,7)], file = "t_neg2_dv.csv", sep = ",", row.names = F)

# No misperception mean difference ---------------------------------------

## Control vars t-tests --------------------------------------------------------
# all control + t1
t_nomis_control <- tidy_t.test(nomis, controlvarlist)

write.table(t_nomis_control, file = "t_nomis_control.csv", sep = ",", row.names = F, col.names = T)
print(xtable(t_nomis_control), include.rownames=FALSE)

# manipulation checked
t_nomis2_control <- tidy_t.test(nomis2, controlvarlist)

write.table(t_nomis2_control, file = "t_nomis2_control.csv", sep = ",", row.names = F, col.names = T)
print(xtable(t_nomis2_control), include.rownames=FALSE)

## DV t-tests ------------------------------------------------------------------
# all control + t1
t_nomis_dv <- tidy_t.test(nomis, dvlist)
print(xtable(t_nomis_dv), include.rownames=FALSE)
write.table(t_nomis_dv[c(1, 4:5, 7)], file = "t_no_dv.csv", sep = ",", row.names = F)

# Manipulation checked
t_nomis2_dv <- tidy_t.test(nomis2, dvlist)
print(xtable(t_nomis2_dv), include.rownames=FALSE)
write.table(t_nomis2_dv[c(1:5, 7)], file = "t_no2_dv.csv", sep = ",", row.names = F)

# Falsification Test -----------------------------------------------------------
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
controlt1$group3 <- ifelse(controlt1$mispercept2 == 0, "오인 없음", 
                           ifelse(controlt1$mispercept2 == 1, "상향 오인", "하향 오인")) 

controlt1 %>% 
  group_by(group, group3) %>% 
  summarize(mean = mean(redistribute),
            se = sd(redistribute)/sqrt(length(redistribute)),
            ) %>% 
  ggplot(data =., 
         aes(x = factor(group, levels = c("3", "1")), color = as.factor(group))
         ) +
  facet_wrap( ~ factor(group3, levels = c("하향 오인", "오인 없음", "상향 오인"))) +
  geom_point(aes(y = mean)) +
  geom_errorbar(aes(ymin = mean - se , ymax = mean + se),
                width = 0.3) +
  scale_x_discrete("", labels = c("3" = "통제집단", "1" = "처치집단1")) + 
  ylab("재분배선호 - 소득이전") +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "grey97")) + 
  scale_color_manual(values = c("black", "grey60"))
ggsave(filename = "소득이전_비교.png",
       height = 3,
       width = 6)

controlt1 %>% 
  group_by(group, group3) %>% 
  summarize(mean = mean(tax_pro),
            se = sd(tax_pro)/sqrt(length(tax_pro)),
            ) %>% 
  ggplot(data =., 
         aes(x = factor(group, levels = c("3", "1")), color = as.factor(group))
  ) +
  facet_wrap( ~ factor(group3, levels = c("하향 오인", "오인 없음", "상향 오인"))) +
  geom_point(aes(y = mean)) +
  geom_errorbar(aes(ymin = mean - se , ymax = mean + se),
                width = 0.3) +
  scale_x_discrete("", labels = c("3" = "통제집단", "1" = "처치집단1")) + 
  ylab("재분배선호 - 누진세 강화") +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "grey97")) + 
  scale_color_manual(values = c("black", "grey60"))
ggsave(filename = "누진세강화_비교.png",
       height = 3,
       width = 6)

controlt1 %>% 
  group_by(group, group3) %>% 
  summarize(mean = mean(duty_gov),
            se = sd(duty_gov)/sqrt(length(duty_gov)),
            group3 = group3) %>% 
  ggplot(data =., 
         aes(x = factor(group, levels = c("3", "1")), color = as.factor(group))
  ) +
  facet_wrap( ~ factor(group3, levels = c("하향 오인", "오인 없음", "상향 오인"))) +
  geom_point(aes(y = mean)) +
  geom_errorbar(aes(ymin = mean - se , ymax = mean + se),
                width = 0.3) +
  scale_x_discrete("", labels = c("3" = "통제집단", "1" = "처치집단1")) + 
  ylab("재분배선호 - 정부책임") +
  theme(legend.position = "none", 
        panel.background = element_rect(fill = "grey97")) + 
  scale_color_manual(values = c("black", "grey60"))
ggsave(filename = "정부책입_비교.png", 
       height = 3,
       width = 6)

# Regression -------------------
## deprecated -----
# lm1 <- lm(duty_gov ~ relevel(factor(group), ref = "3")
#           + obj_decile + age + female + marital + educ + house + employ1
#           # + ideo5 + merito + gen_mobile
#             , data = controlt1)
# 
# lm2 <- lm(duty_gov ~ relevel(factor(group), ref = "3") 
#           + obj_decile + age + female + marital + educ + house + employ1
#           + ideo5 + merito + gen_mobile + trust_gov
#           , data = controlt1)
# 
# lm3 <- lm(duty_gov ~ relevel(factor(group), ref = "3") 
#           + relevel(factor(group3), ref = "오인 없음") 
#           + obj_decile + age + female + marital + educ + house + employ1
#           # + ideo5 + merito + gen_mobile
#           , data = controlt1)
# 
# lm4 <- lm(duty_gov ~ relevel(factor(group), ref = "3") 
#           + relevel(factor(group3), ref = "오인 없음") 
#           + sub_decile + obj_decile + age + female + marital + educ + house 
#           + employ1 + ideo5 + merito + gen_mobile + trust_gov
#           , data = controlt1)
# 
# stargazer(lm1, lm2, lm3, lm4,
#           out = "11_duty_gov.html",
#           covariate.labels = c("treatment group", 
#                                "positive mispercept", 
#                                "negative mispercept"),
#           keep.stat = c("n", "rsq"))
# 
# 
# 
# lm5 <- lm(duty_gov ~ 
#             relevel(factor(group), ref = "3")*relevel(factor(group3), ref = "오인 없음") 
#           + obj_decile + age + female + marital + educ + house + employ1
#           # + ideo5 + merito + gen_mobile + trust_gov
#           , data = controlt1)
# 
# lm6 <- lm(duty_gov ~ 
#             relevel(factor(group), ref = "3")*relevel(factor(group3), ref = "오인 없음") 
#           + obj_decile + age + female + marital + educ + house + employ1
#           + ideo5 + merito + gen_mobile + trust_gov
#           , data = controlt1)
# 
# stargazer(lm5, lm6,
#           out = "11_int.html",
#           order = c(1:3, 15:16, 4:14),
#           covariate.labels = c("treatment group", 
#                                "positive mispercept", 
#                                "negative mispercept",
#                                "treatment group x positive",
#                                "trestment group x negative"),
#           keep.stat = c("n", "rsq"))
# 
# controlt1$province_yeongnam <- ifelse(controlt1$province == 14 | controlt1$province == 15, 1, 0)
# controlt1$province_honam <- ifelse(controlt1$province == 12 | controlt1$province == 13, 1, 0)
# 
# lm7 <- lm(redistribute ~ 
#             relevel(factor(group), ref = "3")*relevel(factor(group3), ref = "오인 없음")
#           + province_yeongnam
#           + obj_decile + age + female + marital + educ + house + employ1
#           # + ideo5 + merito + gen_mobile + trust_gov
#           , data = controlt1)
# 
# lm8 <- lm(redistribute ~ 
#             relevel(factor(group), ref = "3")*relevel(factor(group3), ref = "오인 없음") 
#           + province_honam
#           + obj_decile + age + female + marital + educ + house + employ1
#           # + ideo5 + merito + gen_mobile + trust_gov
#           , data = controlt1)
# 
# stargazer(lm7, lm8,
#           out = "yeongnam_honam_int.html",
#           order = c(1:3, 13:14, 4:12),
#           covariate.labels = c("treatment group", 
#                                "positive mispercept", 
#                                "negative mispercept",
#                                "treatment group x positive",
#                                "trestment group x negative"),
#           keep.stat = c("n", "rsq"))
# 
# 
# lm9 <- lm(redistribute ~ 
#             relevel(factor(group), ref = "3")*relevel(factor(group3), ref = "오인 없음")
#           + minju
#           + obj_decile + age + female + marital + educ + house + employ1
#           # + ideo5 + merito + gen_mobile + trust_gov
#           , data = controlt1)
# 
# lm10 <- lm(redistribute ~ 
#             relevel(factor(group), ref = "3")*relevel(factor(group3), ref = "오인 없음") 
#           + kukhim
#           + obj_decile + age + female + marital + educ + house + employ1
#           # + ideo5 + merito + gen_mobile + trust_gov
#           , data = controlt1)
# 
# stargazer(lm9, lm10,
#           out = "minju_kukhim_int.html",
#           order = c(1:3, 13:14, 4:12),
#           covariate.labels = c("treatment group", 
#                                "positive mispercept", 
#                                "negative mispercept",
#                                "treatment group x positive",
#                                "trestment group x negative"),
#           keep.stat = c("n", "rsq"))

## reported ----
lm11 <- lm(redistribute ~ 
             relevel(factor(group), ref = "3")*relevel(factor(group3), ref = "오인 없음") 
           # + minju
           # + kukhim
           + obj_decile + age + female + marital + educ + house + employ1
           # + ideo5 + merito + gen_mobile + trust_gov
           + factor(province)
           , data = controlt1[controlt1$manipulation == 1, ])

lm12 <- lm(redistribute ~ 
             relevel(factor(group), ref = "3")*relevel(factor(group3), ref = "오인 없음") 
           + minju
           + kukhim
           + obj_decile + age + female + marital + educ + house + employ1
           + ideo5 + merito + gen_mobile + trust_gov
           + factor(province)
           , data = controlt1[controlt1$manipulation == 1, ])

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
