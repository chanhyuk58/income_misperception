library("dplyr")
library("hIRT")
library("xtable")

df <- read.csv("./rawdata.csv")
df <- df %>% 
  mutate(manipulation = ifelse(group == (4 - check), 1, 0),
         mispercept = (sub_decile - obj_decile))
df$mispercept2 <- cut(x = df$mispercept, 
                       breaks = c(-10, -2, 1, 10), 
                       labels = c(-1, 0, 1))
df$over <- ifelse(df$mispercept2 == 1, 1, 0)
df$under <- ifelse(df$mispercept2 == -1, 1, 0)
names(df)

y1 <- df[df$group2 != "t2", c("redistribute", "tax_pro", "duty_gov")]
x1 <- df[df$group2 != "t2", c("group", "employ", "age", "educ", "female", "marital", "house", "ideo5", "over", "under")]
x1$interact_over <- ifelse(x1$group == 1, 1, 0) * x1$over 
x1$interact_under <- ifelse(x1$group == 1, 1, 0) * x1$under

hirt <- hIRT::hgrm(y1, as.matrix(x1))

summary_hirt <- hirt$coefficient %>%
  filter(grepl("^x", row.names(hirt$coefficient)))

coefnames <- c("Treatment group 1", "Employment", "Age", "Education", "Female", "Marital Status", "Homeownership", "Ideology", "Overestimate", "Underestimate", "Treatment group 1 $\times$ Overestimate", "Treatment group 1 $\times$ Underestimate")

coef_texreg <- createTexreg(coef.names = coefnames, coef = summary_hirt$Estimate, se = summary_hirt$Std_Error, pvalues=summary_hirt$p_value)
texreg(coef_texreg, file="./test.tex")
