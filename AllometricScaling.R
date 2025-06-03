#load packages
library(openxlsx)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(rstatix)
library(effsize)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(effsize)

#load data
df<-read.xlsx("Data_HW.xlsx")

#only  controls
df<-df[df$genotype=="control",]

#remove mice with age out of usual range (#7 mice with 255-433 weeks, 20 mice with age between 2 and 13)
df<-df[-which(df$age_in_weeks>20 & df$pipeline=="early_onset"),] #remove 7 animals
df<-df[-which(df$age_in_weeks<14 & df$pipeline=="early_onset"),] #remove 20 animals

#relabel
df$pipeline[df$pipeline=="early_onset"]<-"EA"
df$pipeline[df$pipeline=="late_adult"]<-"LA"

##################################################################
#TABLE 2: table with mean,sd,n
#by center
mean_tbl <- df[df$pipeline=="EA",] %>%
  group_by(center,sex) %>%
  summarise("Body Weight [g]" = paste("N = ",length(which(!is.na(Body_weight))), sep = ""),
            "Heart Weight [mg]" = paste("N = ",length(which(!is.na(Heart_weight))), sep = ""),
            "Tibia Length [mm]" = paste("N = ",length(which(!is.na(Tibia_length))), sep = "")
  )
table<-ggtexttable(t(mean_tbl))

mean_tbl1 <- df[df$pipeline=="LA",] %>%
  group_by(center,sex) %>%
  summarise("Body Weight [g]" = paste("N = ",length(which(!is.na(Body_weight))), sep = ""),
            "Heart Weight [mg]" = paste("N = ",length(which(!is.na(Heart_weight))), sep = ""),
            "Tibia Length [mm]" = paste("N = ",length(which(!is.na(Tibia_length))), sep = "")
  )
table1<-ggtexttable(t(mean_tbl1))

#by pipeline
mean_tbl2 <- df %>%
  group_by(pipeline,sex) %>%
  summarise("Body Weight [g]" = paste("N = ",length(which(!is.na(Body_weight))), sep = ""),
            "Heart Weight [mg]" = paste("N = ",length(which(!is.na(Heart_weight))), sep = ""),
            "Tibia Length [mm]" = paste("N = ",length(which(!is.na(Tibia_length))), sep = "")
  )
table2<-t(mean_tbl2)
table2<-ggtexttable(cbind(table2,c("Total","","N = 25427","N = 25354","N = 9216")))

table%>%tab_add_title(text = "EA", face = "bold", padding = unit(0.1, "line"))
table1%>%tab_add_title(text = "LA", face = "bold", padding = unit(0.1, "line"))
table2%>%tab_add_title(text = "All center", face = "bold", padding = unit(0.1, "line"))

rm(table,table1,table2)

##################################################################
#Plots
#Histogram with grid by pipeline and sex

ggplot(df,aes(x=Heart_weight,col=sex))+geom_histogram(alpha=0.5)+scale_color_manual(values=c("#CC79A7","#009E73"))+
  facet_grid(facets=sex~pipeline)+theme(legend.position = "none")+xlab("Heart weight [mg]")

ggplot(df,aes(x=Tibia_length,col=sex))+geom_histogram(alpha=0.5)+scale_color_manual(values=c("#CC79A7","#009E73"))+
  facet_grid(facets=sex~pipeline)+theme(legend.position = "none")+xlab("Tibia Length [mm]")

ggplot(df,aes(x=Body_weight,col=sex))+geom_histogram(alpha=0.5)+scale_color_manual(values=c("#CC79A7","#009E73"))+
  facet_grid(facets=sex~pipeline)+theme(legend.position = "none")+xlab("Body weight [mg]")

#by center
ggplot(df,aes(x=Heart_weight,col=pipeline))+geom_histogram(alpha=0.5)+scale_color_manual(values=c("#888888","#882255"))+
  facet_grid(facets=sex~center,scales="free_y")+theme(legend.position = "none")+xlab("Heart weight [mg]")

ggplot(df,aes(x=Body_weight,col=pipeline))+geom_histogram(alpha=0.5)+scale_color_manual(values=c("#888888","#882255"))+
  facet_grid(facets=sex~center,scales="free_y")+theme(legend.position = "none")+xlab("Body weight [g]")

ggplot(df,aes(x=Tibia_length,col=pipeline))+geom_histogram(alpha=0.5)+scale_color_manual(values=c("#888888","#882255"))+
  facet_grid(facets=sex~center,scales="free_y")+theme(legend.position = "none")+xlab("Tibia Length [mm]")

#FIGURE 1: grid only for sex
ggplot(df,aes(x=Heart_weight,col=pipeline))+geom_histogram(alpha=0.5)+scale_color_manual(values=c("#888888","#882255"))+
  facet_grid(facets=sex~.,scales="free_y")+theme(legend.position = "none")+xlab("Heart weight [mg]")

ggplot(df,aes(x=Body_weight,col=pipeline))+geom_histogram(alpha=0.5)+scale_color_manual(values=c("#888888","#882255"))+
  facet_grid(facets=sex~.,scales="free_y")+theme(legend.position = "none")+xlab("Body weight [g]")

ggplot(df,aes(x=Tibia_length,col=pipeline))+geom_histogram(alpha=0.5)+scale_color_manual(values=c("#888888","#882255"))+
  facet_grid(facets=sex~.,scales="free_y")+theme(legend.position = "none")+xlab("Tibia Length [mm]")


#calculate mean,sd, median, IQR
mean_tbl <- df %>%
  group_by(pipeline,sex) %>%
  summarise("Heart weight [mg]" = paste(round(mean(Heart_weight, na.rm = TRUE),1), " ± ", round(sd(Heart_weight, na.rm = TRUE),1)," (",length(which(!is.na(Heart_weight))),")", sep = ""),
            "Body weight [g]" = paste(round(mean(Body_weight, na.rm = TRUE),1), " ± ", round(sd(Body_weight, na.rm = TRUE),1)," (",length(which(!is.na(Body_weight))),")", sep = ""),
            "Tibia length [mm]"= paste(round(mean(Tibia_length, na.rm = TRUE),1), " ± ", round(sd(Tibia_length, na.rm = TRUE),1), " (",length(which(!is.na(Tibia_length))),")",sep = ""))
ggtexttable(t(mean_tbl))

mean_tbl2 <- df %>%
  group_by(pipeline,sex) %>%
  summarise("Heart weight [mg]" = paste(round(median(Heart_weight, na.rm = TRUE),1), " [", round(quantile(Heart_weight,probs=0.025, na.rm = TRUE),1),";",round(quantile(Heart_weight,probs=0.975, na.rm = TRUE),1),"]", sep = ""),
            "Body weight [g]" = paste(round(median(Body_weight, na.rm = TRUE),1), " [", round(quantile(Body_weight,probs=0.025, na.rm = TRUE),1),";",round(quantile(Body_weight,probs=0.975, na.rm = TRUE),1),"]", sep = ""),
            "Tibia length [mm]" = paste(round(median(Tibia_length, na.rm = TRUE),1), " [", round(quantile(Tibia_length,probs=0.025, na.rm = TRUE),1),";",round(quantile(Tibia_length,probs=0.975, na.rm = TRUE),1),"]", sep = ""))
ggtexttable(t(mean_tbl2))

##################################################################
#TABLE 3:

#CohensD
#variables vector
vars_to_test <- c("Body_weight", "Heart_weight","Tibia_length")

#calculate cohens'd for sex
results <- lapply(vars_to_test, function(var) {
  df %>%
    filter(!is.na(.data[[var]]), !is.na(sex)) %>%
    group_by(pipeline) %>%
    summarise(
      variable = var,
      cohen_d = tryCatch(
        cohen.d(pick(all_of(var))[[1]] ~ sex)$estimate,
        error = function(e) NA_real_
      ),
      .groups = "drop"
    )
}) %>%
  bind_rows()

results

#calculate cohens'd for pipeline
results_age <- lapply(vars_to_test, function(var) {
  df %>%
    filter(!is.na(.data[[var]]), !is.na(pipeline)) %>%
    group_by(sex) %>%
    summarise(
      variable = var,
      cohen_d = tryCatch(
        cohen.d(pick(all_of(var))[[1]] ~ pipeline)$estimate,
        error = function(e) NA_real_
      ),
      .groups = "drop"
    )
}) %>%
  bind_rows()

results_age

#T-test for sex
ttest_results <- lapply(vars_to_test, function(var) {
  df %>%
    filter(!is.na(.data[[var]]), !is.na(sex)) %>%
    group_by(pipeline) %>%
    summarise(
      variable = var,
      t_statistic = tryCatch(
        t.test(pick(all_of(var))[[1]] ~ sex)$statistic,
        error = function(e) NA_real_
      ),
      p_value = tryCatch(
        t.test(pick(all_of(var))[[1]] ~ sex)$p.value,
        error = function(e) NA_real_
      ),
      mean_male = mean(pick(all_of(var))[[1]][sex == "male"], na.rm = TRUE),
      mean_female = mean(pick(all_of(var))[[1]][sex == "female"], na.rm = TRUE),
      .groups = "drop"
    )
}) %>%
  bind_rows()

#T-test for sex
ttest_results_age <- lapply(vars_to_test, function(var) {
  df %>%
    filter(!is.na(.data[[var]]), !is.na(pipeline)) %>%
    group_by(sex) %>%
    summarise(
      variable = var,
      t_statistic = tryCatch(
        t.test(pick(all_of(var))[[1]] ~ pipeline)$statistic,
        error = function(e) NA_real_
      ),
      p_value = tryCatch(
        t.test(pick(all_of(var))[[1]] ~ pipeline)$p.value,
        error = function(e) NA_real_
      ),
      mean_EA = mean(pick(all_of(var))[[1]][pipeline == "EA"], na.rm = TRUE),
      mean_LA = mean(pick(all_of(var))[[1]][pipeline == "LA"], na.rm = TRUE),
      .groups = "drop"
    )
}) %>%
  bind_rows()


###############################################################
#FIGURE 2 : Scatterplots
#BW
ggscatter(df, x = "Body_weight", y = "Heart_weight",
          color = "sex", palette =c("#CC79A7","#009E73"),alpha=0.6,facet.by = c("sex","pipeline"),
          add = "reg.line", conf.int = FALSE)+stat_cor(aes(color = sex),label.x = 30,label.y=60)+
  stat_regline_equation(label.x = 3,label.y=340)+
  theme(legend.position = "none")+xlab("Body weight [g]")+ylab("Heart weight [mg]")

ggscatter(df, x = "Body_weight", y = "Heart_weight",alpha=.5,
          color = "pipeline", fill="sex",palette =c("#009E73","#108e6c"),alpha=0.4,facet.by = c("sex"),
          add = "reg.line", conf.int = FALSE)+
  #stat_cor(aes(color = pipeline),label.x = 30)+
  stat_regline_equation(aes(color = pipeline),label.x = 45)+
  theme(legend.position = "none")+xlab("Body weight [g]")+ylab("Heart weight [mg]")

#Tibia length
ggscatter(df, x = "Tibia_length", y = "Heart_weight",
          color = "sex", palette =c("#CC79A7","#009E73"),alpha=0.6,facet.by = c("sex","pipeline"),
          add = "reg.line", conf.int = FALSE)+stat_cor(aes(color = sex),label.x = 17,label.y=60)+
  stat_regline_equation(label.y=340)+
  theme(legend.position = "none")+xlab("Tibia Length [mm]")+ylab("Heart weight [mg]")

ggscatter(df, x = "Tibia_length", y = "Heart_weight",
          color = "pipeline", palette =c("#CC79A7","#af5587","#009E73","#0d7256"),alpha=0.4,facet.by = c("sex"),
          add = "reg.line", conf.int = FALSE)+xlim(14,22)+
  #stat_cor(aes(color = pipeline),label.x = 17)+
  stat_regline_equation(aes(color = pipeline),label.x = 15)+
  theme(legend.position = "none")+xlab("Tibia Length [mm]")+ylab("Heart weight [mg]")

##########################
# FIGURE 4
#Allometric Sacling Model

test<-df[df$sex=="female",]
#test<-df[df$sex=="male",]

# Hw  und BW
L <- test$Body_weight
#L <- test$Tibia_length
M <- test$Heart_weight

# Logarithmieren der Daten
logL <- log10(L)
logM <- log10(M)

# Lineare Regression
model <- lm(logM ~ logL)

# Koeffizienten der linearen Regression
c <- coef(model)[2]  # Steigung
logb <- coef(model)[1]  # y-Achsenabschnitt

# Antilogarithmus von log(b) um b zu erhalten
b <- 10^logb

# Ergebnisse ausgeben
cat(sprintf("b = %f, c = %f\n", b, c))

#BW
#female: b = 34.143579, c = 0.383847
#male = 36.424919, c = 0.394736

#TB
#both female: b = 11.102819, c = 0.830599
#both male:b = 32.066647, c = 0.522664

#Durch Logarithmieren geht die zwischen den Variablen x und y bestehende allometrische Beziehung y = b · x c ( mit b > 0) 
#in log(y) = log(b · x c ) = log(b) + c · log(x) über. Diese Gleichung bringt zum Ausdruck, dass zwischen den
#transformierten Variablen ye := log(y) und xe := log(x) die lineare Abhängigkeit ye = c · xe + log(b) besteht

allo_female <- function(x) { 34.143 * x^0.3838 }
allo_male   <- function(x) { 36.424 * x^0.3947 }

#Plot for regression equation
ggscatter(df, x = "Body_weight", y = "Heart_weight",alpha=.5,
          color = "pipeline", fill="sex",palette =c("#CC79A7","#af5587","#009E73","#108e6c"),alpha=0.2,facet.by = c("sex"),
          add = "reg.line", conf.int = FALSE)+xlim(10,80)+
  #stat_cor(aes(color = pipeline),label.x = 30)+
  stat_regline_equation(aes(color = pipeline),label.x = 45)+
  theme(legend.position = "none")+xlab("Body weight [g]")+ylab("Heart weight [mg]")+
  geom_line(data = df %>% filter(sex == "female"),stat='function', fun=allo_female, color='blue', linetype='dashed', size=1)+ 
  geom_line(data = df %>% filter(sex == "male"),stat='function', fun=allo_male, color='blue', linetype='dashed', size=1) +
  #geom_text(data = df %>% filter(sex == "female"),x = 3, y = 300, label ="y == 30 * x^{0.3}", parse = TRUE)
  geom_text(data = data.frame(sex = c("female", "male"),  # Gleichungen für die Facetten
                              x = c(50, 50), 
                              y = c(50, 50), 
                              label = c("y == 34.1 * x^{0.38}",
                                        "y == 36.4 * x^{0.39}")),
            aes(x = x, y = y, label = label),
            parse = TRUE,
            color = "blue",
            size = 4)


ggscatter(df, x = "Tibia_length", y = "Heart_weight",alpha=.5,
          color = "pipeline", fill="sex",palette =c("#CC79A7","#af5587","#009E73","#108e6c"),alpha=0.2,facet.by = c("sex"),
          add = "reg.line", conf.int = FALSE)+xlim(14,22)+
  #stat_cor(aes(color = pipeline),label.x = 30)
  stat_regline_equation(aes(color = pipeline),label.x = 14)+
  theme(legend.position = "none")+xlab("Body weight [g]")+ylab("Heart weight [mg]")+
  geom_line(data = df %>% filter(sex == "female"),stat='function', fun=allo_female, color='blue', linetype='dashed', size=1)+ 
  geom_line(data = df %>% filter(sex == "male"),stat='function', fun=allo_male, color='blue', linetype='dashed', size=1) +
  #geom_text(data = df %>% filter(sex == "female"),x = 3, y = 300, label ="y == 30 * x^{0.3}", parse = TRUE)
  geom_text(data = data.frame(sex = c("female", "male"),  # Gleichungen für die Facetten
                              x = c(20,20), 
                              y = c(50, 50), 
                              label = c("y == 11.1 * x^{0.83}",
                                        "y == 32.1 * x^{0.52}")),
            aes(x = x, y = y, label = label),
            parse = TRUE,
            color = "blue",
            size = 4)


######################
#Simulation Study

#set seed
set.seed(12)

#CASE 1 /CASE 2:
x<-rnorm(10,35,3)
y<-rnorm(10,30,3)
error=rnorm(10,0,3)

dat<-data.frame(bw=c(x,y),geno=c(rep("Group 1",10),rep("Group 2",10)))
#CASE 1
dat$hw=30+(1.5*dat$bw)+error
#CASE 2
dat$hw2=(1.5*dat$bw)+error

######
#OR....

#Case 3:
set.seed(12)
x<-rnorm(10,35,3)
y<-rnorm(10,30,3)
error=rnorm(10,0,3)
dat<-data.frame(bw=c(x,y),geno=c(rep("Group 1",10),rep("Group 2",10)))

#CASE 3
dat$hw[dat$geno=="Group 1"]=rnorm(10,80,5)
dat$hw[dat$geno=="Group 2"]=rnorm(10,80,5)

###############################

#Models and PLOTS
m1=lm(hw~bw+geno,data=dat)
summary(m1)

tab_model(m1,pred.labels = c("Intercept", "Parameter 1", "Group 2"),
          dv.labels = c("Case1: Linear Model with BW as covariate"),
          string.pred = "Coeffcient",
          string.ci = "Conf. Int (95%)",
          string.p = "P-Value"#,
          #file = "CASE1.doc"
)

stat.test <- dat %>%
  t_test(bw ~ geno) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "geno")

dat %>%cohens_d(bw ~ geno) 
ggboxplot(dat,y="bw",x="geno",fill="geno")+xlab("")+ylab("Parameter 1")+theme(legend.position = "none")+scale_fill_manual(values=c("#79AF97FF","#6A6599FF"))+ stat_pvalue_manual(stat.test,label = "T-test, p = {p}")

stat.test <- dat %>%
  t_test(hw ~ geno) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "geno")

dat %>%cohens_d(hw ~ geno) 
ggboxplot(dat,y="hw",x="geno",fill="geno")+xlab("")+ylab("Parameter 2")+theme(legend.position = "none")+scale_fill_manual(values=c("#79AF97FF","#6A6599FF"))+ stat_pvalue_manual(stat.test,label = "T-test, p = {p}")

stat.test <- dat %>%
  t_test(hw2 ~ geno) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "geno")

dat %>%cohens_d(hw2 ~ geno) 
ggboxplot(dat,y="hw2",x="geno",fill="geno")+xlab("")+ylab("Parameter 2")+theme(legend.position = "none")+scale_fill_manual(values=c("#79AF97FF","#6A6599FF"))+ stat_pvalue_manual(stat.test,label = "T-test, p = {p}")


m2=lm(hw2~bw+geno,data=dat)
summary(m2)
tab_model(m2,pred.labels = c("Intercept", "Parameter 1", "Group 2"),
          dv.labels = c("Case1: Linear Model with BW as covariate"),
          string.pred = "Coeffcient",
          string.ci = "Conf. Int (95%)",
          string.p = "P-Value"
          #  file = "CASE1.doc"
)

#Scatterplots
ggscatter(dat, x = "bw", y = "hw",
          color = "geno", palette=c("#79AF97FF","#6A6599FF"),
          add = "reg.line", conf.int = TRUE)+stat_cor(aes(color = geno), label.x = 25)+
  stat_regline_equation(aes(color = geno),label.x=33) +theme(legend.position = "none")+xlab("Parameter 1")+ylab("Parameter 2")

ggscatter(dat, x = "bw", y = "hw2",
          color = "geno", palette=c("#79AF97FF","#6A6599FF"),
          add = "reg.line", conf.int = TRUE)+stat_cor(aes(color = geno), label.x = 28)+
  stat_regline_equation(aes(color = geno),label.x=33) +theme(legend.position = "none")+xlab("Parameter 1")+ylab("Parameter 2")

#calculate ratio CASE 1 /CASE 3
dat$ratio<-dat$hw/dat$bw

stat.test <- dat %>%
  t_test(ratio ~ geno) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "geno")

dat %>%cohens_d(ratio ~ geno) 
ggboxplot(dat,y="ratio",x="geno",fill="geno")+xlab("")+ylab("Parameter 1 / Parameter 2")+theme(legend.position = "none")+scale_fill_manual(values=c("#79AF97FF","#6A6599FF"))+ stat_pvalue_manual(stat.test,label = "T-test, p = {p}")

#calcualte ratio CASE 2
dat$ratio2<-dat$hw2/dat$bw

stat.test <- dat %>%
  t_test(ratio2 ~ geno) %>%
  add_significance()
stat.test <- stat.test %>% add_xy_position(x = "geno")
#stat.test$p.format <- p_format(
#  stat.test$p, accuracy = 0.001,
#)
dat %>%cohens_d(ratio2 ~ geno) 
ggboxplot(dat,y="ratio2",x="geno",fill="geno")+xlab("")+ylab("Parameter 1 / Parameter 2")+theme(legend.position = "none")+scale_fill_manual(values=c("#79AF97FF","#6A6599FF"))+ stat_pvalue_manual(stat.test,label = "T-test, p = {p}")

