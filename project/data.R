library(tidyverse)
library(MatchIt)
library(dplyr)
library(Hmisc)
library(Zelig)
library(treatSens)
library(remotes)
library(mutate)
library(sensitivityfull)
library(sensitivitymw)
library(cobalt)
install.packages("rgenoud")
library(rgenoud)
install.packages("Matching")
library(Matching)
install.packages("tableone")
library(tableone)
library




burn <- read.csv("/Users/yura/Downloads/burn_out.csv", header = TRUE)
burn


############성향점수추정
#로지스틱 회귀모형을 이용한 성향점수 추정
logis_ps = glm(Department ~ Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
               data=burn, family=binomial(link="logit"))
summary(logis_ps)


#성향점수 추정
logis_ps



#성향점수 추정결과
burn$ps_prob=predict(logis_ps, burn, type="response")
burn$ps_logit=predict(logis_ps, burn, type="link")

test_prob = burn$ps_prob[1:10]
log(test_prob/(1-test_prob))
burn$ps_logit[1:10]

plot_logit=burn %>% ggplot(aes(x=ps_logit)) + geom_histogram(bins=40) +
  labs(x="logitPS", y="freq") + theme_bw()
plot_prob=burn %>% ggplot(aes(x=ps_prob)) + geom_histogram(bins=40) +
  labs(x="probPS", y="freq") + theme_bw()
gridExtra::grid.arrange(plot_logit, plot_prob)


#공통지지영역
#히스토그램 이용
plot_prob_cs = ggplot(data=burn, aes(x=ps_prob)) +
  geom_histogram(data=burn %>% filter(Department==1),
                 bins=40, fill="pink", alpha=0.4)+
  geom_histogram(data=burn %>% filter(Department==0),
                 bins=40, fill="yellow", alpha=0.4)+
  labs(x="추정확률(probability, 0~1) 형태의 성향점수", y="빈도")+
  theme_bw()+
  ggtitle("추정확률 이용시 공통지지영역")

plot_logit_cs = ggplot(data=burn, aes(x=ps_logit)) +
  geom_histogram(data=burn %>% filter(Department==1),
                 bins=40, fill="pink", alpha=0.4)+
  geom_histogram(data=burn %>% filter(Department==0),
                 bins=40, fill="yellow", alpha=0.4)+
  labs(x="선형로짓(logit, -∞ ~ +∞) 형태의 성향점수", y="빈도")+
  theme_bw()+ggtitle("선형로짓 이용시 공통지지영역")

gridExtra::grid.arrange(plot_logit_cs, plot_prob_cs, nrow=1)           

#박스플롯 이용
boxplot_prob_cs = burn %>% 
  mutate(Department = ifelse(Department==0, "통제집단", "처치집단")) %>%
  ggplot(aes(y=ps_prob, color=Department)) +
  geom_boxplot()+
  labs(x="추정확률(probability, 0~1) 형태의 성향점수", y="분포",
       color="집단구분")+
  theme_bw() +
  ggtitle("추정확률 이용시 공통지지영역")

boxplot_logit_cs = burn %>% 
  mutate(Department = ifelse(Department==0, "통제집단", "처치집단")) %>%
  ggplot(aes(y=ps_logit, color=Department)) +
  geom_boxplot()+
  labs(x="선형로짓(logit, -∞ ~ +∞) 형태의 성향점수", y="빈도",
       color="집단구분")+
  theme_bw() +
  ggtitle("추정확률 이용시 공통지지영역")

gridExtra::grid.arrange(boxplot_logit_cs, boxplot_prob_cs, nrow=1)



########################효과추정치계산

#전통적 OLS 회귀모형

lm(mean ~ Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training, burn)

#성향점수 계산

burn$pscore = glm(Department~Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
                  data = burn, family = binomial(link = "logit")) %>% fitted()


#ate, att, atc 추정

burn=burn %>% 
  mutate(
    Wate = ifelse(Department==1, 1/pscore, 1/(1-pscore)),
    Watt = ifelse(Department==1, 1, 1/(1-pscore)),
    Watc = ifelse(Department==1, 1/pscore, 1)
  )

head(burn,20)


#공변량 균형성 점검
#공변량 표준화

burn$AgeS=(burn$Age-mean(burn$Age))/sd(burn$Age)
mean(burn$AgeS[burn$Department==1]) - mean(burn$AgeS[burn$Department==0])
wtd.mean(burn$AgeS[burn$Department==1], burn$Wate[burn$Department==1]) -
  wtd.mean(burn$AgeS[burn$Department==0], burn$Wate[burn$Department==0])
var(burn$AgeS[burn$Department==1]) - var(burn$AgeS[burn$Department==0])
wtd.var(burn$AgeS[burn$Department==1], burn$Wate[burn$Department==1]) -
  wtd.var(burn$AgeS[burn$Department==0], burn$Wate[burn$Department==0])


burn$GenderS=(burn$Gender-mean(burn$Gender))/sd(burn$Gender)
mean(burn$GenderS[burn$Department==1]) - mean(burn$GenderS[burn$Department==0])
wtd.mean(burn$GenderS[burn$Department==1], burn$Wate[burn$Department==1]) -
  wtd.mean(burn$GenderS[burn$Department==0], burn$Wate[burn$Department==0])
var(burn$GenderS[burn$Department==1]) - var(burn$GenderS[burn$Department==0])
wtd.var(burn$GenderS[burn$Department==1], burn$Wate[burn$Department==1]) -
  wtd.var(burn$AgeS[burn$Department==0], burn$Wate[burn$Department==0])


burn$ComorbiditiesS=(burn$Comorbidities-mean(burn$Comorbidities))/sd(burn$Comorbidities)
mean(burn$ComorbiditiesS[burn$Department==1]) - mean(burn$ComorbiditiesS[burn$Department==0])
wtd.mean(burn$ComorbiditiesS[burn$Department==1], burn$Wate[burn$Department==1]) -
  wtd.mean(burn$ComorbiditiesS[burn$Department==0], burn$Wate[burn$Department==0])
var(burn$ComorbiditiesS[burn$Department==1]) - var(burn$ComorbiditiesS[burn$Department==0])
wtd.var(burn$ComorbiditiesS[burn$Department==1], burn$Wate[burn$Department==1]) -
  wtd.var(burn$ComorbiditiesS[burn$Department==0], burn$Wate[burn$Department==0])


burn$DesignationS=(burn$Designation-mean(burn$Designation))/sd(burn$Designation)
mean(burn$DesignationS[burn$Department==1]) - mean(burn$DesignationS[burn$Department==0])
wtd.mean(burn$DesignationS[burn$Department==1], burn$Wate[burn$Department==1]) -
  wtd.mean(burn$DesignationS[burn$Department==0], burn$Wate[burn$Department==0])
var(burn$DesignationS[burn$Department==1]) - var(burn$DesignationS[burn$Department==0])
wtd.var(burn$DesignationS[burn$Department==1], burn$Wate[burn$Department==1]) -
  wtd.var(burn$DesignationS[burn$Department==0], burn$Wate[burn$Department==0])


burn$YearsS=(burn$Years-mean(burn$Years))/sd(burn$Years)
mean(burn$YearsS[burn$Department==1]) - mean(burn$YearsS[burn$Department==0])
wtd.mean(burn$YearsS[burn$Department==1], burn$Wate[burn$Department==1]) -
  wtd.mean(burn$YearsS[burn$Department==0], burn$Wate[burn$Department==0])
var(burn$YearsS[burn$Department==1]) - var(burn$YearsS[burn$Department==0])
wtd.var(burn$YearsS[burn$Department==1], burn$Wate[burn$Department==1]) -
  wtd.var(burn$YearsS[burn$Department==0], burn$Wate[burn$Department==0])


burn$WorkedICUS=(burn$WorkedICU-mean(burn$WorkedICU))/sd(burn$WorkedICU)
mean(burn$WorkedICUS[burn$Department==1]) - mean(burn$WorkedICUS[burn$Department==0])
wtd.mean(burn$WorkedICUS[burn$Department==1], burn$Wate[burn$Department==1]) -
  wtd.mean(burn$WorkedICUS[burn$Department==0], burn$Wate[burn$Department==0])
var(burn$WorkedICUS[burn$Department==1]) - var(burn$WorkedICUS[burn$Department==0])
wtd.var(burn$WorkedICUS[burn$Department==1], burn$Wate[burn$Department==1]) -
  wtd.var(burn$WorkedICUS[burn$Department==0], burn$Wate[burn$Department==0])


burn$workweekS=(burn$workweek-mean(burn$workweek))/sd(burn$workweek)
mean(burn$workweekS[burn$Department==1]) - mean(burn$workweekS[burn$Department==0])
wtd.mean(burn$workweekS[burn$Department==1], burn$Wate[burn$Department==1]) -
  wtd.mean(burn$workweekS[burn$Department==0], burn$Wate[burn$Department==0])
var(burn$workweekS[burn$Department==1]) - var(burn$workweekS[burn$Department==0])
wtd.var(burn$workweekS[burn$Department==1], burn$Wate[burn$Department==1]) -
  wtd.var(burn$workweekS[burn$Department==0], burn$Wate[burn$Department==0])


burn$PPES=(burn$PPE-mean(burn$PPE))/sd(burn$PPE)
mean(burn$PPES[burn$Department==1]) - mean(burn$PPES[burn$Department==0])
wtd.mean(burn$PPES[burn$Department==1], burn$Wate[burn$Department==1]) -
  wtd.mean(burn$PPES[burn$Department==0], burn$Wate[burn$Department==0])
var(burn$PPES[burn$Department==1]) - var(burn$PPES[burn$Department==0])
wtd.var(burn$PPES[burn$Department==1], burn$Wate[burn$Department==1]) -
  wtd.var(burn$PPES[burn$Department==0], burn$Wate[burn$Department==0])


burn$mentalsupportS=(burn$mentalsupport-mean(burn$mentalsupport))/sd(burn$mentalsupport)
mean(burn$mentalsupportS[burn$Department==1]) - mean(burn$mentalsupportS[burn$Department==0])
wtd.mean(burn$mentalsupportS[burn$Department==1], burn$Wate[burn$Department==1]) -
  wtd.mean(burn$mentalsupportS[burn$Department==0], burn$Wate[burn$Department==0])
var(burn$mentalsupportS[burn$Department==1]) - var(burn$mentalsupportS[burn$Department==0])
wtd.var(burn$mentalsupportS[burn$Department==1], burn$Wate[burn$Department==1]) -
  wtd.var(burn$mentalsupportS[burn$Department==0], burn$Wate[burn$Department==0])


burn$trainingS=(burn$training-mean(burn$training))/sd(burn$training)
mean(burn$trainingS[burn$Department==1]) - mean(burn$trainingS[burn$Department==0])
wtd.mean(burn$trainingS[burn$Department==1], burn$Wate[burn$Department==1]) -
  wtd.mean(burn$trainingS[burn$Department==0], burn$Wate[burn$Department==0])
var(burn$trainingS[burn$Department==1]) - var(burn$trainingS[burn$Department==0])
wtd.var(burn$trainingS[burn$Department==1], burn$Wate[burn$Department==1]) -
  wtd.var(burn$trainingS[burn$Department==0], burn$Wate[burn$Department==0])




#균형성 점검을 위한 이용자함수 설정

balance_check_PSW = function(var_Department, var_cov, var_wgt) {
  std_var_cov = (var_cov - mean(var_cov))/sd(var_cov)
  simple_M1 = mean(std_var_cov[var_Department==1])
  simple_M0 = mean(std_var_cov[var_Department==0])
  simple_V1 = var(std_var_cov[var_Department==1])
  simple_V0 = var(std_var_cov[var_Department==0])
  wgted_M1 = Hmisc::wtd.mean(x=std_var_cov[var_Department==1], weights=var_wgt[var_Department==1])
  wgted_M0 = Hmisc::wtd.mean(x=std_var_cov[var_Department==0], weights=var_wgt[var_Department==0])
  wgted_V1 = Hmisc::wtd.var(x=std_var_cov[var_Department==1], weights=var_wgt[var_Department==1])
  wgted_V0 = Hmisc::wtd.var(x=std_var_cov[var_Department==0], weights=var_wgt[var_Department==0])
  B_wgt_Mdiff = simple_M1 - simple_M0
  B_wgt_Vratio = simple_V1 / simple_V0
  A_wgt_Mdiff = wgted_M1 - wgted_M0
  A_wgt_Vratio = wgted_V1 / wgted_V0
  balance_index = tibble(B_wgt_Mdiff, A_wgt_Mdiff,
                         B_wgt_Vratio, A_wgt_Vratio)
  balance_index
}

saveRDS(balance_check_PSW, "balance_check_PSW.RData")


#성향점수와 공변량의 균형성 점검

balance_check_PSW(burn$Department, burn$Gender, burn$Wate)
balance_check_PSW(burn$Department, burn$Comorbidities, burn$Wate)
balance_check_PSW(burn$Department, burn$Vaccination, burn$Wate)
balance_check_PSW(burn$Department, burn$Designation, burn$Wate)
balance_check_PSW(burn$Department, burn$Years, burn$Wate)
balance_check_PSW(burn$Department, burn$WorkedICU, burn$Wate)
balance_check_PSW(burn$Department, burn$workweek, burn$Wate)
balance_check_PSW(burn$Department, burn$PPE, burn$Wate)
balance_check_PSW(burn$Department, burn$mentalsupport, burn$Wate)
balance_check_PSW(burn$Department, burn$training, burn$Wate)
balance_check_PSW(burn$Department, burn$pscore, burn$Wate)

ㄴ

#처치효과ㅏ 추정 모수통계기법

lm(mean ~ Department+Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
   burn, weights = Wate)$coef['Department']
lm(mean ~ Department+Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
   burn, weights = Wate) %>%
  confint("Department")

#효과추정치 정리

ate=c(lm(mean ~ Department+Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
         burn, weights = Wate)$coef['Department'],
      lm(mean ~ Department+Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
         burn, weights = Wate) %>%
        confint("Department"))
att=c(lm(mean ~ Department+Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
         burn, weights = Watt)$coef['Department'],
      lm(mean ~ Department+Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
         burn, weights = Watt) %>%
        confint("Department"))
atc=c(lm(mean ~ Department+Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
         burn, weights = Watc)$coef['Department'],
      lm(mean ~ Department+Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
         burn, weights = Watc) %>%
        confint("Department"))

estimands_psw = data.frame(rbind(ate, att, atc))
names(estimands_psw)=c("PEst", "LL95", "UL95")
estimands_psw %>% dplyr::select(LL95, PEst, UL95) %>% round(3)



#ATE추정

set.seed(1234)
z_model=zelig(mean~Department+Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
              data=burn, model='ls', weights="Wate", cite=FALSE)
x_1 = setx(z_model, Department=1, data=burn)
x_0 = setx(z_model, Department=0, data=burn)
s_1 = sim(z_model, x_1, num=10000)
s_0 = sim(z_model, x_0, num=10000)
EST_ate = get_qi(s_1, "ev") - get_qi(s_0,"ev")
summary_est_ate=tibble(
  LL95=quantile(EST_ate, p=c(0.025)),
  PEst=quantile(EST_ate, p=c(0.500)),
  UL95=quantile(EST_ate, p=c(0.975)),
  estimand="ATE", model="Propensity score weighting"
)
summary_est_ate




#ATT추정
set.seed(1234)
z_model=zelig(mean~Department+Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
              data=burn, model='ls', weights="Watt", cite=FALSE)
x_1 = setx(z_model, Department=1, data=burn)
x_0 = setx(z_model, Department=0, data=burn)
s_1 = sim(z_model, x_1, num=10000)
s_0 = sim(z_model, x_0, num=10000)
EST_att = get_qi(s_1, "ev") - get_qi(s_0,"ev")
summary_est_att=tibble(
  LL95=quantile(EST_att, p=c(0.025)),
  PEst=quantile(EST_att, p=c(0.500)),
  UL95=quantile(EST_att, p=c(0.975)),
  estimand="ATT", model="Propensity score weighting"
)
summary_est_att



#ATC추정
set.seed(1234)
z_model=zelig(mean~Department+Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
              data=burn, model='ls', weights="Watc", cite=FALSE)
x_1 = setx(z_model, Department=1, data=burn)
x_0 = setx(z_model, Department=0, data=burn)
s_1 = sim(z_model, x_1, num=10000)
s_0 = sim(z_model, x_0, num=10000)
EST_atc = get_qi(s_1, "ev") - get_qi(s_0,"ev")
summary_est_atc=tibble(
  LL95=quantile(EST_atc, p=c(0.025)),
  PEst=quantile(EST_atc, p=c(0.500)),
  UL95=quantile(EST_atc, p=c(0.975)),
  estimand="ATC", model="Propensity score weighting"
)
summary_est_atc



#3가지 효과추정치 통합

PSW_estimands = bind_rows(
  summary_est_att, summary_est_atc, summary_est_ate
)
PSW_estimands


#시각화

PSW_estimands %>%
  ggplot(aes(x=estimand, y=PEst)) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=LL95, ymax=UL95), width=0.1, lwd=1) +
  labs(x="Estimands", y="Estimates, 95% Confidence Interval") +
  coord_cartesian(ylim=c(0.5, 2.5)) +
  theme_bw() +
  ggtitle("propensity score weighing")



#민감도 분석
SA_PSW_ATE = treatSens(mean~Department+Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
                       trt.family=binomial(link='probit'),
                       grid.dim = c(7,5),
                       nsim=20,
                       standardize = FALSE,
                       data=burn, weights=burn$Wate)
summary(SA_PSW_ATE)

SA_PSW_ATT = treatSens(mean~Department+Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
                       trt.family=binomial(link='probit'),
                       grid.dim = c(7,5),
                       nsim=20,
                       standardize = FALSE,
                       data=burn, weights=burn$Watt)      
summary(SA_PSW_ATT)

SA_PSW_ATC = treatSens(mean~Department+Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
                       trt.family=binomial(link='probit'),
                       grid.dim = c(7,5),
                       nsim=20,
                       standardize = FALSE,
                       data=burn, weights=burn$Watc)
summary(SA_PSW_ATC)

sensPlot(SA_PSW_ATE)
sensPlot(SA_PSW_ATT)
sensPlot(SA_PSW_ATC)




#성향점수기반 그리디매칭

set.seed(1234)
greedy_att = matchit(formula=Department ~ Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
                     data=burn,
                     distance="glm", link="linear.logit", 
                     method="nearest",
                     caliper=0.15,
                     discard='none',
                     ratio=2,
                     replace=FALSE)
summary(greedy_att)


set.seed(4321)
greedy_atc = matchit(formula=Rdepartment ~ Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
                     data=burn,
                     distance="glm", link="linear.logit", 
                     method="nearest",
                     caliper=0.15,
                     discard='none',
                     ratio=2,
                     replace=TRUE)
summary(greedy_atc)



#성향점수기반 최적매칭

set.seed(1234)
optimal_att = matchit(formula=Department ~ Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
                      data=burn,
                      distance="glm", link="linear.logit", 
                      method="optimal",
                      caliper=0.15,
                      discard='none',
                      ratio=2,
                      replace=FALSE)
summary(optimal_att)


set.seed(4321)
optimal_atc = matchit(formula=Rdepartment ~ Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
                      data=burn,
                      distance="glm", link="linear.logit", 
                      method="optimal",
                      caliper=0.15,
                      discard='none',
                      ratio=2,
                      replace=TRUE
)
summary(optimal_atc)



#성향점수기반 전체 매칭

set.seed(1234)
full_att = matchit(formula=Department ~ Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
                   data=burn,
                   distance="glm", link="linear.logit", 
                   method="full",
                   discard='none',
                   ratio=2)
summary(full_att)


set.seed(4321)
full_atc = matchit(formula=Rdepartment ~ Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
                   data=burn,
                   distance="glm", link="linear.logit", 
                   method="full",
                   discard='none',
                   ratio=2)
summary(full_atc)



#성향점수기반 유전매칭

genetic_att = matchit(formula = Department ~ Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
                      data=burn,
                      method="genetic", 
                      distance="glm", link="linear.logit",
                      pop.size=1000, 
                      discard = 'none',
                      ratio=2,
                      distance.tolerance=1e-05)
summary(genetic_att)

genetic_ate = matchit(formula = Department ~ Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
                      data=burn,
                      method="genetic", 
                      distance="glm", link="linear.logit",
                      pop.size=1000, 
                      discard = 'none',
                      ratio=2,
                      distance.tolerance=1e-05)
summary(genetic_ate)

genetic_atc = matchit(formula = Department ~ Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
                      data=burn,
                      method="genetic", 
                      distance="glm", link="linear.logit",
                      pop.size=1000, 
                      discard = 'none',
                      ratio=2,
                      distance.tolerance=1e-05)
summary(genetic_atc)



#마할라노비스 거리점수 기반 그리디 매칭

mahala_att = matchit(formula = Department ~ Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
                     data=burn,
                     mahvars = c("Age", "Gender", "Comorbidities", "Designation", "Years", "WorkedICU", "workweek", "PPE", "mentalsupport", "training"),
                     method = "nearest", 
                     caliper=0.15,
                     discard = "none",
                     ratio=2,
                     replace = FALSE)
summary(mahala_att)


mahala_atc = matchit(formula = Rdepartment ~ Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
                     data=burn,
                     mahvars = c("Age", "Gender", "Comorbidities", "Designation", "Years", "WorkedICU", "workweek", "PPE", "mentalsupport", "training"),
                     method = "nearest", 
                     caliper=0.15,
                     discard = "none",
                     ratio=2,
                     replace = FALSE)
summary(mahala_atc)



#공변량 균형성 점검
#성향점수 기반 그리디 매칭

set.seed(1234) 
greedy_att = matchit(formula = Department ~ Age+Gender+Comorbidities+Designation+Years+WorkedICU+workweek+PPE+mentalsupport+training,
                     data=burn,
                     distance="glm", link="linear.logit", 
                     method="nearest", 
                     caliper=0.15,
                     discard='none',
                     ratio=2, 
                     replace=FALSE) 
summary(greedy_att)
plot(greedy_att, type="QQ")
plot(greedy_att, type="hist")
plot(greedy_att, type="jitter")
plot(greedy_att, type="hist")


MD_greedy_att = match.data(greedy_att)
CreateTableone(vars = c("Age", "Gender", "Comorbidities", "Designation", "Years", "WorkedICU", "workweek", "PPE", "mentalsupport", "training"),
               data=MD_greedy_att,
               strata='treat',
               smd=TRUE)
CreateTableone(vars = c("Age", "Gender", "Comorbidities", "Designation", "Years", "WorkedICU", "workweek", "PPE", "mentalsupport", "training"),
               data=burn,
               strata='treat',
               smd=TRUE)
bal.tab(greedy_att,
        continuous = "std",
        s.d.denom = "pooled",
        m.threshold=0.1,
        v.threshold=2)

#시각화 히스토그램

bal.plot(greedy_att,
         var.name = "distance",
         which="bith",
         mirror=TRUE,
         type="histogram")
