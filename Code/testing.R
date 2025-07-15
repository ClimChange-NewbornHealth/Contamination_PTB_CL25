# testing
formula <- as.formula(paste("lbw", " ~ ", combinations$predictor[1], 
                              "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(month_week1) + factor(year_week1) + factor(covid) + vulnerability"))
  

tic()
model_fit <- glm(formula, data = exp_data, family = binomial(link = "logit"))
toc()

tic()
broom::tidy(model_fit, exponentiate = TRUE, conf.int = TRUE, conf.method = "wald")
toc()
