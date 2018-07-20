library(brms)

###Read in appropriate .csv file
df <- read.csv(Exp2_raw)

###Log transform variables
df$diversity <- log(df$diversity + .000001)
df$trans_prob <- log(df$trans_prob + .000001)
df$word_freq <- log(df$word_freq + .000001)
df$response_time <- log(df$response_time + .000001)

##run models

covariate <- brm(response_time ~ age + concreteness + letters + word_freq + (1|subject_nr) + 
                   (1|item), data = df, save_all_pars = TRUE)

model_a <- brm(response_time ~ age + concreteness + letters + word_freq + trans_prob
               + diversity + (1|subject_nr) + (1|item), data = df, save_all_pars = TRUE)

model_b <- brm(response_time ~ age + concreteness + letters + word_freq
               + diversity + (1|subject_nr) + (1|item), data = df, prior = priors_div,
               save_all_pars = TRUE)

model_c <- brm(response_time ~ age + concreteness + letters + word_freq + trans_prob
               + (1|subject_nr) + (1|item), data = df, prior = priors_tp,
               save_all_pars = TRUE)

###cross-validation
loo(covariate)
loo(model_a)
loo(model_b)
loo(model_c)

###Bayes factors
b1 <- bayes_factor(covariate, model_a)
b2 <- bayes_factor(covariate, model_b)
b3 <- bayes_factor(covariate, model_c)
b4 <- bayes_factor(model_c, model_a)
b5 <- bayes_factor(model_c, model_b)
b6 <- bayes_factor(model_b, model_a)

###Binary logistic regression
exp1_blr <- glm(correct ~ diversity + trans_prob + age + concreteness + word_freq + letters,
                data=df, family="binomial")