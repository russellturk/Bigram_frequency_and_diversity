library(brms)

###Read in appropriate .csv file
df <- read.csv(Meta_raw)

###Log transform variables
df$bigram_freq <- log(df$bigram_freq + .000001)
df$diversity <- log(df$diversity + .000001)
df$trans_prob <- log(df$trans_prob + .000001)
df$word_freq <- log(df$word_freq + .000001)
df$response_time <- log(df$response_time + .000001)

##define priors

priorscov <- c(prior("normal(0, .01)", class = "b", coef = word_freq), 
             prior("normal(0, .01)", class = "b", coef = concreteness),
             prior("normal(.01, .01)", class = "b", coef = letters), 
             prior("normal(.01, .01)", class = "b", coef = age))

priors_all <- c(prior("normal(0, .01)", class = "b", coef = word_freq), 
                prior("normal(0, .01)", class = "b", coef = concreteness),
                prior("normal(.01, .01)", class = "b", coef = letters), 
                prior("normal(.01, .01)", class = "b", coef = age),
                prior("normal(-.01,.01)", class = "b", coef = bigram_freq),
                prior("normal(-.01, .01)", class = "b", coef = diversity),
                prior("normal(0,.01)", class = "b", coef = trans_prob))

priors_freq_div <- c(prior("normal(0, .01)", class = "b", coef = word_freq), 
                prior("normal(0, .01)", class = "b", coef = concreteness),
                prior("normal(.01, .01)", class = "b", coef = letters), 
                prior("normal(.01, .01)", class = "b", coef = age),
                prior("normal(-.01,.01)", class = "b", coef = bigram_freq),
                prior("normal(-.01, .01)", class = "b", coef = diversity))

priors_freq_tp <- c(prior("normal(0, .01)", class = "b", coef = word_freq), 
                prior("normal(0, .01)", class = "b", coef = concreteness),
                prior("normal(.01, .01)", class = "b", coef = letters), 
                prior("normal(.01, .01)", class = "b", coef = age),
                prior("normal(-.01,.01)", class = "b", coef = bigram_freq),
                prior("normal(0,.01)", class = "b", coef = trans_prob))

priors_div_tp <- c(prior("normal(0, .01)", class = "b", coef = word_freq), 
                prior("normal(0, .01)", class = "b", coef = concreteness),
                prior("normal(.01, .01)", class = "b", coef = letters), 
                prior("normal(.01, .01)", class = "b", coef = age),
                prior("normal(-.01, .01)", class = "b", coef = diversity),
                prior("normal(0,.01)", class = "b", coef = trans_prob))

priors_freq <- c(prior("normal(0, .01)", class = "b", coef = word_freq), 
                prior("normal(0, .01)", class = "b", coef = concreteness),
                prior("normal(.01, .01)", class = "b", coef = letters), 
                prior("normal(.01, .01)", class = "b", coef = age),
                prior("normal(-.01,.01)", class = "b", coef = bigram_freq))

priors_div <- c(prior("normal(0, .01)", class = "b", coef = word_freq), 
                prior("normal(0, .01)", class = "b", coef = concreteness),
                prior("normal(.01, .01)", class = "b", coef = letters), 
                prior("normal(.01, .01)", class = "b", coef = age),
                prior("normal(-.01, .01)", class = "b", coef = diversity))

priors_tp <- c(prior("normal(0, .01)", class = "b", coef = word_freq), 
                prior("normal(0, .01)", class = "b", coef = concreteness),
                prior("normal(.01, .01)", class = "b", coef = letters), 
                prior("normal(.01, .01)", class = "b", coef = age),
                prior("normal(0,.01)", class = "b", coef = trans_prob))

##run models

covariate <- brm(response_time ~ age + concreteness + letters + word_freq + (1|subject_nr) + 
                (1|item), data = df, prior = priorscov, save_all_pars = TRUE)

model_a <- brm(response_time ~ age + concreteness + letters + word_freq + trans_prob + bigram_freq
                 + diversity + (1|subject_nr) + (1|item), data = df, prior = priors_all,
                 save_all_pars = TRUE)

model_b <- brm(response_time ~ age + concreteness + letters + word_freq + bigram_freq
                 + diversity + (1|subject_nr) + (1|item), data = df, prior = priors_freq_div,
                 save_all_pars = TRUE)

model_c <- brm(response_time ~ age + concreteness + letters + word_freq+ trans_prob + bigram_freq
                 + (1|subject_nr) + (1|item), data = df, prior = priors_freq_tp,
                 save_all_pars = TRUE)

model_d <- brm(response_time ~ age + concreteness + letters + word_freq + trans_prob
                 + diversity + (1|subject_nr) + (1|item), data = df, prior = priors_div_tp,
                 save_all_pars = TRUE)

model_e <- brm(response_time ~ age + concreteness + letters + word_freq + bigram_freq
                 + (1|subject_nr) + (1|item), data = df, prior = priors_freq,
                 save_all_pars = TRUE)

model_f <- brm(response_time ~ age + concreteness + letters + word_freq
                 + diversity + (1|subject_nr) + (1|item), data = df, prior = priors_div,
                 save_all_pars = TRUE)

model_g <- brm(response_time ~ age + concreteness + letters + word_freq + trans_prob
                 + (1|subject_nr) + (1|item), data = df, prior = priors_tp,
                 save_all_pars = TRUE)

##cross-validation
loo(covariate)
loo(model_a)
loo(model_b)
loo(model_c)
loo(model_d)
loo(model_e)
loo(model_f)
loo(model_g)

####Bayes factors
b1 <- bayes_factor(covariate, model_a)
b2 <- bayes_factor(covariate, model_b)
b3 <- bayes_factor(covariate, model_c)
b4 <- bayes_factor(covariate, model_d)
b5 <- bayes_factor(covariate, model_e)
b6 <- bayes_factor(covariate, model_f)
b7 <- bayes_factor(covariate, model_g)
b8 <- bayes_factor(model_g, model_a)
b9 <- bayes_factor(model_g, model_b)
b10 <- bayes_factor(model_g, model_c)
b11 <- bayes_factor(model_g, model_d)
b12 <- bayes_factor(model_g, model_e)
b13 <- bayes_factor(model_g, model_f)
b14 <- bayes_factor(model_f, model_a)
b15 <- bayes_factor(model_f, model_b)
b16 <- bayes_factor(model_f, model_c)
b17 <- bayes_factor(model_f, model_d)
b18 <- bayes_factor(model_f, model_e)
b19 <- bayes_factor(model_e, model_a)
b20 <- bayes_factor(model_e, model_b)
b21 <- bayes_factor(model_e, model_c)
b22 <- bayes_factor(model_e, model_d)
b23 <- bayes_factor(model_d, model_a)
b24 <- bayes_factor(model_d, model_b)
b25 <- bayes_factor(model_d, model_c)
b26 <- bayes_factor(model_c, model_a)
b27 <- bayes_factor(model_c, model_b)
b28 <- bayes_factor(model_b, model_a)

###Binary logistic regression
meta_blr <- glm(correct ~ bigram_freq + diversity + trans_prob + age + concreteness + word_freq + letters,
                data=df, family="binomial")