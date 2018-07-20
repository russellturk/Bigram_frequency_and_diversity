library(brms)

###Read in appropriate .csv file
df <- read.csv(Exp3_raw)

###Log transform variables
df$bigram_freq <- log(df$bigram_freq + .000001)
df$trans_prob <- log(df$trans_prob + .000001)
df$word_freq <- log(df$word_freq + .000001)
df$response_time <- log(df$response_time + .000001)


##define priors
priorscov <- c(prior("normal(0, .01)", class = "b", coef = word_freq), 
               prior("normal(0, .01)", class = "b", coef = concreteness),
               prior("normal(.01, .01)", class = "b", coef = letters), 
               prior("normal(.01, .01)", class = "b", coef = age))

priors_freq_tp <- c(prior("normal(0, .01)", class = "b", coef = word_freq), 
                    prior("normal(0, .01)", class = "b", coef = concreteness),
                    prior("normal(.01, .01)", class = "b", coef = letters), 
                    prior("normal(.01, .01)", class = "b", coef = age),
                    prior("normal(-.01,.01)", class = "b", coef = bigram_freq),
                    prior("normal(0,.01)", class = "b", coef = trans_prob))

priors_freq <- c(prior("normal(0, .01)", class = "b", coef = word_freq), 
                 prior("normal(0, .01)", class = "b", coef = concreteness),
                 prior("normal(.01, .01)", class = "b", coef = letters), 
                 prior("normal(.01, .01)", class = "b", coef = age),
                 prior("normal(-.01,.01)", class = "b", coef = bigram_freq))

priors_tp <- c(prior("normal(0, .01)", class = "b", coef = word_freq), 
               prior("normal(0, .01)", class = "b", coef = concreteness),
               prior("normal(.01, .01)", class = "b", coef = letters), 
               prior("normal(.01, .01)", class = "b", coef = age),
               prior("normal(0,.01)", class = "b", coef = trans_prob))
###run models
covariate <- brm(response_time ~ age + concreteness + letters + word_freq + (1|subject_nr) + 
                   (1|item), data = df, save_all_pars = TRUE)

model_a <- brm(response_time ~ age + concreteness + letters + word_freq + trans_prob + bigram_freq
               + (1|subject_nr) + (1|item), data = df, prior = priors_freq_tp, save_all_pars = TRUE)

model_b <- brm(response_time ~ age + concreteness + letters + word_freq + bigram_freq
               + (1|subject_nr) + (1|item), data = df, prior = priors_freq, save_all_pars = TRUE)

model_c <- brm(response_time ~ age + concreteness + letters + word_freq + trans_prob
               + (1|subject_nr) + (1|item), data = df, prior = priors_tp, save_all_pars = TRUE)

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
exp1_blr <- glm(correct ~ bigram_freq + trans_prob + age + concreteness + word_freq + letters,
                data=df, family="binomial")