
# user selections ---------------------------------------------------------

test <- "means"
t_type <- "paired"
alt <- "two.sided"
alpha <- 0.05
d <- effect_table$means[2] # medium effect... these values will be set in the 
                           # select input / radio
n <- 70


# what happens ------------------------------------------------------------

# this object will be passed into the first plot
res <- t_table(t_type=t_type, alt=alt, alpha=alpha, n=n, d=d)

# this object contains user's selected power result. Compare if power >= 0.8
compare <- t_compare(t_type=t_type, alt=alt, alpha=alpha, n=n, d=d)
  


# make the plots ----------------------------------------------------------

# left -- for the chosen effect size
power_effect(data=res, n=n)
# right -- for the chosen sample size
power_bar(res, n, effect_table$means)


# make the cards ----------------------------------------------------------

# card 1 is only the selected effect size, so no magic needed

# card 2 is the minimum sample size for 80% power
min_sample(t_type=t_type, alt=alt, alpha=alpha, d=d)

# card 3 assess if the proposed parameters <= 80% power
if (compare$power < 0.8) {
  "too low!"
} else { "good!" }
