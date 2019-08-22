-   [**Problem 1:** Median annual income of faculty members](#problem-1-median-annual-income-of-faculty-members)
    -   [**Section A** Randomization hypothesis test](#section-a-randomization-hypothesis-test)
    -   [**Section B** Bootstrapping for confidence interval](#section-b-bootstrapping-for-confidence-interval)
-   [**Problem 2:** Income differences in Males and Females?](#problem-2-income-differences-in-males-and-females)
    -   [**Section A:** Stating null and alternative hypotheses](#section-a-stating-null-and-alternative-hypotheses)
    -   [**Section B:** Simulation by using infer package:](#section-b-simulation-by-using-infer-package)
    -   [**Section C:** Computing the `p-value`](#section-c-computing-the-p-value)
    -   [**Section D:** Decision on the null hypothesis](#section-d-decision-on-the-null-hypothesis)
    -   [**Section E:** Finding the confidence intervals:](#section-e-finding-the-confidence-intervals)
    -   [**Section F:** Traditional tests for Problem 2 above:](#section-f-traditional-tests-for-problem-2-above)

``` r
library(tidyverse)
knitr::opts_chunk$set(comment=NA)
library(infer)
library(ggplot2)
```

In this work, we will study two problems. In Problem 1, we will be given 25 income amounts of faculty members and we are going to test if the median of the population income is less than a claimed value. In Probelem 2, based on American Community Survey, 2012, we will explore if there is a difference in the mean incomes of males and females.

**Problem 1:** Median annual income of faculty members
======================================================

A sample of twenty-five random faculty members at a university produced the following annual income amounts:

85995, 63094, 74389, 72357, 65584, 78792, 62362, 99567, 79496, 65548, 69233, 65753, 69171, 89415, 73920, 63684, 73889, 74228, 64240, 85067, 75021, 69238, 67248, 95021,85000

**We are going to explore that if the survey results indicate that the median annual income of this university's faculty population is less than than $75,000/year.**

First, we define our null and alternative hypotheses as follows:

*H*<sub>0</sub>: median annual income M = 75,000

*H*<sub>*A*</sub>: median annual income M &lt; 75,000

We will take oursignificance level for *α* as 0.05 (5%).

First, let's find the median of the observed sample, i.e. the median of the 25 faculty income values:

``` r
incomes <- data.frame(income = c(85995, 63094, 74389, 72357, 65584, 78792, 62362, 99567, 79496, 65548, 69233, 65753, 69171, 89415, 73920, 63684, 73889, 74228, 64240, 85067, 75021, 69238, 67248, 95021,85000))
observed_sample_median <- median(incomes$income)
observed_sample_median
```

    [1] 73889

### **Section A** Randomization hypothesis test

-   We are going to use a bootstrap test to reject or accept the null hypothesis above. By using `infer` package, we `specify`, `hypothesize`, `generate` and `calculate` respectively. By using `bootstrapping` method, we will generate 15,000 samples of size 25. The `stat` created will be the medians of each sample.

``` r
n_replicates <- 15000
# Generate 15000 bootstrap samples centered at null
incomes_med_ht <- incomes %>%
  specify(response = income) %>%
  # Use a point hypothesis with a median of 75000
  hypothesize(null = "point", med = 75000) %>% 
  generate(reps = n_replicates, type = "bootstrap") %>% 
  calculate(stat = "median")
```

    Warning: `lgl_len()` is deprecated as of rlang 0.2.0.
    Please use `new_logical()` instead.
    This warning is displayed once per session.

    Warning: `is_lang()` is deprecated as of rlang 0.2.0.
    Please use `is_call()` instead.
    This warning is displayed once per session.

    Warning: `lang()` is deprecated as of rlang 0.2.0.
    Please use `call2()` instead.
    This warning is displayed once per session.

    Warning: The `printer` argument is deprecated as of rlang 0.3.0.
    This warning is displayed once per session.

    Warning: `new_overscope()` is deprecated as of rlang 0.2.0.
    Please use `new_data_mask()` instead.
    This warning is displayed once per session.

``` r
# See the result
incomes_med_ht
```

    # A tibble: 15,000 x 2
       replicate  stat
           <int> <dbl>
     1         1 75031
     2         2 75339
     3         3 70344
     4         4 75000
     5         5 73468
     6         6 73468
     7         7 75339
     8         8 75031
     9         9 70282
    10        10 73468
    # ... with 14,990 more rows

In the below code, we calculate the p-value. Since *H*<sub>*A*</sub> is defined as the median to be the less that 75,000, the p-value corresponds to the left area of the observed median on the null distribution constructed above.

``` r
incomes_med_ht %>%
  # Filter for bootstrap stat less than or equal to observed stat
  filter(stat<=  observed_sample_median) %>%
  # Calculate the p-value
  summarize(p_val = n() / n_replicates)
```

    # A tibble: 1 x 1
      p_val
      <dbl>
    1 0.421

We fail to reject *H*<sub>0</sub> since the p-value (0.4279333) is above the significance level (*α* = 0.05), and conclude that the data do not provide convincing evidence that the median annual income of this university's faculty population is less than $75,000.

### **Section B** Bootstrapping for confidence interval

Now we are going to create a 95% confidence interval for the population median using bootstrapping on the sample data. In order to do this, we are going to present two methods: **percentile** and **standard error methods**. For each method, we will provide confidence interval.

``` r
# Calculate 15000 bootstrap medians of incomes
set.seed(12345) # in order to get the same results, we use set.seed()
income_median_ci <- incomes %>%
  specify(response = income)  %>%
  generate(reps = 15000, type = "bootstrap")  %>%
  calculate(stat = "median")
  
# See the result
income_median_ci
```

    # A tibble: 15,000 x 2
       replicate  stat
           <int> <dbl>
     1         1 69233
     2         2 69233
     3         3 74389
     4         4 72357
     5         5 73920
     6         6 74228
     7         7 73920
     8         8 74228
     9         9 73920
    10        10 73920
    # ... with 14,990 more rows

Every value of `stat` is the median income of a sample constructed using `bootstrapping` method, and there are 15,000 of samples, thus 15,000 of values in the `stat` column .

Now, let's plot the histogram using the following R-code:

``` r
# View its structure
#str(rent_med_ci)
# Plot the rent_med_ci statistic
ggplot(income_median_ci, aes(stat)) +
  # Make it a histogram with a binwidth of 1000
  geom_histogram(binwidth = 1000)
```

![](PracticalStatisticsForDataScientists_files/figure-markdown_github/unnamed-chunk-5-1.png)

As seen in the plot, the medians do not look like normal. Thus we suggest to use percentile even though we will present two methods: percentile and standard error methods.

**Percentile Method:** We use the following R-code to calculate a 95% confidence interval for the bootstrap medians.

``` r
# Calculate the 95% CI via percentile method
income_median_ci %>%
  summarize(
    l = quantile(stat,0.025),
    u = quantile(stat, 0.975)
  )
```

    # A tibble: 1 x 2
          l     u
      <dbl> <dbl>
    1 67248 78792

The percentile method gives us 95% confidence interval for the population median of income as \[67,248, 78,792\].

**Standard Error Method:** We should first find the t-score and degrees of freedom.

``` r
# Calculate the degrees of freedom
degrees_of_freedom <- nrow(incomes)-1
  
# Determine the critical value
t_star <- qt(0.975, degrees_of_freedom)
t_star
```

    [1] 2.063899

The degrees of freedom is 24 and the critical t-score is 2.063899. In the below code, we use the standard error formula for the confidence interval.

``` r
# Calculate the CI using the std error method
income_median_ci %>%
  # Calculate the std error of the statistic
  summarize(income_se = sd(stat)) %>%
  # Calculate the lower and upper limits of the CI
  summarize(
    l = observed_sample_median - t_star * income_se,
    u = observed_sample_median + t_star * income_se
  )
```

    # A tibble: 1 x 2
           l      u
       <dbl>  <dbl>
    1 68625. 79153.

Using standard error method, we get 95% confidence interval for the population median of income as \[68,624.9, 79,153.1\].

-   Do our results confirm Part A above?

The confidence intervals obtained using both percentile method and standard error methods, include the null value=75,000. Since we failed to reject the null hypothesis, it totally makes sense.

**Problem 2:** Income differences in Males and Females?
=======================================================

In this problem, we are going to explore if there is a difference between the mean income in females and mean income in males. We are going to use the dataset: `acs12.csv` which can be obtained here: <https://www.openintro.org/stat/data.php?data=acs12>. The results are from the US Census American Community Survey, 2012. Our aim will be to explore if the sample indicates a difference in mean population incomes for the two genders.

Let's first glimpse our data.

``` r
acs12 <- read.csv('acs12.csv')
glimpse(acs12)
```

    Observations: 2,000
    Variables: 13
    $ income       <int> 60000, 0, NA, 0, 0, 1700, NA, NA, NA, 45000, NA, ...
    $ employment   <fct> not in labor force, not in labor force, NA, not i...
    $ hrs_work     <int> 40, NA, NA, NA, NA, 40, NA, NA, NA, 84, NA, 23, N...
    $ race         <fct> white, white, white, white, white, other, white, ...
    $ age          <int> 68, 88, 12, 17, 77, 35, 11, 7, 6, 27, 8, 69, 69, ...
    $ gender       <fct> female, male, female, male, female, female, male,...
    $ citizen      <fct> yes, yes, yes, yes, yes, yes, yes, yes, yes, yes,...
    $ time_to_work <int> NA, NA, NA, NA, NA, 15, NA, NA, NA, 40, NA, 5, NA...
    $ lang         <fct> english, english, english, other, other, other, e...
    $ married      <fct> no, no, no, no, no, yes, no, no, no, yes, no, no,...
    $ edu          <fct> college, hs or lower, hs or lower, hs or lower, h...
    $ disability   <fct> no, yes, no, no, yes, yes, no, yes, no, no, no, n...
    $ birth_qrtr   <fct> jul thru sep, jan thru mar, oct thru dec, oct thr...

We have 2000 observations and 13 variables but we will be interested in only two variables: `income` and `gender`.We also want to filter the `NA` values and also the ones that are "employed".

``` r
acs12_filtered<-acs12%>% filter(!is.na(income)&!is.na(gender))%>%filter(employment=="employed")
nrow(acs12_filtered)
```

    [1] 843

It seems like we have 843 observations after we filtered our data. Since we have no missing values now, we are ready to use `infer` package and perform a permutation test for the difference between mean incomes for the two genders.

### **Section A:** Stating null and alternative hypotheses

In this section, we will state the null and alternative hypotheses.

Let's say the mean income for females is *μ*<sub>1</sub> and males is *μ*<sub>2</sub>. Then our null and alternative hypotheses are:

*H*<sub>0</sub>: *μ*<sub>1</sub> − *μ*<sub>2</sub> = 0

*H*<sub>*A*</sub>: *μ*<sub>1</sub> − *μ*<sub>2</sub> ≠ 0

### **Section B:** Simulation by using infer package:

In this section, we will use infer to perform a permutation test for the difference between mean incomes for the two genders.

**<span style="color:red">Solution:</span>**

First, we have to find the observed mean difference. The following R-code helps us find that difference. *Note that we will not use `pull()` and `diff()` since the sign may end up with something we may not want.*

``` r
# Calculate observed difference in means
 mean_incomes_gender<-acs12_filtered %>%
  # Group by habit group
  group_by(gender) %>%
  # Calculate mean weight for each group
  summarize(mean_incomes = mean(income)) 
mean_incomes_gender
```

    # A tibble: 2 x 2
      gender mean_incomes
      <fct>         <dbl>
    1 female       29244.
    2 male         55887.

``` r
diff_mean_obs<- mean_incomes_gender$mean_incomes[1]-mean_incomes_gender$mean_incomes[2]
diff_mean_obs
```

    [1] -26643.53

The observed mean difference is found as -26643.53.

Now we are going to construct a null distribution using `permute`. In the following R-code, we generate 1500 samples and the `stat` are the mean differences of the two cities for 1500 samples.

``` r
n_replicates <- 1500
# Generate 1500 differences in means via randomization
diff_mean_ht <- acs12_filtered  %>% 
  # Specify income vs. gender
  specify(income~gender) %>% 
  # Null = no difference between means
  hypothesize(null="independence") %>% 
  # Shuffle labels 1500 times
  generate(reps=1500, type="permute") %>%
  # Calculate test statistic, male then female
  calculate(stat="diff in means", c("male", "female"))

# See the result
diff_mean_ht
```

    # A tibble: 1,500 x 2
       replicate   stat
           <int>  <dbl>
     1         1  -280.
     2         2  -333.
     3         3  4006.
     4         4  1863.
     5         5  4863.
     6         6  -740.
     7         7 -3762.
     8         8   825.
     9         9 -6134.
    10        10 -1770.
    # ... with 1,490 more rows

### **Section C:** Computing the `p-value`

Since we have a two tail test, we will find the area where the stat is smaller than the observed data and then we will multiply it by two. The following code does it for us.

``` r
# pvalue 

diff_mean_ht %>%
  # Identify simulated test statistics at least as extreme as observed
  filter(stat<diff_mean_obs) %>%
  # Calculate p-value
  summarize(
    one_sided_p_val = n()/n_replicates,
    two_sided_p_val = 2*one_sided_p_val)
```

    # A tibble: 1 x 2
      one_sided_p_val two_sided_p_val
                <dbl>           <dbl>
    1               0               0

The two-sided p-value is 0.

### **Section D:** Decision on the null hypothesis

Since the two sided p-value is smaller than 0.08, we do reject the null hypothesis.

### **Section E:** Finding the confidence intervals:

In this section, we will use bootstrapping to compute a 92% confidence interval for the difference in mean incomes between the two genders.

Using `infer` and `bootstrapping` method, below we generate 1500 samples and calculate their mean differences.

``` r
set.seed(1234)
# Generate 1500 bootstrap difference in means
diff_mean_ci <- acs12_filtered  %>%
  # Specify income vs. gender
  specify(income~gender) %>% 
  # Generate 1500 bootstrap replicates
  generate(reps=1500, type="bootstrap") %>%
  # Calculate the difference in means, female then male
  calculate(stat="diff in means", c("female", "male"))

#see the result
diff_mean_ci
```

    # A tibble: 1,500 x 2
       replicate    stat
           <int>   <dbl>
     1         1 -29084.
     2         2 -27534.
     3         3 -23633.
     4         4 -28912.
     5         5 -22116.
     6         6 -30892.
     7         7 -24302.
     8         8 -24560.
     9         9 -27635.
    10        10 -33736.
    # ... with 1,490 more rows

We are going to use two methods to compute the confidence interval: `Percentile Method` and `Standard Error Method`.

**Percentile Method:** Now, let's compute a 92% confidence interval for the difference in mean responses between the two cities. Note that the lower and upper limits for 92% confidence interval are 0.04 and 0.96 respectively.

``` r
# Calculate the 92% CI via percentile method
diff_mean_ci %>%
  summarize(
    l = quantile(stat,0.04),
    u = quantile(stat, 0.96)
  )
```

    # A tibble: 1 x 2
            l       u
        <dbl>   <dbl>
    1 -33394. -20331.

The 92% confidence interval is \[-33,393.78,-20,330.57\].

**Standard Error Method:** We should first find the t-score and degrees of freedom.

``` r
# Calculate the degrees of freedom
degrees_of_freedom <- nrow(acs12_filtered)-1
degrees_of_freedom
```

    [1] 842

``` r
# Determine the critical value
t_star <- qt(0.96, degrees_of_freedom)
t_star
```

    [1] 1.752802

The degrees of freedom is 842 and the critical t-score is 1.752802. In the below code, we use the standard error formula for the confidence interval.

``` r
# Calculate the CI using the std error method
diff_mean_ci %>%
  # Calculate the std error of the statistic
  summarize(incomes_se = sd(stat)) %>%
  # Calculate the lower and upper limits of the CI
  summarize(
    l = diff_mean_obs - t_star * incomes_se,
    u = diff_mean_obs + t_star * incomes_se
  )
```

    # A tibble: 1 x 2
            l       u
        <dbl>   <dbl>
    1 -33104. -20183.

By using standard error method, we can say that the 92% confidence interval is \[-33,104.23 ,-20,182.84\].

As observed, the percentile and the standard error methods give very close confidence intervals.

The confindence intervals calculated using percentile and standard error methods, do not contain the null value=0. This is expected because we did reject the null hypothesis in part D.

### **Section F:** Traditional tests for Problem 2 above:

#### Performing t-test:

In this section we are going to use a traditional t-test to confirm our results for Problem 2 above. First we use `t.test()` to confirm our results. Following code helps us to achieve our goal. Note that we should set the confidence level as 0.92 since the default value is 0.95.

``` r
# Construct 92% CI using a t-test
test_results <-t.test(income~gender, data = acs12_filtered, null = 0, alternative = "two.sided",conf.level = 0.92)

# See the results
test_results
```


        Welch Two Sample t-test

    data:  income by gender
    t = -7.4437, df = 694.94, p-value = 2.903e-13
    alternative hypothesis: true difference in means is not equal to 0
    92 percent confidence interval:
     -32918.97 -20368.10
    sample estimates:
    mean in group female   mean in group male 
                29243.70             55887.23 

The results obtained here are similar to what we got in the previous sections. The p-value is almost 0 and the confidence interval \[-32,918.97, -20,368.10\] is very close to what we got previously for both methods.

#### Performing an ANOVA test:

We are going to use `aov()` to perform ANOVA test. We use the following code:

``` r
library(broom)
# Run an analysis of variance on income vs. gender
aov_income_gender <- aov(data=acs12_filtered,income~gender)

# Tidy the model
tidy(aov_income_gender)
```

           term  df        sumsq       meansq statistic     p.value
    1    gender   1 1.476260e+11 147625976576  47.76128 9.51035e-12
    2 Residuals 841 2.599458e+12   3090913181        NA          NA

The total variability in the response variable is 1.476260e+11+2.599458e+12=27.47e+11

Percentage of explained variability is 1.476260e+11/27.47e+11=5.37%. F-statistic = 47.76128.

The p-value, which is almost 0, tells us we should reject the null hypothesis which states that there is no difference between the mean incomes of the two genders.

#### Conditions for ANOVA:

We have to check three conditions: *independence*, *approximate normality* and *equal variance*.

##### Checking the independence:

We assume that sampled observations are independent and the groups are independent of each other.

##### Checking the normality condition

We are going to check if distribution of the response variable is nearly normal within each group. In order to achieve that we will plot the histogram of each distribution. We are going to take the `log` of the income in order to make the histograms less skewed.

``` r
ggplot(acs12_filtered, mapping = aes(x=log(income))) +
  # Add a histogram layer
  geom_histogram() +
  # Facet by class
  facet_wrap(~gender)
```

    `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    Warning: Removed 56 rows containing non-finite values (stat_bin).

![](PracticalStatisticsForDataScientists_files/figure-markdown_github/unnamed-chunk-21-1.png)

By looking at the histograms, we can say that both look like approximately normally distributed.

#### Checking the equal variance condition

In addition to checking the normality of distributions of incomes across gender, we need to check that the variances from each are roughly equal.

``` r
acs12_filtered %>%
  # Group by class
  group_by(gender) %>%
  # Calculate the std dev of wordsum as std_dev_wordsum
  summarize(std_dev_incomes=sd(income))
```

    # A tibble: 2 x 2
      gender std_dev_incomes
      <fct>            <dbl>
    1 female          32026.
    2 male            68768.

We see that the standard deviations are not close to each other. However, if we take the `log` we get the log standard deviations as log 10.3743, 11.13849.

Even though, the coditions for ANOVA are not satisfied for `income` but for its log, the results of ANOVA test confirms our answers above. The p-value obtained using ANOVA and the p-value we obtained before are both almost 0. It means we do reject the null hypothesis. We can conclude that the data does provide convincing evidence that the mean response difference of the two genders is different than 0.
