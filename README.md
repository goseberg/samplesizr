# samplesizr

Sample size calculation with R based on the book

[1] M.Kieser: Fallzahlberechnung in der medizinischen Forschung [2018], 1th Edition.



There are the following functions for sample size calculation:

* n_ancova,
* n_chisq,
* n_chisq_mult_groups,
* n_ftest,
* n_ttest,
* n_ztest,

and for power calculation

* power_binomial,
* power_chisq_mult_groups,
* power_ftest,
* power_ttest,
* power_ztest.

## Sample size calculation

### n_ancova

Sample Size Calculation for the Analysis of Covariances (ANCOVA)
for test on mean difference for two samples correlated to a covariate.
See pages 18 - 20 in [1] for further details.

### n_chisq

Sample size Calculation for the Chi-Square Test for two
independent samples with binary data using the absolute rate
difference quantifying the effect of an intervention.
See pages 21 - 26 in [1] for further details.

### n_chisq_mult_groups

Sample size calculation for the Chi-Square test comparing
rates of \eqn{k > 2} independent samples.
See page 30 in [1] for further details.

### n_ftest

Sample size calculation for the f-Test comparing
means of \eqn{k > 2} independent samples.
See page 29 in [1] for further details.

### n_ttest

Sample size calculation for the t-Test comparing
two independent samples.
See pages 16 - 18 in [1] for further details.

### n_ztest
Sample size calculation for the z-Test comparing
two independent samples.
See pages 13 - 16 in [1] for further details.

## Power calculation

### power_binomial
Power Calculation for the Chi-Square Test for two
independent samples with binary data using the absolute rate
difference quantifying the effect of an intervention.
See pages 21 - 26 in [1] for further details.

### power_chisq_mult_groups
Power calculation for the Chi-Square test comparing
rates of \eqn{k > 2} independent samples.
See page 30 in [1] for further details.

### power_ftest
Power calculation for the f-Test comparing
means of \eqn{k > 2} independent samples.
See page 29 in [1] for further details.

### power_ttest
Power calculation for the t-Test comparing
two independent samples.
See pages 16 - 18 in [1] for further details.

###power_ztest
Power calculation for the z-Test comparing
two independent samples.
See pages 13 - 16 in [1] for further details.
