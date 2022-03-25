# ispor-europe2021-gas-sim

Code for the ISPOR Europe 2021 abstract **The Goal Attainment Scaling Method is Robust to Violations of Normality in Goal Scales: A Simulation Study**

## Background

Goal attainment scaling (GAS) is a patient-centric outcome measure that captures meaningful change through personally identified treatment goals. For each goal, a 5-point attainment scale is defined to describe possible outcomes, with the midpoint representing the “expected outcome”. A key assumption in GAS analysis is that the scores on the scale approximate a normal distribution. Using data simulation techniques, we investigated whether GAS performance varied if the assumption of normality was violated.

## Methods

We employed a latent variable model to generate GAS data (Urach et al. 2019). We varied the number of subjects (40, 60, 80; equally assigned to control or treatment), treatment effect size (0.3, 0.5, 0.7, 0.9 and 1.1), and number of goals per subject (1-5 goals). Latent goal scores were discretized into 5-point scales using either: “uniform” intervals that were spaced to return a uniform score distribution, and “normal” intervals that were spaced to approximate a normal distribution. For each set of parameters, 1000 trials were simulated.  Two-sided t-tests were performed on summary T-scores. Power was the percentage of simulations detecting a significant treatment effect (α=0.05).

## Results

The statistical power did not vary significantly whether using uniform or normal intervals, with less than 1% difference for most parameter combinations. As expected, T-scores were higher when using uniform intervals. However, the standardized effect sizes were equal across small-medium effect sizes (<0.7), as typically seen in most GAS studies. At the higher effect sizes, the uniform intervals underestimated the treatment effect compared to normal intervals (difference of 0.03-0.07). This difference is unlikely to impact interpretation of GAS results.

## Conclusions

Violating the distributional assumptions of GAS data is unlikely to affect the power to detect a treatment effect at effect sizes typically seen in GAS studies. The GAS method is robust to perturbations in normality of goal scores.
