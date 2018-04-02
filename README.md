# Problem statement
Differences in pay between women and men is a troubling and well-documented phenomena that occurs around the globe. In the United States, it is estimated that women make roughly $0.80 for every dollar that men make.

Such patterns are particularly troubling when they are observed in public sector jobs, as governments should not only enforce but also demonstrate fair and balanced employment practices.

Here, we investigated wage discrepancy within the City of Seattle.

# Summary
The City of Seattle largely pays men and women equal wages for the same job (median wage difference = $0).

![For a given job title, men and women are likely to receive similar hourly wages.](https://github.com/bochocki/seattle_gender_pay_disparity/blob/master/wage_differences_histogram.png)

However, the city employs vastly more men than women. The imbalance in employment is particularly noticeable in a few large departments, which contain ????% of the city's workforce.

Of important note is that the gender imbalance is particularly striking when comparing jobs with high salaries, as .

On average, men are paid ~$3.00/hour more than women (*P*-value < 0.001). Assuming a 40 hour work week and 50 week work year, men are paid, on average, ~$6000 more per year than women.

# Data analysis summary
All analyses were performed using Python v3.X and R 3.4.3

Data downloaded from City of Seattle repository.

Gender was identified using the Python package `gender_guesser`, which uses first names to categorize genders into the following categories: *andy* (androgynous), *male*, *female*, *mostly_male*, *mostly_female*, and *unknown*. We combined *mostly_male* and *male* into a single *male* category and *mostly_female* and *female* into a single *female* category.

We explored the data for patterns of pay vs sex within jobs, departments, and compensation levels.
The statistical significance of differences in pay between men and women was determined using three methods: T-test, Wilcoxon rank sum test, and linear regression. All tests were highly significant and indicated that men receive more compensation than women (even after corrections for multiple comparisons).
