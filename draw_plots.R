### SETUP ----------------------------------------------------------------------

library(jpeg)
library(ggplot2)
library(tidyr)
library(forcats)

# set working directory
wd <- '~/Downloads'
setwd(wd)

# load data frame
df <- read.csv('city_of_seattle_with_sex.csv')

# load dollar bill image
dollar <- readJPEG("dollar.jpg") 

# assign colors for plots
#             women         men          unknown
my_cols <- c('lightgreen', 'darkgreen', 'gray65')


### DATA-WRANGLING FUNCTIONS ---------------------------------------------------

sort_factors <- function(df, fac, x) {
  
  # A wrapper function to use `forcats::fct_reorder` in dplyr data wrangling
  # pipelines.
  df[, fac] <- forcats::fct_reorder(df[, fac][[1]], df[, x][[1]])
  
  return(df)
}


median_wage_differences <- function(df) {
  
  # Given the gender wage dataframe `df`, return the absolute and percent
  # differences in median hourly wages between women and men who have the same
  # job title.
  
  df %>%
    dplyr::group_by(depart, title, sex) %>%
    dplyr::summarize(mhr = median(h_rate)) %>%
    tidyr::spread(sex, 'mhr') %>%
    dplyr::select(-unknown) %>%
    na.omit() %>%
    dplyr::mutate(abs_diff = female - male,
                  pct_diff = 1 - (female / male))
}


relative_wages_by_department <- function(df) {
  
  # Given the gender wage dataframe `df`, return the median hourly wages for
  # women relative to men in the same department. Sort departments (as factors)
  # by women's relative wages, descending.
  
  df %>%
    dplyr::group_by(depart, sex) %>%
    dplyr::summarize(mhr = median(h_rate)) %>%
    tidyr::spread(sex, 'mhr') %>%
    dplyr::select(-unknown) %>%
    na.omit() %>%
    dplyr::mutate(frac = female / male) %>%
    sort_factors('depart', 'frac')
}


count_genders <- function(df) {
  
  # Given the gender wage dataframe `df`, return counts of the number of women, 
  # men, & people whose genders could not be inferred by their first names.
  
  df %>%
    dplyr::group_by(depart) %>%
    dplyr::count(sex) %>%
    tidyr::spread(sex, 'n') %>%
    tidyr::replace_na(list(female = 0, male = 0, unknown = 0)) %>%
    dplyr::mutate(total = unknown + female + male) %>%
    sort_factors('depart', 'total')
}


wage_bracket_gender_ratios <- function(df, bin_width = 10) {
  
  # Given the gender wage dataframe `df` and a bin_width that defines the size
  # of a wage bracket (defau;lt $10), return a dataframe that describes the
  # number of men and women in each wage bracket, the fraction of women in each
  # wage bracket, the number of men and women in the bracket, and the upper
  # bound of each bracket.
  
  # count the number of men and women in each wage bracket
  s <- seq(0, 180, by = bin_width)
  w <- hist(df[df$sex == 'female', ]$h_rate, breaks = s, plot = FALSE)$counts
  m <- hist(df[df$sex ==   'male', ]$h_rate, breaks = s, plot = FALSE)$counts
  
  # set 0 counts to NA
  w[which(w == 0)] <- NA
  m[which(m == 0)] <- NA
  
  # build a dataframe
  df_new <- data.frame(n = w+m, 
                       frac = w/(m + w), 
                       br = s[-1], 
                       w = w, 
                       m = m)
  
  # clean up dataframce
  df_new$frac[which(is.infinite(df_new$frac))] <- NA
  df_new$n[which(df_new$n == 0)] <- NA
  
  return(df_new)
}


### PLOTTING FUNCTIONS ---------------------------------------------------------

plot_wage_differences_hist <- function(df, nbins, colours) {
  
  # Plot a histogram of wage differences between women and men
  
  ggplot() +
    theme_classic() + 
    geom_histogram(
      data = df, 
      aes(x = pct_diff), 
      bins = nbins, 
      fill = colours[3]) +
    geom_histogram(
      data = df[df$pct_diff < 0, ], 
      aes(x = pct_diff), 
      bins = nbins, 
      fill = colours[2]) +
    geom_histogram(
      data = df[df$pct_diff > 0, ], 
      aes(x = pct_diff), 
      bins = nbins, 
      fill = colours[1]) +
    geom_histogram(
      data = df[df$pct_diff == 0, ], 
      aes(x = pct_diff), 
      bins = nbins, 
      fill = colours[3]) +
    scale_x_continuous(
      limits = c(-0.3, 0.3),
      breaks = c(-0.3, -0.2, -0.1, 0.0, 0.1, 0.2, 0.3),
      labels = c('30% less', '20% less', '10% less', 
                 'Equal pay', 
                 '10% more', '20% more', '30% more')) +
    scale_y_continuous(expand = c(0,0)) +
    ggtitle('Compared to men with the same job title, women receive...') +
    xlab('Women\'s median income as a fraction of men\'s median income' ) +
    ylab('Number of jobs with this income difference')
}


plot_relative_wages_dollar <- function(df, img, colours) {
  
  # Plot women's wages relative to men's, by department, using an image of a
  # dollar bill.
  
  ggplot() + 
    theme_minimal() +
    theme(
      plot.margin = margin(20, 30, 10, 10),
      axis.line.x = element_line(colour = "gray75", 
                                 size = 0.5, 
                                 linetype = "solid"),
      axis.ticks.x = element_line(colour = "gray75", 
                                  size = 0.5, 
                                  linetype = "solid"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()) +
    annotation_raster(img, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=0) +
    geom_bar(
      data = df, 
      aes(x = depart, y = 1 - frac),
      stat='identity', 
      width = 1,
      fill = 'black',
      alpha = 0) +
    geom_bar(
      data = df[df$frac > 1, ], 
      aes(x = depart, y = 1-frac),
      stat='identity', 
      width = 1,
      fill = colours[1],
      alpha = 1) +
    geom_bar(
      data = df[df$frac <= 1, ], 
      aes(x = depart, y = 1-frac),
      stat='identity', 
      width = 1,
      fill = 'white',
      alpha = 1) +
    geom_vline(
      xintercept = (1:nrow(df)) + 0.5, 
      colour = 'white', 
      size=0.25) +
    geom_hline(
      yintercept = 0, 
      colour = 'gray65', 
      linetype = 'dashed') +
    scale_y_reverse(
      breaks = c(-0.2, 0, 0.25, 0.5, 0.75, 1),
      labels = c('$1.20', '$1.00', '$0.75','$0.50','$0.25','$0.00')) +
    coord_flip(
      ylim = c(-0.21, 1), 
      expand = c(0,0)) +
    xlab('') + 
    ylab('') + 
    ggtitle('For each dollar a man makes, a woman in the same department makes...')
}


plot_gender_counts <- function(df, min_size, colours) {
  
  # Plot the number of women and men in each department.
  
  # distribute unknown individuals equally among the counts for men and women
  df$unknown <- df$unknown / 2
  df$female  <- df$unknown + df$female
  df$male    <- df$unknown + df$male
  
  # only plot departments that exceed the specified minimum size
  ggplot(data = df[df$total >= min_size, ], aes(x = depart)) +
    theme_classic() +
    geom_hline(
      yintercept = 0, 
      colour = colours[3]) +
    geom_hline(
      yintercept = c(-500, 500, 1000), 
      colour = colours[3], 
      linetype = 'dashed') +
    geom_bar(
      aes(y = -female),  
      stat = "identity", 
      position = "identity", 
      width = 0.85, 
      fill = colours[1]) +
    geom_bar(
      aes(y = male), 
      stat = "identity", 
      position = "identity", 
      width = 0.85, 
      fill = colours[2]) +
    geom_bar(
      aes(y = unknown), 
      stat = "identity", 
      position = "identity", 
      width = 0.85, 
      fill = colours[3]) +
    geom_bar(
      aes(y = -unknown), 
      stat = "identity", 
      position = "identity", 
      width = 0.85, 
      fill = colours[3]) +
    scale_y_continuous(breaks = c(-500, 0, 500, 1000),
                       labels = c(500, 0, 500, 1000)) +
    coord_flip() +
    xlab('') +
    ylab('  <- Number of Women | Number of Men ->') +
    ggtitle('Number of employees in each department, by inferred gender')
}


plot_wage_bracket_gender_ratios <- function(df) {
  
  # A plot to show that fewer women have high hourly wages
  
  bin_size <- diff(df$br)[1]
  
  ggplot(data = df, aes(x = br - (bin_size / 2), y = frac)) +
    theme_minimal() +
    theme(panel.grid.minor=element_blank()) +
    geom_point(aes(size = n), col = 'black') +
    scale_y_continuous(
      breaks = c(-0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), 
      labels = c('', '0%', '10%', '20%', '30%', '40%', '50%', '60%'),
      limits = c(-0.01, 0.65),
      expand = c(0,0)) +
    scale_x_continuous(
      limits = c(0, 120), 
      breaks = seq(0, 120, by = 10)) +
    scale_size_area(
      'Number of \nEmployees', 
      breaks = c(1, 10, 100, 1000, 3000)) +
    xlab('Income bracket (in dollars per hour)') +
    ylab('Percentage of women in each income bracket') +
    ggtitle('Fewer women have high hourly wages')
}


### DATA WRANGLING -------------------------------------------------------------

# make a single 'unknown' gender label
df[df$sex == 'andy', 'sex'] <- 'unknown'


### DRAW PLOTS -----------------------------------------------------------------

# plot a histogram of wage differences
df %>%
  median_wage_differences() %>%
  plot_wage_differences_hist(30, my_cols)

# plot women's wages, relative to men's, by department
df %>%
  relative_wages_by_department() %>%
  plot_relative_wages_dollar(dollar, my_cols)

# plot number of women and men in each department
df %>%
  count_genders() %>%
  plot_gender_counts(20, my_cols)

# plot to show that fewer women have high hourly wages
df %>%
  wage_bracket_gender_ratios(10) %>%
  plot_wage_bracket_gender_ratios()
