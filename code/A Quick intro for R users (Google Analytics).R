knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  cache = TRUE
)

library(tidyverse)
library(palmerpenguins)
library(lubridate)
library(readxl)
library(here)
library(skimr)
library(janitor)
#library(directlabels)

data("penguins")
head(penguins, 5)
tail(penguins, 12)
attach(penguins)

# output a specific rows(s)
penguins[1:5, ]

# a specified no of rows
penguins[c(2, 5), ]

# output the entire row for a specific column(s)
penguins[1:3]

# specific columns by name for a specific range
# Note the (levels)
# unique values of the selected column
penguins$island[2:6]

# a specified list of rows
penguins$island[c(2, 8)]

# Bar plots can be used to show the count of each categorical value in one variable only. 
# To use the bar plot for two variables - use the column chart. 
# However it's better to use scatter plots and heat maps.

ggplot(drop_na(penguins)) +
  geom_col(mapping = aes(flipper_length_mm, body_mass_g, fill = sex))

# check geom_dl
ggplot(
  drop_na(penguins),
  aes(flipper_length_mm, body_mass_g, color = sex, alpha = 0.25)
)+
  geom_smooth(se = FALSE) +
  geom_point(show.legend = FALSE) +
  geom_point(aes(label = sex, fontface = "italic"), method = "smart.grid") +
  labs(
    title = "Flipper length vs Body mass",
    subtitle = "The relation between body mass and the flipper length",
    caption = "Data collected by Dr. Kristen Gorman",
    x = "Flipper length(mm)",
    y = "Body mass(g)"
  ) +
  annotate(
    geom = "segment",
    x = c(204, 200),
    xend = c(204, 200),
    y = c(3200, 4800),
    yend = c(4000, 4150),
    color = c("lightcoral", "lightblue"),
    size = c(0.45, 0.45),
    angle = c(90, 90),
    arrow = arrow()
  )

# visualize the relationship between the flipper_length and
# body_mass in addition to the frequency of value
count_table = drop_na(penguins) %>% 
  count(flipper_length_mm, body_mass_g)
print(count_table)

ggplot(
  drop_na(penguins),
  aes(flipper_length_mm, body_mass_g, shape = sex)
)+
  geom_count()


drop_na(penguins) %>% 
  count(island, species) %>% 
  ggplot()+
  geom_tile(mapping = aes(island, species, fill = n))

# bar plot that shows the number of each species per island
# Note that the default position is the stack one
# multiple plots using facet_grid

ggplot(drop_na(penguins), aes(island, fill = species)) +
  geom_bar(show.legend = FALSE, width = 0.3, position = "stack") +
  scale_fill_manual(values = c("#00AFBB", "#E7B300", "#FC4E07")) +
  facet_wrap(sex ~ ., ncol = 1, strip.position = "top") +
  scale_y_continuous(limits = c(0, 90), n.breaks = 10) +
  theme(
    axis.text = element_text(size = 15, angle = 45),
    axis.text.x = element_text(vjust = 0.65),
    axis.title = element_text(size = 25, color = "steelblue"),
    strip.text = element_text(size = 20, color = "steelblue"),
    strip.background = element_blank()
  ) +
  stat_count(
    geom = "text",
    colour = "black",
    size = 5,
    aes(label = species),
    position = position_stack(vjust = 0.5)
  ) +
  stat_count(
    geom = "text",
    color = "black",
    size = 4,
    position = position_stack(vjust = 0.28),
    aes(label = ..count..)
  )

# Histograms for quantitative variables
ggplot(penguins, aes(bill_length_mm, fill = species)) +
  geom_histogram(show.legend = FALSE, col = 'grey') +
  scale_fill_manual(values = c("ForestGreen", "#E7B300", "#FC4E07"))+
  facet_wrap(species ~., strip.position = 'top', ncol = 1) +
  theme(
    axis.title = element_text(size = 20),
    strip.text = element_text(size = 20),
    axis.text = element_text(size = 15)
  ) +
  scale_y_continuous(limits = c(0, 25))
ggsave("penguins_2.png")

# 3 alternative to histogram
# density plot
ggplot(penguins, aes(bill_length_mm, fill = species)) +
  geom_density(show.legend = FALSE) +
  scale_fill_manual(values = c("ForestGreen", "#E7B300", "#FC4E07")) +
  facet_wrap(species ~., ncol = 1, strip.position = 'top') +
  scale_x_continuous(limits = c(20, 60), n.breaks = 5) +
  theme(
    axis.text = element_text(size = 15, angle = 45),
    axis.title = element_text(size = 20),
    strip.text = element_text(size = 20)
  )

# Frequency plot
ggplot(penguins, aes(bill_length_mm, color = species)) +
  geom_freqpoly(show.legend = FALSE) +
  scale_color_manual(values = c("ForestGreen", "#E7B300", "#FC4E07")) +
  facet_grid(species ~ .) +
  scale_x_continuous(limits = c(20, 60), n.breaks = 5) +
  scale_y_continuous(limits = c(0, 30), n.breaks = 6) +
  theme(
    axis.text = element_text(size = 15, angle = 45),
    axis.title = element_text(size = 20),
    strip.text = element_text(size = 20)
  )

# Dot plot
ggplot(penguins, aes(bill_length_mm, fill = species)) +
  geom_dotplot(show.legend = FALSE) +
  scale_fill_manual(values = c("ForestGreen", "#E7B300", "#FC4E07")) +
  facet_wrap(species ~ ., ncol = 1, strip.position = 'top') +
  scale_x_continuous(limits = c(20, 60), n.breaks = 5) +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 15),
    strip.text = element_text(size = 20)
  )

# Box plots
# a) Box plots are usually used to plot qualitative vs. quantitative variables
# b) Any value > 1.5*IQR will be plotted as an outlier

ggplot(drop_na(penguins), aes(species, body_mass_g, fill = species)) +
  geom_boxplot(
    show.legend = FALSE,
    outlier.shape = 21, # a number 0:25, NA will hide the outliers
    outlier.size = 3,
    outlier.fill = 'red'
  ) +
  facet_wrap(sex ~ ., scales = 'free_x', ncol = 1, strip.position = 'top') +
  scale_y_continuous(limits = c(2500, 6500), n.breaks = 10) +
  theme(
    axis.text = element_text(angle = 45, size = 15),
    axis.text.x = element_text(vjust = 0.65),
    axis.title = element_text(size = 20, color = 'steelblue'),
    strip.text = element_text(size = 20, color = 'steelblue')
  ) +
  ylim(3000, 6000) # zoom in a specified portion of the plot(with clipping)

# box plots for cont. variables
# use cut_interval/cut_number/cut_width
# cut functions basically group the values into a specified number of categories
# highlight = FALSE -> source code not to appear in final rendered file

penguins %>%
  ggplot(
    aes(
      flipper_length_mm,
      body_mass_g,
      group = cut_interval(flipper_length_mm, n = 4),
      fill = cut_interval(flipper_length_mm, n = 4)
    )
  ) +
  geom_boxplot(show.legend = FALSE) +
  scale_y_continuous(limits = c(2500, 6500), n.breaks = 20)

# violin plot
# violin shape -> gets thicker/thinner -> frequency of each value
ggplot(drop_na(penguins), aes(species, body_mass_g, fill = species)) +
  geom_violin(
    show.legend = FALSE,
    outlier.shape = 21,
    outlier.size = 3,
    outlier.fill = 'red',
    draw_quantiles = c(0.25, 0.5, 0.75)
  ) +
  facet_wrap(sex ~ ., scales = 'free_x', ncol = 1) +
  scale_y_continuous(limits = c(2000, 7000), n.breaks = 10) +
  theme(
    axis.text = element_text(angle = 45, size = 15),
    axis.text.x = element_text(vjust = 0.65),
    axis.title = element_text(size = 20, color = 'steelblue'),
    strip.text = element_text(size = 20, color = 'steelblue')
  ) +
  coord_cartesian(ylim = c(2500, 6500))

# atomic vectors, one can't input more than one data type
# (character, double, integer, logical) in the same vector

vec = c("ok", "this", "is_a_trial")
typeof(vec)

# assign names for vector elements(optional)
names(vec) = c("a", "b", "c")
print(vec)

vec["c"]
vec[2]
vec[c(2, 3)]
length(vec)

# Lists, one may include more than one data type
new_list = list(1L, 5.2, "OK", TRUE, NULL)
typeof(new_list)

names(new_list) = c("a", "b", "c", "d", "e")
str(new_list)

# alternatively 
another_list = list("a" = 1L, "b" = 5.3, "c" = "OK", "d" = FALSE)
names(another_list)

# compare two floating numbers using a specified tolerance
# more useful than just using (==)
dplyr::near(4.56161, 4.56, tol = 0.01)

# date in Calgary, Canada
today(tzone = 'MST')

# time diff in hours between Cairo & Calgary
calgary = now(tzone = "MST")
cairo = now(tzone = "EET")
cat((hour(cairo) - hour(calgary)), "hrs")

# Some examples to convert strings into datetime format. 
# Note how we should use the right function that matches the string order
# All datetimes will be shown in the default format 
# (yyyy-mm-dd hh:mm:ss UTC)

dmy("6th of October 2021")
dmy_hms("13 of January 1993 10:59:12 PM")


# extract specific part of the datetime
minute(dmy_hms("13 of January 1993 10:59:12 PM"))
second(dmy_hms("13 of January 1993 10:59:12 PM"))
year(dmy_hms("13 of January 1993 10:59:12 PM"))
week(calgary)

# simple df
new_df = data.frame(
  name = c("mohamed", "aly", "farid"),
  salary = c(15000, 12000, 16500)
)
new_df

new_tibble = tribble(
  ~name, ~salary,
  "mohamed", 15000,
  "aly", 12000,
  "farid", 16500
)
new_tibble


# seperate a column
x_df = tribble(
  ~name, ~age, ~email,
  "aly_kamal", 25, "aly_kamal@gmail.com"
)

x_df %>%
  separate(name, c("first", "last"), sep = "_")

# all available built in datasets
data()


### load a built in dataset, specify its name and package
who_data = read_builtin("who", "tidyr")
str(who_data)
glimpse(who_data)

# show only the number and names of the datasets columns
print(colnames(who_data))

# rename a column(s)
# either temporarily
rename(who_data, sp_m1524 = new_sp_m1524)

# permanently
who_data = rename(who_data, sp_m1524 = new_sp_m1524, iso = iso2)

# summary statistics
who_data %>%
  summarise(
    latest_year = max(year, na.rm = TRUE),
    average_sp = mean(c(new_sp_f5564, new_sp_f65, new_sn_m014), na.rm = TRUE)
  )

filter(who_data, country == c("Afghanistan", 'Albana') & year > 1990)

# & and | should never be used inside of an if clause because they can return 
# vectors. Always use && and || instead

if(who_data[100,]['country'] %in%
  c('China', 'Afghanistan', 'Saudi')) {
  print("Asian Country")
}else if (who_data[100,]['country'] == 'Algeria'){
  print("African Country")
}else {
  print("European Country")
}

# unique values
print(unique(who_data['country']))

if(who_data['country'] %in% c("Egypt", 'Algeria', 'Senegal')){
  print("ok")
}else{
  print("not exist")
}


# load a dataset either from the web or by uploading it
# each file may contain only one sheet and be in csv format
world_happiness = read.csv(
  "World_Happiness_data.csv",
  show_col_types = FALSE
)
world_happiness

# view the list of sheets for an excel file
excel_sheets("CO2 Dataset.xlsx")

# paste the file path and sheet name
co2_df = read_excel(
  "E:/Google_Course_Data_Analysis_with_R/datasets/CO2 Dataset.xlsx",
  sheet = "CO2 Data Cleaned"
)

# summarized statistics
skim_without_charts(co2_df)
summary(co2_df)

# names lower case and only letters
# numbers and underscores
co2_df = clean_names(co2_df)

co2_df %>%
  group_by(country_name) %>%
  summarize(average_emissions = 
              mean(co2_per_capita_metric_tons, na.rm = TRUE))

if(co2_df$`CO2 (kt)`= 4){
  print(unique(co2_df$country_name))
}

# sum of multiple columns
co2_df %>%
  mutate(
    'total' = rowSums(
      co2_df[, c('co2_per_capita_metric_tons', 'co2_kt')],
      na.rm = TRUE
    )
  )
head(co2_df)

# sum of each column separately
colSums(co2_df[, c('co2_per_capita_metric_tons', 'co2_kt')],
        na.rm = TRUE)

# merge multiple column into a single one
# set remove to FALSE to keep original columns
co2_df = co2_df %>%
  unite(
    combined,
    c("country_name", "co2_per_capita_metric_tons", "year"),
    sep = "/"
  )
tail(co2_df)

# reverse process
co2_df = co2_df %>%
  separate(
    combined,
    c("country_name", "co2_per_capita_metric_tons", "year"),
    sep = "/"
  )
print(co2_df)

# Pivoting
beachbugs_wide = read_csv('E:/Google_Course_Data_Analysis_with_R/datasets/beachbugs_wide.csv')
head(beachbugs_wide)

bb_long = beachbugs_wide %>%
  pivot_longer(
    cols = colnames(beachbugs_wide)[2:12],
    names_to = 'beach',
    values_to = 'bugs'
  )
head(bb_long)

beachbugs_long = read_csv("E:/Google_Course_Data_Analysis_with_R/datasets/beachbugs_long.csv")
head(beachbugs_long)

bb_wide = beachbugs_long %>%
  pivot_wider(
    names_from = 'site',
    values_from = 'buglevels'
  )
head(bb_wide)

# create new directories and new files as follows
# successfully create = TRUE output
dir.create("trial_dir")
dir.create("new_dir")
file.create("new_dir/new_file.csv")

# create matrices using the matrix() function provided the elements and
# the number of rows or the number of cols
matrix_a = matrix(c(3:14), nrow = 4)
matrix_b = matrix(c(20:31), nrow = 3)


# matrices multiplication
# # of columns of 1st one must == # of rows of 2nd
matrix_c = matrix_b %*% matrix_a
matrix_c

# If we wanna choose a variable name that's a reserved word (TRUE, in, etc.) or
# starts with an (_), we must use backticks (` `) to avoid getting errors.

`TRUE` = 18
`_x01` = 21
`TRUE`
`_x01`

# (<<-) is an assigment operator that's mainly used in functions. 
# For instance if we wanna change change the global value of a variable inside a
# function as follows.
x <- 1
y <- 3

fun_3 <- function(){
  x <- 2
  y <<- 5
  print(paste(x, y))
}
fun_3()
print(y)
print(x)

# To access functions of a package use the (::) operators
# Enter the package name then (::) and press tab to view the list of functions
lubridate::.__C__Interval


# explore the pipes tool
tg_df = read_builtin("ToothGrowth", "datasets")
print(tg_df)
str(tg_df)
glimpse(tg_df)

# effectiveness of each supplement on teeth
supp_effect <- tg_df %>%
  group_by(supp) %>%
  summarise(
    average_length = mean(len, na.rm = TRUE),
    average_dose = mean(dose, na.rm = TRUE),
  )
supp_effect

# a df from ggplot2
diamond_df = read_builtin("diamonds", "ggplot2")

# new column(s)

diamond_df = add_column(
  diamond_df,
  category = "low_price",
  .before = "cut"
)

diamond_df$category[diamond_df$price > 1000 &
  diamond_df$price < 10000] = 'Meduim Price'
diamond_df$category[diamond_df$price > 10000] = "High Price"

diamond_df %>%
  group_by(category)%>%
  summarise(
    average_price = mean(price),
    average_carat = mean(carat)
  )
# selected columns from original df
cut_to_price = select(diamond_df, colnames(diamond_df)[3:8])
print(cut_to_price)

# exclude specific columns
select(diamond_df, -colnames(diamond_df)[c(3, 8)])

# another example
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = color, fill = cut)) +
  facet_wrap(. ~cut)

# readr package
readr_example()

# load a dataset
read_csv(readr_example('mtcars.csv'), show_col_types = FALSE)

# a new file in directory and put/write data in
file.create("massey_rating")
mr = read_file(readr_example('massey-rating.txt'))
write_file(mr, file = "massey_rating")











































