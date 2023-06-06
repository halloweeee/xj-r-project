install.packages("usethis")
install.packages("tidyverse")
library(tidyverse)
?sum
x <- 2(alt-)
roll2 <- function(bones=1:6){
  dice <- sample(bones,size=2,
                 replace = TRUE)
  sum(dice)
}
roll2
x <- roll2()
x

install.packages("gitcreds")
library(gitcreds)
install.packages("here")
library(here)
install.packages("medicaldata")
library(medicaldata)
install.packages("cowplot")
library(cowplot)

library(readr)
data1 <- read.csv("C:\\Users\xj23u063\Desktop\R Project\basic-statistics-and-projects-in-R-main\data\raw\insurance_with_date.csv", fileEncoding = "UTF-8")
data2 <- read.csv("C:/Users/xj23u063/Desktop/R Project/basic-statistics-and-projects-in-R-main/data/raw/insurance_with_date.csv", fileEncoding = "UTF-8")
data2
str(data2)
read.csv("path/to/insurance_with_date.csv", fileEncoding = "UTF-8")
dat <- read_csv("data/raw/insurance_with_date.csv")
#read.csv,  read.csv2;
sexregion
reformatted <- dat |> 
  mutate(
    across(c(sex, region), factor),
    # sex = factor(sex),
    # region = factor(region),
    gt2_children = children > 2,
    smokes = smoker == "yes",
    date_6m = date + months(6)
    # date_6m = date + 30.4 * 6
  )
str(reformatted)

library(ggplot2)

ebola <- read_csv("data/raw/ebola.csv") %>%
  arrange(Date)
str(ebola)

ebola_2 <- ebola %>% filter(Country %in% c("Sierra Leone", "Guinea", "Liberia"),
                            Date<="2015-03-21")

plot_ebola_point_v0 <- ggplot(data = ebola_2, 
                              mapping = aes(x = Date, y = Cum_conf_cases, col = Country)) + 
  geom_point()
plot_ebola_point_v0 

plot_ebola_point_v1 <- ggplot(data = ebola_2, 
                              mapping = aes(x = Date, y = Cum_conf_cases, col = Country)) + 
  geom_line(mapping = aes(group = Country))
plot_ebola_point_v1 

plot_ebola_point_v2 <- ggplot(data = ebola_2, 
                              mapping = aes(x = Date, y = Cum_conf_cases,fill=Country, col = Country)) + 
  geom_col(position = "stack", alpha = 0.7,
           linetype = "solid", linewidth = 0.7, width = 1)+
  ggtitle(label = "Com_Confirmed cases in 3 countries") +
  xlab(label = "Time") +
  ylab(label = "# of cum_confirmed cases")
plot_ebola_point_v2 



library(unibeCols)

plot_ebola_point_v4 <- ggplot(data = ebola_2, 
                              mapping = aes(x = Date, y = Cum_conf_cases,fill=Country, col = Country)) + 
  geom_point(alpha = 0.7, shape = 21, size = 1.5, stroke = 1.5) +
  scale_fill_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c("#7FFFD4", "#EEC591", unibeIceS()[1]),
                    labels = c("G", "L", "SL")) +
  scale_colour_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                    labels = c("G", "L", "SL")) +
  # scale_colour_manual(name = "Country",
  #                     breaks = c("BE", "VD", "ZH"),
  #                     values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
  #                     labels = c("Bern", "Vaud", "Zurich")) +
  scale_x_date(breaks = as.Date(c("2014-09-01", "2014-10-01", "2014-11-01", 
                                  "2014-12-31","2015-02-01")),
               labels = c("1 Sep", "1 Oct", "1 Nov", "31 Dec", "1 Feb"),
               limits = as.Date(c("2014-8-30", "2015-02-01"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 10000, by = 1000),
                     limits = c(0, 10000)) +
  
  ggtitle(label = "Confirmed cases in 3 countris") +
  xlab(label = "Time") +
  ylab(label = "# of confirmed cases")+
  theme_bw() + theme(legend.position="bottom") +
  theme(panel.spacing = unit(2, "lines")) +
  facet_grid(cols = vars(Country))
plot_ebola_point_v4

install.packages("cowplot")
library(cowplot)

plot_ebola_point_grid <- plot_grid(plotlist = list(plot_ebola_point_v0, plot_ebola_point_v1, plot_ebola_point_v2, 
                                                   plot_ebola_point_v4),
                                   labels = c("V0", "V1", "V2", "V4"), label_size = 12, nrow = 2)

plot_ebola_point_grid

getwd()

