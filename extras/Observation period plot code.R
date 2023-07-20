# CODE TO CREATE A FIGURE OF THE OBSERVATION PERIOD

library(readxl)
Lockdown_Periods_Dates <- read_excel("extras/Lockdown Periods Dates.xlsx")
View(Lockdown_Periods_Dates)

library(ggplot2)
library(dplyr)
library(here)

lockdown <- data.frame(periods = Lockdown_Periods_Dates$Periods, start = as.Date(Lockdown_Periods_Dates$Start_date), end = as.Date(Lockdown_Periods_Dates$End_date))
lockdown$lockdown <- 1

lockdown <- lockdown  %>%  mutate(periods = factor(periods, levels=c("Pre-Lockdown (Jan 2017 - Feb 2020)", "Lockdown (March 2020 - June 2020)", "Post-First Lockdown (July 2020 - Oct 2020)",
                                                                     "Second Lockdown (Nov 2020 - Dec 2020)", "Third Lockdown (Jan 2021 - March 2021)", "Easing of Restrictions (April 2021 - June 2021)", 
                                                                     "Legal Restrictions Removed (July 2021 - Dec 2021)") ))

lockdown <- lockdown %>% rename("Lockdown Periods" = periods) 

start_date <- as.Date("2017-01-01") 

Observation_period_plot <- ggplot(lockdown, aes(x = start, y = lockdown, fill = `Lockdown Periods`)) +
  geom_rect(aes(xmin = start, xmax = end, ymin = 0, ymax = 1), alpha = 1) +
  scale_fill_discrete() +
  theme_minimal() +
  labs(x = "Date", y = "Lockdown Period") +
 # scale_y_continuous(limits = c(0,1)) +
 # coord_cartesian(ylim=c(0.25,0.5)) +
  scale_x_date(date_labels="%b %Y",date_breaks  ="2 month", limits = as.Date(c("2017-01-01", "2021-12-01")), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=12),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        axis.title.y=element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

Observation_period_plot

ggsave(here("extras","Observation_period_plot.jpg"), Observation_period_plot, dpi=600, scale = 1,  width = 16, height = 8)

