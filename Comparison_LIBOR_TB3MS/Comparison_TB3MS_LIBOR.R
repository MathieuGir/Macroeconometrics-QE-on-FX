# COMPARING LIBOR AND TREASURY BILLS RATE

# we had some hesitation about the choice of variable as it was difficult to access LIBOR
# let's compare both just for fun, as theory would say theyr behavior is quite close

TRESOR <- read.csv("~/work/TB3MS.csv")
head(TRESOR)
LIBOR <- read.csv("~/work/Libor3M.csv", sep = ";") # slight issue of separator but it is now fixed
head(LIBOR) 

# packages for the plot
library(ggplot2)
library(dplyr)

# date format
LIBOR$Date <- as.Date(LIBOR$Date, format = "%d/%m/%Y")
TRESOR$observation_date <- as.Date(TRESOR$observation_date)

# plot

ggplot() +
  # PX_LAST
  geom_line(data = LIBOR, aes(x = Date, y = as.numeric(gsub(",", ".", PX_LAST)), color = "PX_LAST"), size = 1) +
  # TB3MS
  geom_line(data = TRESOR, aes(x = observation_date, y = TB3MS, color = "TB3MS"), size = 1) +
  # aesthetic
  labs(title = "PX_LAST and TB3MS over Time",
       x = "Date", 
       y = "Value") +
  # second y axis
  scale_color_manual(values = c("PX_LAST" = "blue", "TB3MS" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_y_continuous(name = "PX_LAST", sec.axis = sec_axis(~., name = "TB3MS")) +
  # broad x axis, and simple mention of the year (reading purposes)
  scale_x_date(
    limits = c(min(LIBOR$Date) - 365, max(LIBOR$Date) + 365), 
    date_labels = "%Y", 
    date_breaks = "1 year"
  ) +
  # broader x axis (aesthetic purposes)
  theme(
    plot.margin = margin(0.5, 1, 0.5, 1, "cm"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



