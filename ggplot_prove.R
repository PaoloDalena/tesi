# Prove ggplot

db <- data.frame(media_frasi, media_parole, numero_frasi, numero_parole, names.arg = 2008:2017)
ggplot(data = db) +
  geom_col(aes(x = as.factor(names.arg), y = numero_parole, fill = numero_frasi))

ggplot(data = db) +
  geom_col(aes(x = as.factor(names.arg), y = media_parole, fill = media_frasi)) +
  coord_cartesian(ylim = c(0, 6))



# Example -----------------------------------------------------------------

p <- ggplot(obs, aes(x = Timestamp))
p <- p + geom_line(aes(y = air_temp, colour = "Temperature"))

# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = rel_hum/5, colour = "Humidity"))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Relative humidity [%]"))

# modifying colours and theme options
p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Air temperature [°C]",
              x = "Date and time",
              colour = "Parameter")
p <- p + theme(legend.position = c(0.8, 0.9))
p

