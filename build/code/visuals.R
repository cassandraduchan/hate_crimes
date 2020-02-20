library(broom)

tidy(hc)
tidy(acs)



##annual stacked bars 
hc$hc_type <- filter(!is.na(hc$hc_type))
g <- ggplot(hc$year)
g + geom_bar(aes(fill = hc$hc_type)) 

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

hc_plot <- ggplot(hc, aes(year))

hc_plot + geom_bar(aes(fill = hc_type)) + scale_fill_manual(values=cbPalette) +
  theme_minimal() +
  xlab("Year") + ylab("Hate Crime Count") + guides(fill=guide_legend(title="Hate Crime Type"))

##percent of people identifying with certain groups
