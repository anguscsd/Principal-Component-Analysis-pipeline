# .sexcheck file visualisation 

# load .sexcheck file
sexcheck_raw <- read.table("test.sexcheck", header = TRUE)

# ggplot histogram showing distribution of F values
ggplot(sexcheck_raw, aes(x = F)) +
  geom_histogram(binwidth = 0.025, color = "red", fill = "red") +
  geom_vline(xintercept = 0.2,          
             linetype = "solid",          
             color = "black",             
             size = 0.5) +
  geom_vline(xintercept = 0.8,           
             linetype = "solid",          
             color = "black",             
             size = 0.5) +
  scale_x_continuous(breaks = seq(-0.5, 1, by = 0.1), name = "F-value") +  
  scale_y_continuous(expand = c(0, 0), name = "Number of individuals") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),        # Remove grid lines
    axis.line = element_line(color = "black", size = 0.8),  # Add solid axis lines
    axis.ticks = element_line(color = "black", size = 0.8), # Add axis ticks
    axis.title = element_text(size = 12),  # Adjust axis title size
    axis.text = element_text(size = 10),   
    plot.title = element_text(size = 15, hjust = 0.5)
  ) +
  ggtitle("Heterozygocity Metric")

# PEDSEX=1: 1865 (males)
# PEDSEX=2: 1698 (females)