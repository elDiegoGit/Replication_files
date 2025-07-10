# ------------------------------------------------------------------------
# Note: This script plots the IRFs median response for 25 EM countries 
# with their respective confidence intervals. This was the code used for the
# Figure 2 in the Fed Notes.
# June, 2025.
# ------------------------------------------------------------------------
  

# Required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

# Set the path to Excel file
file_path <- "/Users/dv/Dropbox/Research/3_short_projects/1_sv_tot/Median_IRFs_with_Confidence_Bands.xlsx"

# Read the Excel file
#df <- read_excel(file_path)
raw_df <- read_excel(file_path, col_names = FALSE)

# Remove every 11th row (header rows at 1, 12, 23, 34)
data_df <- raw_df[-seq(1, nrow(raw_df), by = 11), ]

# Rename columns
colnames(data_df) <- c("Period","Median IRF", "16th Percentile", "84th Percentile", "Variable")

# Convert all numeric-looking columns:
data_df <- data_df %>%
  mutate(
    Period = as.numeric(gsub("[^0-9eE+\\.\\-]", "", as.character(Period)))
  ) %>%
  mutate(across(
    c("Median IRF", "16th Percentile", "84th Percentile"),
    ~ as.numeric(gsub("[^0-9eE+\\.\\-]", "", as.character(.))) * 100
  ))


# Reorder variables:
data_df$Variable <- factor(data_df$Variable, levels = c("Trade balance", "Investment", "Consumption", "Output"))


# Plot the results
print(ggplot(data_df, aes(x = `Period`, y = `Median IRF`)) +
  geom_ribbon(aes(ymin = `16th Percentile`, ymax = `84th Percentile`),
              fill = "blue", alpha = 0.2) +
  geom_line(color = "orange", size = 1) +
  facet_wrap(~ Variable, scales = "free_y") +
    scale_x_continuous(breaks = 0:10, labels = 0:10) +  # integer ticks only
  theme_minimal(base_size = 14) +
  labs(y = "% deviation from trend", x=NULL)
  #labs(title = "Median Impulse Response Functions with 90–10% Confidence Bands",
  #     x = "Period", y = "% deviation from trend")
 +
theme(
    #panel.grid.major = element_line(linetype = "dashed", color = "gray85 "),
    panel.grid.minor = element_line(linetype = "dashed", color = "gray85"),
    strip.text = element_text(face = "bold")  # Bold facet titles
  ))

# Save to file and open manually
ggsave("/Users/dv/Dropbox/Research/3_short_projects/1_sv_tot/median_irfs_plot.png", width = 10, height = 6, dpi = 300)

