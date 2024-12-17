# # ============================
# 1. Setup & Package Loading
# ============================

# Function to install and load multiple packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)  
      library(package, character.only = TRUE)  
    }
  }
}

# List of required packages
packages <- c("tidyverse", "dplyr", "readxl", "plotly", "RColorBrewer", "htmlwidgets")
install_and_load(packages)

# Clear environment
rm(list = ls())

# ============================
# 2. User Adjustable Parameters
# ============================

# File paths
file_path <- "data/alcoholspecificdeathssupplementary2020.xlsx"  # Data folder
static_plot_path <- "plots/alcohol_deaths_static_plot.png"       # Plots folder
interactive_plot_path <- "plots/alcohol_deaths_interactive_plot.html"

# Plot settings
plot_title <- "Alcohol-Specific Death Rates by Gender and Deprivation Quintile"
plot_subtitle <- "Most deprived(Q1) and least deprived(Q5) quintiles in England and Wales from 2011 - 2020"
x_axis_label <- "Year"
y_axis_label <- "Age-Standardised Rate per 100,000"
plot_caption <- "Data Source: Office for National Statistics"

# Custom Colors for Plot Lines
custom_colors <- c(
  "Q1 - Female" = "#1f78b4", 
  "Q5 - Female" = "#e66101", 
  "Q1 - Male"   = "#4daf4a", 
  "Q5 - Male"   = "#984ea3"  
)

# Data Import Parameters
excel_sheet <- 6
skip_rows <- 3
row_slice_start <- 2
row_slice_end <- 51

# X-axis Display Range
years_to_display <- as.character(2011:2020)

# ============================
# 3. Import Data 
# ============================

if (!file.exists(file_path)) {
  stop(paste("File not found at path:", file_path))
}

df <- read_excel(file_path, sheet = excel_sheet, skip = skip_rows)

# ============================
# 4. Wrangle Data 
# ============================

df_cleaned <- df %>% 
  select(-3, -7) %>%
  slice(row_slice_start:row_slice_end)

colnames(df_cleaned) <- c("Year", "IMD_quintile", "Females", "Female_LCI", "Female_UCI", "Males", "Male_LCI", "Male_UCI")

df_long <- df_cleaned %>% 
  pivot_longer(cols = c("Females", "Males"), 
               names_to = "Gender", 
               values_to = "Rate")

cols_to_convert <- c("Female_LCI", "Female_UCI", "Male_LCI", "Male_UCI", "Rate")
df_long[cols_to_convert] <- lapply(df_long[cols_to_convert], as.numeric)

df_long$Year <- factor(df_long$Year)
df_long <- df_long %>% filter(!is.na(Year))

df_long_filtered <- df_long %>% 
  filter(IMD_quintile %in% c(1, 5))

df_long_filtered <- df_long_filtered %>% 
  mutate(Label = case_when(
    IMD_quintile == 1 & Gender == "Females" ~ "Q1 - Female",
    IMD_quintile == 5 & Gender == "Females" ~ "Q5 - Female",
    IMD_quintile == 1 & Gender == "Males" ~ "Q1 - Male",
    IMD_quintile == 5 & Gender == "Males" ~ "Q5 - Male",
    TRUE ~ NA_character_
  ))

# Sanity Check: Check for missing values in critical columns
if (any(is.na(df_long_filtered$Rate))) {
  warning("Warning: Missing values detected in 'Rate'. Check your data.")
}

# Sanity Check: Check for non-negative 'Rate'
if (any(df_long_filtered$Rate < 0, na.rm = TRUE)) {
  warning("Warning: Negative values detected in 'Rate'. Verify the data is correct.")
}

# ============================
# 5. Create initial ggplot 
# ============================

p <- ggplot(df_long_filtered, aes(x = Year, 
                                  y = Rate, 
                                  color = Label,  
                                  group = Label, 
                                  text = paste("Year:", Year, 
                                               "<br>Rate:", Rate, 
                                               "<br>Label:", Label))) +
  geom_line(linewidth = 1.2) +  
  geom_point(size = 2) +  
  labs(
    title = plot_title,  
    subtitle = plot_subtitle,  
    x = x_axis_label,
    y = y_axis_label,
    color = "Quintile and Gender",
    caption = plot_caption 
  ) +
  theme_minimal() +
  scale_color_manual(values = custom_colors) +  
  scale_x_discrete(limits = years_to_display)  

# Save initial plot
ggsave(static_plot_path, plot = p, width = 10, height = 6)

# ============================
# 6. Convert to Interactive Plotly 
# ============================

interactive_plot <- ggplotly(p, tooltip = "text")

# Add title, subtitle, and caption as plotly annotations
interactive_plot <- interactive_plot %>%
  layout(
    title = list(
      text = paste0(
        plot_title,
        '<br>',
        '<span style="font-size: 12px;">', plot_subtitle, '</span>'
      )
    ),
    annotations = list(
      list(
        x = 1.3,  
        y = -0.16, 
        text = plot_caption,  
        showarrow = FALSE,
        xref = 'paper',  
        yref = 'paper',  
        xanchor = 'right',  
        yanchor = 'auto',
        font = list(size = 10, color = 'black')
      )
    ),
    margin = list(b = 60, r = 80), 
    legend = list(
      y = 0.8  
    )
  )

# Save interactive plot as HTML
htmlwidgets::saveWidget(as_widget(interactive_plot), interactive_plot_path)

# View interactive plot
interactive_plot