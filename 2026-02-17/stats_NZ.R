library(tidyverse)


# 1 - Import Data ---------------------------------------------------------
dataset <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-02-17/dataset.csv')
dataset

# measure: Agricultural production category
# value: Number of units produced
# value_label: Unit of production


# 2 - Tidy Data -----------------------------------------------------------
# Create factors for measure and value_label variables
dataset$measureF <- as.factor(dataset$measure)
dataset$value_labelF <- as.factor(dataset$value_label)

str(dataset)
summary(dataset)


# The gap between people and sheep in New Zealand is rapidly closing. 
# There's now about 4.5 sheep to every person in New Zealand compared to a peak of 
# 22 sheep per person in the 1980s, that's according to figures released by Stats NZ 
# this week.



# 3 - First Question ------------------------------------------------------
# Is sheep production unique in its decline? Do other types of meat production show the same pattern?

# Get levels of unit of production
levels(dataset$value_labelF)
# Get Meat production data
meatLevels <- grep(pattern = 'alpacas|cattle|chickens|deer|goats|horses|livestock|llamas|pigs|poultry|sheep', 
                   x = levels(dataset$value_labelF), value = T)
# Get Production category levels
levels(dataset$measureF)

# Filter data
meat_dataset <- dataset %>% 
  filter(value_labelF %in% meatLevels) %>% 
  # Keep only total values
  filter(measureF %in% c('Alpacas and Llamas', 'Horses', 
                         # 'Total Cattle',
                         # We want Cattle production to be separated
                         'Total Beef Cattle', 'Total Dairy Cattle (including Bobby Calves)',
                         'Total Deer', 'Total goats', 'Total Pigs', 'Total poultry', 'Total Sheep'))
  
  
# Drop unused levels
meat_dataset <- droplevels(meat_dataset)
meat_dataset



## Plot ####
png('tidytuesday/2026-02-17/figure1.png',
    width = 300,
    height = 180,
    units = 'mm',
    res = 300)
meat_dataset %>% 
  group_by(measureF) %>% 
  # Normalize production to see general trends rather than absolute values 
  mutate(svalue = scale(value)) %>% 
  ggplot()+
  geom_smooth(aes(x = year_ended_june, y = svalue, color = measureF),
              se = F)+
  facet_wrap(measureF~., scales = 'free')+
  labs(x = 'Year',
       y = 'Normalized Production')+
  theme_bw()+
  theme(legend.position = 'none')
dev.off()

# 4 - Second Question -----------------------------------------------------
# Which agricultural industries have shown the most production growth?
library(ggthemes)


# Compute CAGR: Compound Growth Annual Rate
top_growth <- dataset %>%
  group_by(measureF, value_label) %>%
  # Keep only productions that are still operating
  filter(any(year_ended_june == 2024)) %>% 
  arrange(year_ended_june, .by_group = TRUE) %>%
  summarise(
    start_year  = first(year_ended_june),
    end_year    = last(year_ended_june),
    start_value = first(value),
    end_value   = last(value),
    n_years = end_year - start_year,
    # Compute CAGR
    CAGR = (end_value / start_value)^(1 / n_years) - 1,
    .groups = "drop"
  ) %>%
  # Filter out redundant deer data
  filter(! measureF %in% c('Total male deer', 'Total female deer')) %>% 
  arrange(desc(CAGR)) %>%
  slice_head(n = 8)

# Format a clean text label for CAGR
top_growth <- top_growth %>%
  mutate(cagr_label = paste0( percent(CAGR, accuracy = 0.1)),
         measureUnits = paste0(measureF, '\n(', value_label, ')'))
top_growth$measureUnits <- as.factor(top_growth$measureUnits)

# Keep top 8 production data 
growth_dataset <- dataset %>%
  filter(measureF %in% top_growth$measureF) %>%
  group_by(measureF) %>% 
  # Compute adjusted value (subtract initial value) to get a baseline for each production
  mutate(value_adj = value - value[year_ended_june == min(year_ended_june)]) %>% 
  ungroup() %>% 
  mutate(
    # Set the factor levels directly using the already-sorted top_growth data frame
    measureF = factor(measureF, levels = top_growth$measureF),
    measureUnits = paste0(measureF, '\n(', value_label, ')'),
    # Reorder levels as in measureF
    measureUnits = fct_reorder(measureUnits, as.numeric(measureF))
  )



## Plot ####
library(scales)
# Define a function that physically squishes the axis before a specific year
squeeze_trans <- function(split_year = 1975, squeeze_factor = 0.3) {
  trans_new(
    name = "squeeze",
    transform = function(x) {
      ifelse(is.na(x), NA,
             ifelse(x <= split_year, 
                    x * squeeze_factor, 
                    x - split_year * (1 - squeeze_factor)))
    },
    inverse = function(y) {
      ifelse(is.na(y), NA,
             ifelse(y <= split_year * squeeze_factor, 
                    y / squeeze_factor, 
                    y + split_year * (1 - squeeze_factor)))
    }
  )
}

# Plot
png('tidytuesday/2026-02-17/figure2.png',
     width = 300,
     height = 180,
     units = 'mm',
     res = 300)
ggplot(growth_dataset,
       aes(x = year_ended_june,
           y = value_adj,                
           group = measureF,
           fill = measureF)) +
  geom_area() +              
  facet_grid(measureUnits ~ ., scales = "free", switch = 'y') + # Move on strip to the left 
  labs(
    title = "Top 8 Growing Productions",
    subtitle = "Change in production volume from baseline",
    x = 'Year',
    y = NULL,
    tag = "CAGR"
  ) +
  theme_minimal() +                     
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, face = "bold"),
    strip.background = element_blank(),
    # Rotate strip text to have it horizontally
    strip.text.y.left = element_text(angle = 0, face = "bold"), 
    # Move strip facets outside
    strip.placement = "outside",
    plot.tag.position = c(0.98, 0.98),  
    plot.tag = element_text(face = "bold", size = 10, hjust = 1)
  ) +
  # Squeeze first 40 years
  scale_x_continuous(
    trans = squeeze_trans(split_year = 1975, squeeze_factor = 0.2),
    breaks = c(1935, 1955, 1975, 1985, 1995, 2005, 2015, 2025) 
  ) +
  # Add CAGR labels
  geom_text(data = top_growth,
            aes(x = 2025,               
                y = Inf,                # Push text to facet ceiling
                label = cagr_label),
            inherit.aes = FALSE,        
            hjust = 0,                  
            vjust = 3,                # Move text downwards
            size = 3.5, 
            fontface = "bold",
            color = "grey30")
dev.off()
