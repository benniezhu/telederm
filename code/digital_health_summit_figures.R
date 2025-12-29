library(tidyverse)
# Create a data frame for twopm oop
tele_oop_mfx <- tibble(
    Level = c("2021", "2022", "Male", "Black", "Hispanic", "Other Race", "HS Diploma", "Bachelor or more", "Midwest", "South", "West", "100%-199% FPL", "200%-399% FPL", "400%+ FPL", "Medicaid", "Medicare", "Medicare Advantage", "Uninsured 1+ Month", "Other Insurance", "Acne", "Psoriasis", "Hair Loss", "Pruritus", "Dermatitis", "Charlson Score"),
    dy_dx = c(0.9294443, 0.6984593, -0.174708, 1.784037, 5.756096, 1.323325, 0.3813651, 1.110924, 0.3042733, 0.2415343, 0.9677466, 1.307576, -0.2571811, 0.1843203, -0.6250909, -0.3694714, 0.6555152, 0.9556798, NA, 1.012983, -0.0942601, 0.860907, NA, 0.8074578, 0.0554592),
    Std_Err = c(0.2797713, 0.2447962, 0.2226774, 1.33812, 3.147644, 0.6334426, 0.4309113, 0.5434083, 0.2924259, 0.2140466, 0.3903959, 1.412904, 0.3100735, 0.3465267, 0.1601568, 0.2623969, 0.9164464, 1.057551, NA, 0.59228, 0.3067802, 1.141713, NA, 0.4981038, 0.1619893),
    t_value = c(3.32, 2.85, -0.78, 1.33, 1.83, 2.09, 0.89, 2.04, 1.04, 1.13, 2.48, 0.93, -0.83, 0.53, -3.9, -1.41, 0.72, 0.9, NA, 1.71, -0.31, 0.75, NA, 1.62, 0.34),
    p_value = c(0.001, 0.005, 0.433, 0.184, 0.069, 0.038, 0.377, 0.042, 0.299, 0.26, 0.014, 0.356, 0.408, 0.595, 0.000, 0.16, 0.475, 0.367, NA, 0.088, 0.759, 0.452, NA, 0.106, 0.732),
    CI_Lower = c(0.3784565, 0.2163522, -0.6132539, -0.8512869, -0.4429439, 0.0758091, -0.4672813, 0.0407236, -0.2716367, -0.1800138, 0.1988922, -1.475029, -0.8678468, -0.498137, -0.9405072, -0.8862417, -1.149355, -1.127084, NA, -0.1534664, -0.6984398, -1.387607, NA, -0.173519, -0.2635661),
    CI_Upper = c(1.480432, 1.180566, 0.2638379, 4.419361, 11.95514, 2.570841, 1.230012, 2.181125, 0.8801834, 0.6630824, 1.736601, 4.090181, 0.3534846, 0.8667776, -0.3096746, 0.1472989, 2.460385, 3.038444, NA, 2.179433, 0.5099196, 3.109422, NA, 1.788435, 0.3744845)
) %>%
    mutate(spending_type = "OOP")

# Create a data frame for twopm total 
tele_total_mfx <- tibble(
    Level = c("2021", "2022", "Male", "Black", "Hispanic", "Other Race", "HS Diploma", "Bachelor or more", "Midwest", "South", "West", "100%-199% FPL", "200%-399% FPL", "400%+ FPL", "Medicaid", "Medicare", "Medicare Advantage", "Uninsured 1+ Month", "Other Insurance", "Acne", "Psoriasis", "Hair Loss", "Pruritus", "Dermatitis", "Charlson Score"),
    dy_dx = c(8.782723, 1.935483, -0.7562079, 33.3051, 14.58278, 9.100839, 0.5255147, 6.877716, -4.864792, -3.8737, 2.181148, -4.043194, -0.0245688, -0.2967091, 5.236314, 4.383635, 17.74762, -0.6179911, 28.46085, 5.567795, 13.51596, 51.04843, -3.413747, -0.341599, -0.8936342),
    Std_Err = c(3.693495, 1.715873, 1.821029, 22.16121, 10.31161, 4.68507, 1.796323, 3.817327, 3.475147, 3.020543, 3.520274, 5.645809, 5.635594, 5.464961, 10.67472, 8.475931, 17.59812, 1.548564, 37.90872, 3.905133, 10.67756, 53.27801, 3.798544, 2.356663, 1.31537),
    t_value = c(2.38, 1.13, -0.42, 1.50, 1.41, 1.94, 0.29, 1.80, -1.40, -1.28, 0.62, -0.72, -0.00, -0.05, 0.49, 0.52, 1.01, -0.40, 0.75, 1.43, 1.27, 0.96, -0.90, -0.14, -0.68),
    p_value = c(0.018, 0.260, 0.678, 0.134, 0.159, 0.053, 0.770, 0.073, 0.163, 0.201, 0.536, 0.475, 0.997, 0.957, 0.624, 0.605, 0.314, 0.690, 0.453, 0.155, 0.207, 0.339, 0.370, 0.885, 0.498),
    CI_Lower = c(1.508811, -1.443731, -4.342515, -10.33885, -5.724759, -0.1258669, -3.012137, -0.6400697, -11.70869, -9.822312, -4.751627, -15.16196, -11.12322, -11.05932, -15.78632, -12.30874, -16.90985, -3.66771, -46.19601, -2.122914, -7.512253, -53.87647, -10.89454, -4.982776, -3.484104),
    CI_Upper = c(16.05664, 5.314698, 2.830099, 76.94904, 34.89032, 18.32754, 4.063167, 14.3955, 1.97911, 2.074912, 9.113922, 7.075577, 11.07409, 10.4659, 26.25894, 21.07601, 52.4051, 2.431728, 103.1177, 13.2585, 34.54418, 155.9733, 4.067048, 4.299578, 1.696835)
) %>%
    mutate(spending_type = "Total")

# 2. Define the Mapping and the "Proper Order"
# We define the order of categories and the order of levels within them


# 2. Explicitly define the internal order of levels
# List them exactly as you want them to appear from TOP to BOTTOM
level_order <- c(
  # Time
  "2021", "2022",
  # Demographics
  "Male", "Black", "Hispanic", "Other Race",
  # Education
  "HS Diploma", "Bachelor or more",
  # Income
  "100%-199% FPL", "200%-399% FPL", "400%+ FPL",
  # Region
  "Midwest", "South", "West",
  # Insurance
  "Medicaid", "Medicare", "Medicare Advantage", "Uninsured 1+ Month", "Other Insurance",
  # Health Condition
  "Acne", "Psoriasis", "Hair Loss", "Pruritus", "Dermatitis", "Charlson Score"
)

# 3. Apply the factor levels and category mapping
tele_spending_mfx <- tele_spending_mfx %>%
  mutate(Category = case_when(
    Level %in% c("2021", "2022") ~ "Time",
    Level %in% c("Male", "Black", "Hispanic", "Other Race") ~ "Demographics",
    Level %in% c("HS Diploma", "Bachelor or more") ~ "Education",
    Level %in% c("100%-199% FPL", "200%-399% FPL", "400%+ FPL") ~ "Income (FPL)",
    Level %in% c("Midwest", "South", "West") ~ "Region",
    Level %in% c("Medicaid", "Medicare", "Medicare Advantage", "Uninsured 1+ Month", "Other Insurance") ~ "Insurance",
    TRUE ~ "Health/Condition"
  )) %>%
  mutate(
    Category = factor(Category, levels = c("Time", "Demographics", "Education", "Income (FPL)", "Region", "Insurance", "Health/Condition")),
    # We use rev() because ggplot plots the first factor level at the bottom of the y-axis
    Level = factor(Level, levels = rev(level_order))
  )

# 4. Create the Plot
ggplot(tele_spending_mfx, aes(x = dy_dx, y = Level, color = Category)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", alpha = 0.4) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.3, size = 0.9) + # Slightly thicker bars
  geom_point(size = 3) + # Slightly larger points for poster
  # Use space = "free_y" to keep the groupings tight
  facet_grid(Category ~ spending_type, scales = "free", space = "free_y") +
  labs(
    title = "Marginal Effects on Teledermatology Spending",
    subtitle = "Two Part Models, 95% CI",
    x = "Marginal Effect (Change in $)",
    y = NULL,
    caption = 'Reference groups for insurance, race/ethnicity, sex, poverty categories,
     year and census region were private insurance, non-Hispanic white, female,
      <100% FPL, 2020, and Northeast'
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text.y = element_text(angle = 0, face = "bold"),
    strip.background = element_rect(fill = "grey95"),
    axis.text.y = element_text(size = 9)
  )

ggsave("F:/projects/telederm/output/two_pm_plot.png", 
width = 12, height = 10, units = 'in', dpi = 400)


##########################################

#logistic regression coefs

telemed_data <- tibble(
  Variable = c("age", "age2", "2021", "2022", "Male", "Black", "Hispanic", "Other Race",
               "HS Diploma", "Bachelor or More", "Midwest", "South", "West",
               "100%-199% FPL", "200%-399% FPL", "400%+ FPL", "Medicaid", 
               "Medicare", "Medicare Advantage", "Uninsured 1+ Month", "Other Insurance", 
               "Acne", "Psoriasis", "Hair Loss", "Pruritus", "Dermatitis", "Charlson Score", 
               "Constant"),
  Odds_Ratio = c(0.9430985, 1.000477, 2.725295, 1.525384, 0.9984264, 6.467061, 5.519723, 3.415515, 
                 2.046686, 5.470094, 1.009454, 0.8942062, 2.942355, 0.6875718, 1.10214, 1.119851, 
                 0.8461868, 0.8160815, 0.844796, 2.029489, 1.264022, 5.49117, 3.4714, 1.171245, 
                 1.844603, 2.642832, 0.8539511, 0.0034279),
  Std_Err = c(0.045876, 0.0004886, 0.9266525, 0.5971661, 0.2771, 2.978739, 3.723172, 1.333083, 
              1.21533, 3.935941, 0.5802336, 0.4727729, 1.339935, 0.3754649, 0.5422327, 0.4979834, 
              0.4571995, 0.6861477, 0.4733853, 1.069933, 1.395739, 2.019579, 1.885915, 0.9238613, 
              1.576175, 0.9722306, 0.1249745, 0.0036361),
  t_value = c(-1.20, 0.98, 2.95, 1.08, -0.01, 4.05, 2.53, 3.15, 1.21, 2.36, 0.02, -0.21, 2.37, 
              -0.69, 0.20, 0.25, -0.31, -0.24, -0.30, 1.34, 0.21, 4.63, 2.29, 0.20, 0.72, 2.64, 
              -1.08, -5.35),
  p_value = c(0.230, 0.330, 0.003, 0.282, 0.995, 0.000, 0.012, 0.002, 0.229, 0.019, 0.987, 0.833, 
              0.019, 0.493, 0.843, 0.799, 0.757, 0.809, 0.764, 0.181, 0.832, 0.000, 0.023, 0.841, 
              0.474, 0.009, 0.282, 0.000),
  CI_Lower = c(0.8569437, 0.9995154, 1.395075, 0.7055766, 0.5780173, 2.610703, 1.46219, 1.583557, 
               0.6355869, 1.326108, 0.325435, 0.3156738, 1.200048, 0.234566, 0.4182617, 0.466469, 
               0.2919757, 0.155818, 0.2802106, 0.7185913, 0.14366, 2.661335, 1.190825, 0.2477451, 
               0.3428255, 1.280645, 0.6401212, 0.0004244),
  CI_Upper = c(1.037915, 1.00144, 5.323896, 3.297723, 1.724612, 16.01977, 20.83679, 7.3668, 
               6.590639, 22.56372, 3.131183, 2.53301, 7.214256, 2.015445, 2.904192, 2.688426, 
               2.452369, 4.274148, 2.546942, 5.731805, 11.12175, 11.33001, 10.11956, 5.537204, 
               9.925055, 5.453936, 1.13921, 0.0276871)
) %>%
rename(Level = Variable) %>%
mutate(Category = case_when(
    Level %in% c("2021", "2022") ~ "Time",
    Level %in% c("age", "age2", "Male", "Black", "Hispanic", "Other Race") ~ "Demographics",
    Level %in% c("HS Diploma", "Bachelor or More") ~ "Education",
    Level %in% c("100%-199% FPL", "200%-399% FPL", "400%+ FPL") ~ "Income (FPL)",
    Level %in% c("Midwest", "South", "West") ~ "Region",
    Level %in% c("Medicaid", "Medicare", "Medicare Advantage", "Uninsured 1+ Month", "Other Insurance") ~ "Insurance",
    TRUE ~ "Health/Condition"
  )) %>%
  filter(Category != "Health/Condition")


# 1. Prepare the data and define the exact ordering
cat_order <- c("Time", "Demographics", "Education", "Income (FPL)", "Region", "Insurance")

# Define internal order for levels (including your Demographic preference)
level_order_logistic <- c(
  "2021", "2022",                                      # Time
  "age", "age2", "Male", "Black", "Hispanic", "Other Race", # Demographics
  "HS Diploma", "Bachelor or More",                    # Education
  "100%-199% FPL", "200%-399% FPL", "400%+ FPL",       # Income
  "Midwest", "South", "West",                          # Region
  "Medicaid", "Medicare", "Medicare Advantage",        # Insurance
  "Uninsured 1+ Month", "Other Insurance"
)

plot_data <- telemed_data %>%
  # Filter out 'Constant' as it skews the scale and is usually not plotted
  filter(Level != "Constant") %>% filter(Level != "age") %>% filter(Level != "age2") %>%
  mutate(
    Category = factor(Category, levels = cat_order),
    Level = factor(Level, levels = rev(level_order_logistic)) # rev() so first item is at top
  )

# 2. Create the Plot
ggplot(plot_data, aes(x = Odds_Ratio, y = Level, color = Category)) +
  # Add vertical reference line at 1
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", alpha = 0.5) +
  # Point and Confidence Intervals
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), , height = 0.3, size = 0.9) +
  # Faceting to group by category
  facet_grid(Category ~ ., scales = "free_y", space = "free_y") +
  # Use Log scale for X axis (standard for Odds Ratios)
  scale_x_log10(breaks = c(0.1, 0.5, 1, 2, 5, 10, 20)) +
  labs(
    title = "Odds Ratios for Teledermatology Usage",
    x = "Odds Ratio (Log Scale)",
    y = NULL,
    color = "Variable Group",
    caption = 'Reference groups for insurance, race/ethnicity, sex, poverty categories,
     year and census region were private insurance, non-Hispanic white, female,
      <100% FPL, 2020, and Northeast'
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 1, size = 10, face = "italic"),
    strip.text.y = element_text(angle = 0, face = "bold"),
    strip.background = element_rect(fill = "grey95"),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 9)
  ) +
  guides(color = guide_legend(override.aes = list(size = 1)))
  
ggsave("F:/projects/telederm/output/logistic_plot.png", 
width = 12, height = 10, units = 'in', dpi = 400)


