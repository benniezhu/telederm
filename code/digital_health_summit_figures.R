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
    title = "Marginal Effects on Telehealth Spending",
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
