# Install required packages if not already installed
# Loading Necessary Libraries -----------------------------------------------
packages <- c("tidyverse", "farver", "purrr")

# Function to install and load packages if necessary
install_if_necessary <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
    require(package_name, character.only = TRUE)
  }
}
invisible(lapply(packages, install_if_necessary))


# ----------------------------
# 1. Define Reference Color
# ----------------------------

# Define reference color in LCH
ref_L <- 45        # Lightness
ref_C <- 80        # Chroma (set to maximum chroma as per requirement)
ref_H <- 0         # Hue in degrees (e.g., 0° for red)

# Create a data frame for the reference color
ref_color_LCH <- data.frame(L = ref_L, C = ref_C, H = ref_H)

# Convert reference color from LCH to Lab using farver
ref_color_Lab <- convert_colour(ref_color_LCH, from = "lch", to = "lab")

# ----------------------------
# 2. Generate Chroma Axis Grid
# ----------------------------

# Fixed hue for the chroma axis (same as reference hue)
fixed_H <- ref_H  # 0° for red

# Define chroma values at intervals of 10 units
chroma_values <- seq(0, 80, by = 10)  # From 0 to 80 in steps of 10

# Create a dataframe with varying chroma and fixed hue
grid <- data.frame(
  L = ref_L,
  C = chroma_values,
  H = fixed_H
)

# Convert grid colors from LCH to Lab using farver
grid_lab <- convert_colour(grid, from = "lch", to = "lab")

# Add Lab components to the grid dataframe
grid <- grid %>%
  mutate(
    L_star = grid_lab[, "l"],
    a_star = grid_lab[, "a"],
    b_star = grid_lab[, "b"]
  )

# ----------------------------
# 3. Compute Delta E2000 Values
# ----------------------------

# Compute Delta E2000 values relative to the reference color
grid$deltaE2000 <- compare_colour(
  from = as.matrix(grid[, c("L_star", "a_star", "b_star")]),
  to = as.matrix(ref_color_Lab),
  from_space = "lab",
  to_space = "lab",
  method = "cie2000"
)

# Manually set deltaE2000 to 0 for the reference point (C = 80)
grid <- grid %>%
  mutate(
    deltaE2000 = ifelse(C == ref_C, 0, deltaE2000)
  )

# ----------------------------
# 4. Prepare for Plotting
# ----------------------------

# Convert hue to radians for polar plotting
grid <- grid %>%
  mutate(
    H_rad = (H * pi) / 180  # Convert degrees to radians
  )

# ----------------------------
# 5. Create Additional Points at Specified Angles
# ----------------------------

# Define additional hue angles (convert negative angles to equivalent positive angles)
additional_hues <- c(-15, -30, -45, -60, -75, -90)
additional_hues_positive <- (additional_hues + 360) %% 360  # Convert to [0, 360)

# Create a dataframe for additional points
additional_points_LCH <- data.frame(
  L = ref_L,
  C = ref_C,  # Place at maximum chroma
  H = additional_hues_positive
)

# Convert additional points from LCH to Lab
additional_points_Lab <- convert_colour(additional_points_LCH, from = "lch", to = "lab")

# Add Lab components to the additional points dataframe
additional_points <- additional_points_LCH %>%
  mutate(
    L_star = additional_points_Lab[, "l"],
    a_star = additional_points_Lab[, "a"],
    b_star = additional_points_Lab[, "b"]
  )

# Compute Delta E2000 values relative to the reference color
additional_points$deltaE2000 <- compare_colour(
  from = as.matrix(additional_points[, c("L_star", "a_star", "b_star")]),
  to = as.matrix(ref_color_Lab),
  from_space = "lab",
  to_space = "lab",
  method = "cie2000"
)

# Since these are additional points, they are not the reference point
# However, if any of them coincide with C = 80 and H = 0, ensure deltaE2000 is 0
additional_points <- additional_points %>%
  mutate(
    deltaE2000 = ifelse(C == ref_C & H == ref_H, 0, deltaE2000)
  )

# Convert hue to radians for polar plotting
additional_points <- additional_points %>%
  mutate(
    H_rad = (H * pi) / 180  # Convert degrees to radians
  )

# ----------------------------
# 6. Create Background Hue Wheel
# ----------------------------

# Define a fine grid for the background hue wheel
background_hues <- seq(0, 359, by = 1)    # Hue from 0 to 359 degrees
background_chromas <- seq(0, 80, by = 5) # Chroma from 0 to 80 in steps of 5

# Create a complete grid of hues and chromas
background_grid <- expand.grid(
  H = background_hues,
  C = background_chromas
) %>%
  mutate(
    L = ref_L  # Maintain consistent lightness
  )

# Reorder columns to match LCH
background_grid <- background_grid %>%
  select(L, C, H)

# Convert background grid from LCH to Lab
background_lab <- convert_colour(background_grid, from = "lch", to = "lab")

# Convert Lab to sRGB
background_rgb <- convert_colour(background_lab, from = "lab", to = "rgb")

# Add RGB components to the background grid
background_grid <- background_grid %>%
  mutate(
    R = background_rgb[, "r"],
    G = background_rgb[, "g"],
    B = background_rgb[, "b"]
  ) %>%
  mutate(
    # Correct scaling: Convert from [0, 255] to [0, 1]
    R_clipped = pmin(pmax(R / 255, 0), 1),
    G_clipped = pmin(pmax(G / 255, 0), 1),
    B_clipped = pmin(pmax(B / 255, 0), 1),
    # Create hex color without setting to NA
    hex = rgb(R_clipped, G_clipped, B_clipped)
  )

# ----------------------------
# 7. Combine All Points
# ----------------------------

# Combine the main grid and additional points for plotting
all_points <- bind_rows(
  grid %>% mutate(Point_Type = "Main Axis"),
  additional_points %>% mutate(Point_Type = "Additional Points")
)

# ----------------------------
# 8. Create Hue Labels Dataframe
# ----------------------------

# Define key hue angles for labels (e.g., every 30 degrees)
label_hues <- seq(0, 330, by = 30)  # 0°, 30°, ..., 330°

# Create a dataframe for hue labels
hue_labels <- data.frame(
  H = label_hues,
  label = paste0(label_hues, "°")
)

# Convert hue to radians and set label position slightly outside the maximum chroma
hue_labels <- hue_labels %>%
  mutate(
    H_rad = (H * pi) / 180,  # Convert degrees to radians
    C = 85  # Position labels slightly beyond maximum chroma for clarity
  )

# ----------------------------
# 9. Plotting
# ----------------------------

# Create the plot
p <- ggplot() +
  # Add background hue wheel using geom_tile
  geom_tile(data = background_grid,  aes(x = (H * pi / 180), y = C, fill = hex),
            width = (1 * pi / 180), height = 5, alpha = 1) +
  # Use the hex colors directly
  scale_fill_identity() +
  
  # Draw the main chroma axis line
  geom_line(data = grid, aes(x = H_rad, y = C), color = "grey70", linewidth = .5) +
  
  # Add points for each chroma value on the main axis (all points black)
  geom_point(data = grid %>% filter(C != ref_C), aes(x = H_rad, y = C), color = "black", size = 3) +
  
  # Highlight the reference point at maximum chroma with deltaE=0
  geom_point(data = grid %>% filter(C == ref_C), aes(x = H_rad, y = C),
             color = "white", size = 3) +
  
  # Add text labels for Delta E2000 values on the main axis
  geom_text(aes(x = H_rad, y = C, label = round(deltaE2000,1)),
            data = grid %>% filter(C != ref_C), hjust = -.3, vjust = 0.5, size = 4, color = "black") +

  # Add text labels for Delta E2000 values on the main axis
  geom_text(aes(x = H_rad, y = C, label ="Reference"),
            data = grid %>% filter(C == ref_C), hjust = -.1, vjust = 0.5, size = 4, color = "black") +
  
  # Add additional points at specified angles (all points black)
  geom_point(data = additional_points, aes(x = H_rad, y = C), color = "black",
             size = 3) +
  
  # Add text labels for Delta E2000 values on additional points
  geom_text(data = additional_points,
    aes(x = H_rad, y = C, label = round(deltaE2000, 1)),
    hjust = -0.3, vjust = 0.5, size = 4, color = "black") +
  
  # Add Hue Axis Labels
  geom_text(data = hue_labels, aes(x = H_rad, y = C+10, label = label),
            size = 4, color = "black", inherit.aes = FALSE) +
  
  # Optionally, add tick marks for hue labels
  geom_segment(data = hue_labels,
    aes(x = H_rad, y = C - 2.5, xend = H_rad, yend = C + 2.5),
    color = "black", size = 0.5) +
  
  # Remove the color gradient legend as it's no longer needed
  guides(fill = FALSE) +
  
  # Set polar coordinates
  coord_polar(theta = "x", start = (fixed_H * pi / 180)) +
  
  # Theme and labels
  theme_minimal() +
  labs(
    title = "Delta E2000 Values Along Chroma and Hue Axis",
    subtitle = paste("Reference LCH:", paste(ref_L, ref_C, ref_H, sep = ", ")),
    x = "Hue (degrees)", y = "Chroma") +
  
  # Customize theme elements
  theme(
    axis.text.x = element_blank(),  axis.title.x = element_blank(),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(), legend.position = "none")

# Display the plot
png(height=7, width=6, res=300, type="cairo",family="Garamond",
    filename="./out/HueWheelWithDeltaE2000.png", units="in")
print(p)
dev.off()