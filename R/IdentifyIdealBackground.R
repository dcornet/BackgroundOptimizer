# Loading Necessary Libraries -----------------------------------------------
packages <- c("tidyverse", "colorspace", "colorscience", "patchwork")

# Function to install and load packages if necessary
install_if_necessary <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
    require(package_name, character.only = TRUE)
  }
}

invisible(lapply(packages, install_if_necessary))

# Load RGB pixel collection and custom functions ---------------------------
SampName<-'EggplantPixelRGBsamples'
object_data<-read.csv2(paste0("./out/", SampName, ".csv"))
Object<-gsub('PixelRGBsamples', '', SampName)
# Load color name data from CSV files
xkcd_colors <- read.csv2('./data/XKCDWrgb.csv')

# Helper function to convert LCH to Lab
lch_to_lab <- function(LCH_data) {
  # LCH_data should be a data frame or matrix with columns named "L","C","H"
  LCH_data <- as.data.frame(LCH_data)
  
  L_vals <- LCH_data[["L"]]
  C_vals <- LCH_data[["C"]]
  H_vals_deg <- LCH_data[["H"]]
  
  # Convert Hue from degrees to radians
  H_vals_rad <- H_vals_deg * pi / 180
  
  # Calculate a and b
  a_vals <- C_vals * cos(H_vals_rad)
  b_vals <- C_vals * sin(H_vals_rad)
  
  return(data.frame(L=L_vals, a=a_vals, b=b_vals))
}

lab_to_lch <- function(Lab_data) {
  # Lab_data should be a data frame or matrix with columns named "L", "a", "b"
  Lab_data <- as.data.frame(Lab_data)
  
  L_vals <- Lab_data[["L"]]
  a_vals <- Lab_data[["a"]]
  b_vals <- Lab_data[["b"]]
  
  # Calculate chroma
  C_vals <- sqrt(a_vals^2 + b_vals^2)
  
  # Calculate hue in degrees
  H_vals_rad <- atan2(b_vals, a_vals)
  H_vals_deg <- H_vals_rad * 180 / pi
  H_vals_deg[H_vals_deg < 0] <- H_vals_deg[H_vals_deg < 0] + 360  # Ensure [0, 360)
  
  return(data.frame(L=L_vals, C=C_vals, H=H_vals_deg))
}

lch_to_hex <- function(L, C, H) {
  # Create a polarLUV object
  col <- polarLUV(L, C, H)
  
  # Convert to sRGB
  rgb <- as(col, "sRGB")
  
  # Handle out-of-gamut colors by clamping
  rgb_clamped <- coords(rgb)
  rgb_clamped[rgb_clamped < 0] <- 0
  rgb_clamped[rgb_clamped > 1] <- 1
  
  # Convert to HEX
  rgb_hex <- rgb(rgb_clamped[,1], rgb_clamped[,2], rgb_clamped[,3])
  
  return(rgb_hex)
}

getClosestXKCD <- function(r, g, b) {
  distances <- sqrt((xkcd_colors$red - r)^2 + (xkcd_colors$green - g)^2 + (xkcd_colors$blue - b)^2)
  closest_index <- which.min(distances)
  closest_color <- xkcd_colors$ColName[closest_index]
  return(closest_color)
}

add_rgb_values_colorscience <- function(df) {
  # Prepare RGB columns
  rgb_values <- apply(df[, c("L", "C", "H")], 1, function(row) {
    # Extract L, C, H values
    L <- row[1]
    C <- row[2]
    H <- row[3]
    
    # Convert LCH to Lab
    a <- C * cos(H * pi / 180)
    b <- C * sin(H * pi / 180)
    
    # Convert Lab to RGB
    tryCatch({
      lab <- c(L, a, b)
      rgb <- Lab2XYZ(lab)
      rgb <- XYZ2RGB(rgb)
      # Ensure RGB values are between 0 and 1
      rgb <- pmax(pmin(rgb, 1), 0)
      return(rgb)
    }, error = function(e) {
      # Return NA in case of conversion error
      return(c(NA, NA, NA))
    })
  })
  
  # Transpose and bind the RGB values to the dataframe
  rgb_df <- as.data.frame(t(rgb_values))
  colnames(rgb_df) <- c("R", "G", "B")
  
  # Add RGB columns to the original dataframe
  df <- cbind(df, rgb_df)
  return(df)
}

# Convert Object Pixels to LAB ---------------------------------------------
rgb_matrix <- as.matrix(object_data[, c("red", "green", "blue")])
object_lab <- convertColor(rgb_matrix, from = "sRGB", to = "Lab")

# Compute the centroid (mean) LAB of the object
object_lab_mean <- colMeans(object_lab)

# Convert Mean LAB to LCH Manually ------------------------------------------
L_val <- object_lab_mean[1]
a_val <- object_lab_mean[2]
b_val <- object_lab_mean[3]

C_val <- sqrt(a_val^2 + b_val^2)
H_val <- atan2(b_val, a_val) * 180 / pi
if (H_val < 0) H_val <- H_val + 360

object_lch_mean <- c(L_val, C_val, H_val)

L_mean <- object_lch_mean[1]
C_mean <- object_lch_mean[2]
H_mean <- object_lch_mean[3]

# Represent a hue wheel with reference hue = 0° and ΔE2000 distances --------
reference_lch <- c(L_mean, C_mean, 0)

L_val <- reference_lch[1]
C_val <- reference_lch[2]
H_val <- reference_lch[3]

# Convert reference LCH to Lab
reference_lab <- lch_to_lab(data.frame(L=L_val, C=C_val, H=H_val))[1,]

hues_wheel <- seq(0, 359, by=1)
wheel_lch <- data.frame(L=rep(L_mean, length(hues_wheel)), 
                        C=rep(C_mean, length(hues_wheel)), 
                        H=hues_wheel)

# Convert wheel LCH to Lab
wheel_lab <- lch_to_lab(wheel_lch)
reference_lab <- as.numeric(reference_lab[1, ])
wheel_lab <- as.matrix(wheel_lab)

# Compute ΔE2000 from reference color
de2000_wheel <- apply(wheel_lab, 1, function(row_lab) {
  deltaE2000(reference_lab, matrix(row_lab, nrow=1))
})

target_de2000 <- c(1, 2, 3, 4, 5, 10)
marked_points <- sapply(target_de2000, function(target) {
  idx <- which.min(abs(de2000_wheel - target))
  idx
})

# Generating Candidate Background Colors -----------------------------------
L_candidates <- L_mean
C_candidates <- seq(0, 80, by = 2) # ensure distinct chroma
hues_candidates <- seq(0, 359, by = 5) # sample every 5 degrees

# Generate all combinations of L, C, and H
candidate_lch<-expand.grid(L=L_candidates, C=C_candidates, H=hues_candidates)

# Convert candidate LCH to Lab
candidate_lab <- lch_to_lab(candidate_lch)

# Compute ΔE2000 for each candidate to object_lab_mean
candidate_de2000 <- apply(candidate_lab, 1, function(c_lab) {
  deltaE2000(matrix(object_lab_mean, nrow=1), matrix(c_lab, nrow=1))
})
candidate_lch$deltaE2000 <- candidate_de2000

# Filter candidates that are "far enough" (ΔE2000 > 5)
far_candidates <- candidate_lch %>% filter(deltaE2000 > 5)

# Identify best candidates (max ΔE2000)
max_deltaE2000 <- max(candidate_lch$deltaE2000)
best_candidates <- candidate_lch %>% filter(deltaE2000 == max_deltaE2000)

cat("\nCandidate background colors (in LCH) and their ΔE2000:\n")
print(head(candidate_lch))   ### TODO add color name!
cat("\nBest candidate background color(s):\n")
print(best_candidates) ### TODO add color name!

# Convert far candidates and best candidates to Lab and then sRGB
far_candidates_lab <- lch_to_lab(far_candidates[,c("L","C","H")])
far_candidates_rgb <- convertColor(as.matrix(far_candidates_lab), from="Lab", to="sRGB")
far_candidates_rgb <- pmax(pmin(far_candidates_rgb, 1), 0)

best_candidates_lab <- lch_to_lab(best_candidates[,c("L","C","H")])
best_candidates_rgb <- convertColor(as.matrix(best_candidates_lab), from="Lab", to="sRGB")
best_candidates_rgb <- pmax(pmin(best_candidates_rgb, 1), 0)

# Compute ΔE2000 of each object pixel to best candidate (pick the first best)
best_candidate_lab <- as.matrix(best_candidates_lab[1, , drop=FALSE])
object_de2000_to_best <- apply(object_lab, 1, function(obj_lab) {
  deltaE2000(matrix(obj_lab, nrow=1), best_candidate_lab)
})

# Objective colors not well distinguishable (ΔE2000 < 5)
near_object_pixels <- object_data[object_de2000_to_best < 5,]

###############################################################################
# Additional Plots Requested by the User
###############################################################################

# 1) Plot selected pixels color ranged by hue (using geom_tile)
# Convert object_data to LCH and order by hue
object_lch <- lab_to_lch(as.data.frame(object_lab))

object_lab_df <- data.frame(L=object_lab[,1], a=object_lab[,2], b=object_lab[,3])
object_lch_df <- lab_to_lch(object_lab_df)

object_pixels_hue <- data.frame(object_data, object_lch_df)
object_pixels_hue <- object_pixels_hue %>% arrange(H)

object_pixels_hue$index <- seq_len(nrow(object_pixels_hue))

# Keep only the 10 farest colors
# Perform clustering to ensure diversity among the top candidates Using k-means clustering to group into 10 clusters
set.seed(123) # For reproducibility
kmeans_result <- kmeans(object_pixels_hue[, c("L", "C", "H")], centers = 10)

# Select one representative from each cluster Add cluster assignments to the data
object_pixels_hue$cluster <- kmeans_result$cluster

# Select one row with the highest deltaE2000 from each cluster
object_pixels_hue10 <- object_pixels_hue %>%
  group_by(cluster) %>%
  slice_max(H, n = 1, with_ties = FALSE) %>%
  ungroup()
object_pixels_hue10 <- object_pixels_hue10 %>%  arrange(H)
object_pixels_hue10$order<-rownames(object_pixels_hue10)

# Add color name
object_pixels_hue10$colNameXKCD <- apply(object_pixels_hue10, 1, function(row) {
  getClosestXKCD(as.numeric(row['red']) * 255, as.numeric(row['green']) * 255, as.numeric(row['blue']) * 255)
})

p1<-ggplot(object_pixels_hue10, aes(x=reorder(order, H), y=1, fill=rgb(red,green,blue))) +
  geom_tile() +
  scale_fill_identity() +
  geom_text(aes(label = colNameXKCD), size = 4, angle = 90, hjust = 0.5, 
            vjust = 0, color = ifelse(object_pixels_hue10$L<40, "white", "black")) +    
  theme_minimal() +
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank()) +
  ggtitle(paste0("Subsample ", Object,  " Color Ordered by Hue"))+
  coord_fixed(ratio = 2.5, expand = FALSE)

# 2) Plot bg potential colors
# Apply the conversion to all candidate colors
candidate_lch$hex <- mapply(lch_to_hex, candidate_lch$L, candidate_lch$C, candidate_lch$H)

# Similarly, convert best_candidates to HEX
best_candidates$hex <- lch_to_hex(best_candidates$L, best_candidates$C, best_candidates$H)

# Add an identifier for best candidates
candidate_lch <- candidate_lch %>%
  mutate(is_best = ifelse(H %in% best_candidates$H & C == best_candidates$C & L == best_candidates$L, TRUE, FALSE))

# For better visualization, arrange the hues in order
candidate_lch <- candidate_lch %>%
  arrange(H) %>%
  mutate(order = row_number())

# Keep only the 10 farest colors
# Order by deltaE2000 in decreasing order
candidate_lch <- candidate_lch %>%  arrange(desc(deltaE2000))

# Perform clustering to ensure diversity among the top candidates Using k-means clustering to group into 10 clusters
set.seed(123) # For reproducibility
kmeans_result <- kmeans(candidate_lch[, c("L", "C", "H")], centers = 10)

# Select one representative from each cluster Add cluster assignments to the data
candidate_lch$cluster <- kmeans_result$cluster

# Select one row with the highest deltaE2000 from each cluster
candidate_lch10 <- candidate_lch %>%
  group_by(cluster) %>%
  slice_max(deltaE2000, n = 1, with_ties = FALSE) %>%
  ungroup()
candidate_lch10 <- candidate_lch10 %>%  arrange(desc(deltaE2000))
candidate_lch10$order<-rownames(candidate_lch10)

candidate_lch10<- add_rgb_values_colorscience(candidate_lch10)

# Add color name
candidate_lch10$colNameXKCD <- apply(candidate_lch10, 1, function(row) {
  getClosestXKCD(as.numeric(row['R']) * 255, as.numeric(row['G']) * 255, as.numeric(row['B']) * 255)
})

# Plotting with ggplot2
p2<-ggplot(candidate_lch10, aes(x = reorder(order, H), y = 1, fill = hex)) +
  geom_tile(color = "white") +
  scale_fill_identity() +
  geom_text(aes(label = colNameXKCD), size = 4, angle = 90, hjust = 0.5,
            vjust = 0, color = ifelse(candidate_lch10$L<40, "white", "black")) +    
  geom_text(aes(label = round(deltaE2000, 1), y=1), size = 4,  hjust = 1, 
            vjust = 9, color = ifelse(candidate_lch10$L<40, "white", "black")) +      
  theme_minimal() +
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank()) +
  coord_fixed(ratio = 2.5, expand = FALSE) +
  # Highlight the best candidates
  geom_tile(data = subset(candidate_lch10, is_best), aes(x = order, y = 1),
    color = "red", size = 2, fill = NA) +
  ggtitle("Candidate Background Colors with Best Candidate Highlighted and ΔE2000 values") 

combined_plot <- p1 / p2  # Using patchwork's '/' operator for stacking plots vertically
png(height = 5, width = 10, res = 300, type = "cairo", family = "Garamond", units = "in",
    filename = paste0("./out/", Object, "SelectedPixelsVScandidatesBG.png"))
print(combined_plot)
dev.off()


# 2) Lab plot of candidate and object colors -----------------------------------
# For candidate data:
lab_cand <- lch_to_lab(candidate_lch10[, c("L", "C", "H")])
lab_cand <- lab_cand[, c("a", "b")]
candidate_lab10 <- cbind(candidate_lch10, lab_cand)  # candidate_lab10 now has L (original), a, b, hex, etc.

# For object data:
lab_obj <- lch_to_lab(object_pixels_hue10[, c("L", "C", "H")])
lab_obj <- lab_obj[, c("a", "b")]
object_lab10 <- cbind(object_pixels_hue10, lab_obj)  # object_lab10 now has L, a, b, hex, etc.

# Mark each set and combine
candidate_lab10 <- candidate_lab10 %>%
  mutate(point_type = "Candidate")

object_lab10 <- object_lab10 %>%
  mutate(point_type = "Object")

all_points <- bind_rows(candidate_lab10, object_lab10)

# Choose a fixed L for the slice
L_fixed <- 40  

# Define a and b ranges (use a coarser step to keep the plot from being huge)
a_vals <- seq(-128, 127, by = 5)
b_vals <- seq(-128, 127, by = 5)
grid_lab <- expand.grid(a = a_vals, b = b_vals)
grid_lab$L <- L_fixed

# Function to convert Lab to hex using convertColor (you likely already have one)
lab_to_hex <- function(L, a, b) {
  lab_matrix <- cbind(L, a, b)
  srgb <- convertColor(lab_matrix, from = "Lab", to = "sRGB")
  srgb_clamped <- pmax(pmin(srgb, 1), 0)
  apply(srgb_clamped, 1, function(rgb) {
    grDevices::rgb(rgb[1], rgb[2], rgb[3])
  })
}

grid_lab$hex <- with(grid_lab, lab_to_hex(L, a, b))
all_points$hex <- with(all_points, lab_to_hex(L, a, b))

p_lab_slice <- ggplot() +
  geom_tile(data = grid_lab, aes(x = a, y = b, fill = hex),
            width  = 5, height = 5) +
  scale_fill_identity() +
  geom_point(data = all_points, aes(x = a, y = b, color=point_type), size = 3) +
  coord_fixed(ratio = 1) +  
  theme_bw(base_size = 18) +
  # labs( title = paste("Lab Slice at L =", L_fixed),
  #       subtitle = "Background tiles represent Lab -> sRGB; points are Candidate (circles) and Object (squares)") +
  theme(panel.grid = element_blank())+
  scale_color_manual("Color", values=c("grey80", "black"), label=c("User pixel", "Proposed Background"))+
  theme(legend.position = "inside", legend.position.inside = c(0.75,0.85))+
  xlab("CIE a* value")+
  ylab("CIE b* value")

print(p_lab_slice)
