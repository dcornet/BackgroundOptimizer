# Loading Necessary Libraries -----------------------------------------------
packages <- c("tidyverse", "imager")

# Function to install and load packages if necessary
install_if_necessary <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
    require(package_name, character.only = TRUE)
  }
}

invisible(lapply(packages, install_if_necessary))

# Parameters ---------------------------------------------------------------
number_of_images <- 3       # Number of images to load and collect object pixels from
pixels_per_image <- 20      # Number of object pixels to sample from each image


# Data Collection for Object Pixels -----------------------------------------
object_data <- data.frame()

for (i in 1:number_of_images) {
  cat(paste0("\nSelect image ", i, " of ", number_of_images, "...\n"))
  selected_image <- load.image(file.choose())
  plot(selected_image)
  
  cat(paste0("\nPlease select ", pixels_per_image, " pixels corresponding to the object of interest...\n"))
  object_points <- as.data.frame(lapply(locator(pixels_per_image), round, 0))
  
  # Extract RGB values at selected points
  img_pixels <- data.frame(
    red   = diag(selected_image[object_points$x, object_points$y, 1, 1]),
    green = diag(selected_image[object_points$x, object_points$y, 1, 2]),
    blue  = diag(selected_image[object_points$x, object_points$y, 1, 3])
  )
  
  object_data <- bind_rows(object_data, img_pixels)
}

write.csv2(object_data, "./out/PixelRGBsamples.csv", row.names = F)

