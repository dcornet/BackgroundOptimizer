# Optimal Background Selector for Image Analysis Experiments

This repository provides a tool to help users identify the best background colors for image analysis experiments. It utilizes advanced color science techniques to ensure the selected backgrounds are distinguishable from object colors, enhancing the reliability of image analysis.

---

## Features
1. **Automatic Package Management**:
   - Installs and loads required R packages (`tidyverse`, `imager`, `colorspace`, `colorscience`).

2. **Object Pixel Extraction**:
   - Load and display images for manual selection of object pixels.
   - Sample RGB values at selected pixel locations.

3. **Color Conversion**:
   - Convert colors between RGB, LAB, and LCH color spaces for accurate color representation.

4. **Perceptual Color Difference (ΔE2000)**:
   - Compute ΔE2000 color differences to evaluate distinguishability between object colors and candidate background colors.

5. **Hue Wheel Visualization**:
   - Visualize ΔE2000 differences as a function of hue to identify distinguishable background options.

6. **Candidate Background Colors**:
   - Generate background color candidates based on user-defined constraints.
   - Identify and filter candidates based on ΔE2000 thresholds.

7. **Advanced Plots**:
   - Plot object pixel colors ordered by hue.
   - Display hue wheels with objective and background colors for comprehensive visualization.

---

## Code Overview

### 1. **Setup**
- Installs necessary R packages using a helper function.
- Sets user-defined parameters for image loading and pixel sampling.

### 2. **Color Conversion Functions**
- `lch_to_lab`: Converts LCH color data to LAB.
- `lab_to_lch`: Converts LAB color data to LCH.

### 3. **Object Pixel Collection**
- Allows users to interactively select object pixels from images.
- Extracts RGB values at user-specified pixel locations.

### 4. **Color Analysis**
- Converts object pixel colors from RGB to LAB and LCH.
- Computes the centroid of object colors in the LAB color space.
- Calculates ΔE2000 differences between object colors and candidate backgrounds.

### 5. **Visualization**
- Plots ΔE2000 values across a hue wheel.
- Identifies and marks candidate background colors with significant ΔE2000 differences.
- Provides detailed plots of object and candidate background colors.

### 6. **Background Selection**
- Generates candidate background colors in the LCH space.
- Filters candidates to ensure sufficient perceptual distance from object colors.

---

## Usage

### Prerequisites
Ensure R is installed on your system. Required libraries will be installed automatically when the script runs.

### Running the Script
1. Clone this repository.
2. Open the script in your preferred R editor.
3. Run the script step-by-step:
   - Load images and interactively select object pixels.
   - Review visualizations to evaluate candidate backgrounds.
   - Use suggested backgrounds for your experiments.

### Outputs
- A hue wheel visualization with ΔE2000 values.
- A list of candidate background colors with corresponding ΔE2000 values.
- Plots of object pixels and candidate background colors.

---

## Example Workflow
1. Load two images.
2. Select 20 pixels per image corresponding to the object of interest.
3. View a hue wheel with ΔE2000 differences from the object colors.
4. Generate candidate background colors with sufficient ΔE2000 values.
5. Use suggested backgrounds for your analysis.

---

## Libraries Used
- `tidyverse`: Data manipulation and plotting.
- `imager`: Image processing and pixel extraction.
- `colorspace`: Color space conversions and utilities.
- `colorscience`: Advanced color science operations.

---

## Contribution
Contributions are welcome! Please submit a pull request or open an issue if you have ideas for improving the script or adding features.

---

## License
This project is licensed under the MIT License. See the `LICENSE` file for details.
