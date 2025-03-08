# libraries we need
libs <- c(
  "tidyverse", "R.utils",
  "httr", "sf", "stars",
  "rayshader"
)

# install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

# load libraries
invisible(lapply(libs, library, character.only = T))

### 1. LOAD & UNZIP DATA
### ------------------------
file_path <- "C:/MyFILES/data_visualization/kontur_population_IN_20220630.gpkg.gz"
unzipped_file <- gsub(".gz", "", file_path)

if (!file.exists(unzipped_file)) {
  R.utils::gunzip(file_path, remove = F)
}

### 2. LOAD DATA
### -------------
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"
pop_sf <- sf::st_read(unzipped_file) |> sf::st_transform(crs = crsLAEA)

ggplot() +
  geom_sf(
    data = pop_sf,
    color = "grey10", fill = "grey10"
  )

### 3. SHP TO RASTER
### ----------------

bb <- sf::st_bbox(pop_sf)

determine_raster_size <- function() {
  height <- sf::st_distance(
    sf::st_point(c(bb[["xmin"]], bb[["ymin"]])),
    sf::st_point(c(bb[["xmin"]], bb[["ymax"]]))
  )
  width <- sf::st_distance(
    sf::st_point(c(bb[["xmin"]], bb[["ymin"]])),
    sf::st_point(c(bb[["xmax"]], bb[["ymin"]]))
  )
  
  if (height > width) {
    height_ratio <- 1
    width_ratio <- width / height
  } else {
    width_ratio <- 1
    height_ratio <- height / width
  }
  
  return(list(width_ratio, height_ratio))
}

size <- 3000
ratios <- determine_raster_size()
width <- round((size * ratios[[1]]), 0)
height <- round((size * ratios[[2]]), 0)

pop_rast <- stars::st_rasterize(
  pop_sf |> dplyr::select(population, geom),
  nx = width, ny = height
)

plot(pop_rast)

pop_mat <- pop_rast |> as("Raster") |> rayshader::raster_to_matrix()

cols <- rev(c(
  "#0b1354", "#283680",
  "#6853a9", "#c863b3"
))
texture <- grDevices::colorRampPalette(cols)(256)

# Create the initial 3D object
pop_mat |>
  rayshader::height_shade(texture = texture) |>
  rayshader::plot_3d(
    heightmap = pop_mat,
    solid = F,
    soliddepth = 0,
    zscale = 15,
    shadowdepth = 0,
    shadow_darkness = .95,
    windowsize = c(800, 800),
    phi = 65,
    zoom = .65,
    theta = -30,
    background = "white"
  )

# Adjust the view
rayshader::render_camera(phi = 75, zoom = .7, theta = 0)

rayshader::render_highquality(
  filename = "population_density_3D.png",
  preview = T,
  light = T,
  lightdirection = 225,
  lightaltitude = 60,
  lightintensity = 400,
  interactive = F,
  width = width, height = height
)

