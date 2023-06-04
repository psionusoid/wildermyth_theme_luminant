library(png)

#whether to use the solid color method
solid <- TRUE

#the color to use for the solid color method (example: "#FFFF00")
solid_color <- "#00FFFF"

#what to add to the filename for this set of images
file_name_modifier <- paste0("_", paste0(strsplit(solid_color, split = "")[[1]][-1], collapse = ""))

all_files <- list.files()

#find original files
files <- all_files[c(grep("L.png", all_files),
                     grep("R.png", all_files),
                     grep("F.png", all_files),
                     grep("M.png", all_files))]

#strip extensions
files_no_ext <- tools::file_path_sans_ext(files)

#number of files
n_files <- length(files)

#function to convert to grayscale (requires alpha value)
rgb2gray <- function(rgb, method = "weighted"){
  if(method == "weighted"){
    weights <- c(.299,.587,.114)
    gray_val <- sum(rgb[1:3]*weights)
  } else if(method == "max"){
    gray_val <- max(rgb[1:3])
  } else if(method == "white"){
    gray_val <- 1
  }
  
  gray <- c(rep(gray_val,3),rgb[4])
}

#function to convert image to a solid color (so the image is formed by opacity)
rgb2solid <- function(image, color){
  #convert color code to rgb values between 0 and 1
  col_values <- as.vector(col2rgb(color)/255)
  #replace color values
  for(i in 1:3){
    image[,,i] <- col_values[i]
  }
  return(image)
}

for(i in seq_along(files)){
  image <- readPNG(files[i])
  if(solid){
    image_new <- rgb2solid(image, color = solid_color)
  } else{
    image_gray <- apply(image, MARGIN = c(1,2), FUN = rgb2gray, method = "white")
    #transpose array to be same as original
    image_new <- aperm(image_gray, perm = c(2,3,1))
  }
  #write out grayscale png
  writePNG(image = image_new, target = paste0(files_no_ext[i], file_name_modifier, ".png"))
}
