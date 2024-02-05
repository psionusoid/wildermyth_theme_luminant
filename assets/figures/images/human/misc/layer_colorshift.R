library(png)

#whether to use the solid color method
solid <- TRUE

#the color to use for the solid color method (example: "#FFFF00")
#solid_color <- "#FF2020" #red
#solid_color <- "#FFA500" #orange
#solid_color <- "#40FF40" #green
#solid_color <- "#00FFFF" #cyan
#solid_color <- "#6060FF" #blue
#solid_color <- "#FF40FF" #magenta
#solid_color <- "#202020" #shadowy
solid_color <- c("#FF2020", #red
                 "#FFA500", #orange
                 "#40FF40", #green
                 "#00FFFF", #cyan
                 "#6060FF", #blue
                 "#FF40FF", #magenta
                 "#202020", #shadowy
                 "#FFFFFF") #white
#how much to adjust the opacity for each color
alpha_scale_base <- 0.8 #base
alpha_scale_tint <- 0.8 #tint-able
alpha_scale <- c(0.8, #red
                 0.8, #orange
                 0.8, #green
                 0.8, #cyan
                 0.8, #blue
                 0.8, #magenta
                 0.8, #shadowy
                 0.8) #white

#what to add to the filename for these sets of images
file_name_modifier <- sapply(solid_color, FUN = function(x) paste0("_", paste0(strsplit(x, split = "")[[1]][-1], collapse = "")))

all_files <- list.files()

#find original files
files <- all_files[c(grep("L.png", all_files),
                     grep("R.png", all_files),
                     grep("tF.png", all_files),
                     grep("M.png", all_files))]
#find files of radiant layers for alpha scaling
files_radiant <- files[c(grep("R.png", all_files),
                             grep("ntF.png", all_files),
                             grep("ntM.png", all_files))]

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

#function to scale alpha values
scale_alpha <- function(image, alpha_scale){
  image[,,4] <- alpha_scale*image[,,4]
  #drop alpha to 1 if greater than 1
  image[,,4][image[,,4] >1] <- 1
}

#base color
for(i in seq_along(files)){
  image <- readPNG(files[i])
  if(solid){
    image_new <- image
  } else{
    image_gray <- apply(image, MARGIN = c(1,2), FUN = rgb2gray, method = "white")
    #transpose array to be same as original
    image_new <- aperm(image_gray, perm = c(2,3,1))
  }
  if(i %in% files_radiant){
    image_new <- scale_alpha(image_new, alpha_scale_base)
  }
  #write out png
  writePNG(image = image_new, target = paste0(files_no_ext[i], "_base.png"))
}
#tintable color
for(i in seq_along(files)){
  image <- readPNG(files[i])
  if(solid){
    image_new <- rgb2solid(image, color = "#FFFFFF")
  } else{
    image_gray <- apply(image, MARGIN = c(1,2), FUN = rgb2gray, method = "white")
    #transpose array to be same as original
    image_new <- aperm(image_gray, perm = c(2,3,1))
  }
  if(i %in% files_radiant){
    image_new <- scale_alpha(image_new, alpha_scale_tint)
  }
  #write out png
  writePNG(image = image_new, target = paste0(files_no_ext[i], "_tint.png"))
}
#other colors
for(k in seq_along(solid_color)){
  for(i in seq_along(files)){
    image <- readPNG(files[i])
    if(solid){
      image_new <- rgb2solid(image, color = solid_color[k])
    } else{
      image_gray <- apply(image, MARGIN = c(1,2), FUN = rgb2gray, method = "white")
      #transpose array to be same as original
      image_new <- aperm(image_gray, perm = c(2,3,1))
    }
    if(i %in% files_radiant){
      image_new <- scale_alpha(image_new, alpha_scale[k])
    }
    #write out png
    writePNG(image = image_new, target = paste0(files_no_ext[i], file_name_modifier[k], ".png"))
  }
}

