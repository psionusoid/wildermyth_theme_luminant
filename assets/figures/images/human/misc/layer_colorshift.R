library(png)

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

for(i in seq_along(files)){
  image <- readPNG(files[i])
  image_gray <- apply(image, MARGIN = c(1,2), FUN = rgb2gray, method = "white")
  #transpose array to be same as original
  image_gray <- aperm(image_gray, perm = c(2,3,1))
  #write out grayscale png
  writePNG(image = image_gray, target = paste0(files_no_ext[i], "_tint.png"))
}
