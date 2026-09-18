# Crop a photo into a circle: center-crop to a square, then make the corners
# transparent so the image displays as a round avatar. Writes a PNG (RGBA).
# Pure R (uses the `jpeg` and `png` packages); supports .jpg/.jpeg/.png input.
crop_circle <- function(input_path, output_path, feather = 1.5) {
  ext <- tolower(tools::file_ext(input_path))
  img <- if (ext %in% c("jpg", "jpeg")) {
    jpeg::readJPEG(input_path)
  } else if (ext == "png") {
    png::readPNG(input_path)
  } else {
    stop("Formato não suportado: ", ext)
  }

  # Normalise to an [h, w, 3] RGB array (drop alpha / expand grayscale).
  if (length(dim(img)) == 2L) {
    img <- array(rep(img, 3), dim = c(dim(img), 3))
  } else if (dim(img)[3] >= 4L) {
    img <- img[, , 1:3, drop = FALSE]
  }

  h <- dim(img)[1]
  w <- dim(img)[2]
  side <- min(h, w)
  row0 <- floor((h - side) / 2)
  col0 <- floor((w - side) / 2)
  square <- img[(row0 + 1):(row0 + side), (col0 + 1):(col0 + side), , drop = FALSE]

  # Circular alpha mask with a soft (feathered) edge to avoid jaggies.
  center <- (side + 1) / 2
  radius <- side / 2
  rows <- matrix(seq_len(side), nrow = side, ncol = side)
  cols <- matrix(seq_len(side), nrow = side, ncol = side, byrow = TRUE)
  dist <- sqrt((rows - center)^2 + (cols - center)^2)
  alpha <- pmin(pmax((radius - dist) / feather + 0.5, 0), 1)

  rgba <- array(0, dim = c(side, side, 4))
  rgba[, , 1:3] <- square
  rgba[, , 4] <- alpha
  png::writePNG(rgba, output_path)
  invisible(output_path)
}

# File name of the circular version of a photo (the convention used everywhere).
circular_photo_name <- function(image_file) {
  paste0(tools::file_path_sans_ext(image_file), "_circle.png")
}

# Make a circular PNG for every photo in `img_dir`, skipping any whose circular
# version is already up to date. Run once at startup so dropping a normal photo
# into the folder is enough — the app crops it automatically.
make_circular_photos <- function(img_dir = "www/img") {
  originals <- list.files(
    img_dir,
    pattern = "\\.(jpg|jpeg|png)$",
    ignore.case = TRUE,
    full.names = FALSE
  )
  originals <- originals[!grepl("_circle\\.png$", originals)]
  for (file in originals) {
    src <- file.path(img_dir, file)
    out <- file.path(img_dir, circular_photo_name(file))
    up_to_date <- file.exists(out) &&
      file.mtime(out) >= file.mtime(src)
    if (!up_to_date) crop_circle(src, out)
  }
  invisible(NULL)
}
