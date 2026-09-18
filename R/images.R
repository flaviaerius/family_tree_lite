# Helpers for showing person photos (the `image_file` column) inside the nodes.
#
# Photos are never served as static files. Each person's photo is uploaded per
# session (see resolve_uploaded_images() in R/uploaded_images.R), cropped to a
# circle, and stored directly in the `image_file` column as a base64 `data:` URI.
# That URI is embedded in the plot JSON — no public URL, no separate HTTP request.

# Read an image file and return it as a base64 `data:` URI.
to_base64_uri <- function(path) {
  ext <- tolower(tools::file_ext(path))
  mime <- switch(ext,
    jpg = , jpeg = "image/jpeg",
    png  = "image/png",
    gif  = "image/gif",
    webp = "image/webp",
    "image/jpeg"
  )
  paste0("data:", mime, ";base64,", base64enc::base64encode(path))
}

# By the time a layout is built, `image_file` already holds a data URI (or a full
# URL). Pass it straight through to plotly.
image_source <- function(image_file) {
  trimws(image_file)
}

has_image <- function(image_file) {
  !is.na(image_file) & nzchar(trimws(image_file))
}

# Build the list of plotly layout-image specs, one per node that has a photo.
# `base_diameter` is the photo diameter (in data units) for a node whose marker
# `size` equals `ref_size`; other nodes scale proportionally. Drawn above the
# traces so the photo is visible; node clicks still register because plotly
# captures them on its drag layer, which sits above the images.
build_node_images <- function(layout, base_diameter, ref_size) {
  with_photo <- layout[has_image(layout$image_file), , drop = FALSE]
  if (nrow(with_photo) == 0) {
    return(list())
  }
  node_sizes <- if ("size" %in% names(with_photo)) with_photo$size else
    rep(ref_size, nrow(with_photo))
  lapply(seq_len(nrow(with_photo)), function(i) {
    diameter <- base_diameter * (node_sizes[i] / ref_size)
    list(
      source = image_source(with_photo$image_file[i]),
      xref = "x", yref = "y",
      x = with_photo$x[i], y = with_photo$y[i],
      sizex = diameter, sizey = diameter,
      xanchor = "center", yanchor = "middle",
      sizing = "contain", layer = "above"
    )
  })
}
