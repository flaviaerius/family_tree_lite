# Resolve the family's `image_file` references against a per-session folder of
# uploaded photos. For each person whose `image_file` name matches an uploaded
# file, the photo is cropped to a circle (crop_circle, in R/crop_circle.R) and the
# `image_file` column is replaced by a base64 `data:` URI (to_base64_uri, in
# R/images.R). People without a matching photo get NA and simply render without
# an image. Nothing is written outside `work_dir`, which is unique to the session
# and deleted when the session ends.
#
# `uploads` is the data frame from a Shiny fileInput (columns `name`, `datapath`).
# With a folder upload (webkitdirectory) `name` arrives as "MyFolder/joao.jpg", so
# matching is done on the basename, case-insensitively.
resolve_uploaded_images <- function(family, uploads, work_dir) {
  if (is.null(uploads) || nrow(uploads) == 0) {
    return(family)
  }
  lookup <- stats::setNames(uploads$datapath, tolower(basename(uploads$name)))
  family$image_file <- vapply(
    family$image_file,
    function(ref) {
      if (is.na(ref) || !nzchar(trimws(ref))) {
        return(NA_character_)
      }
      key <- tolower(basename(trimws(ref)))
      if (!key %in% names(lookup)) {
        return(NA_character_) # no matching uploaded photo
      }
      src <- lookup[[key]]
      circ <- tempfile(fileext = ".png", tmpdir = work_dir)
      tryCatch(
        {
          crop_circle(src, circ)
          to_base64_uri(circ)
        },
        error = function(e) NA_character_
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
  family
}
