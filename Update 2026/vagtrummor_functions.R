# Shapefile-safe, unique DBF field names (unique AFTER shortening & uppercasing)
make_shp_names <- function(x, max_len = 10, sep = "_") {
  stopifnot(is.character(x), length(max_len) == 1, max_len >= 1)
  
  x[is.na(x)] <- ""
  x[x == ""] <- "X"
  
  # "Launder" similarly to typical Shapefile/DBF constraints
  safe <- gsub("[^A-Za-z0-9_]+", "_", x)
  safe <- gsub("_+", "_", safe)
  safe <- gsub("^_+|_+$", "", safe)
  safe[safe == ""] <- "X"
  safe <- toupper(safe)
  
  # Ensure starts with a letter (common constraint)
  starts_ok <- grepl("^[A-Z]", safe)
  safe[!starts_ok] <- paste0("X", safe[!starts_ok])
  
  # Initial truncate
  base <- substr(safe, 1, max_len)
  out  <- base
  
  seen <- new.env(parent = emptyenv())
  
  for (i in seq_along(out)) {
    key <- out[i]
    if (!exists(key, envir = seen, inherits = FALSE)) {
      assign(key, 1L, envir = seen)
      next
    }
    
    k <- get(key, envir = seen, inherits = FALSE)
    repeat {
      suffix <- paste0(sep, k)               # _1, _2, ...
      keep <- max_len - nchar(suffix)
      if (keep < 1) {
        candidate <- substr(paste0("X", k), 1, max_len)
      } else {
        candidate <- paste0(substr(base[i], 1, keep), suffix)
      }
      
      if (!exists(candidate, envir = seen, inherits = FALSE)) {
        out[i] <- candidate
        assign(candidate, 1L, envir = seen)
        break
      }
      k <- k + 1L
    }
    
    assign(key, k + 1L, envir = seen) # update next suffix for this key
  }
  
  out
}

unique_short_names <- function(x, max_len = 10, sep = "_", make_safe = TRUE) {
  stopifnot(is.character(x), length(max_len) == 1, max_len >= 1)
  
  # Coerce empties / NAs
  x[is.na(x)] <- ""
  x[x == ""] <- "X"
  
  if (make_safe) {
    # Basic "DBF-safe-ish" normalization:
    # - Replace non-alphanumeric with "_"
    # - Collapse multiple underscores
    # - Trim underscores
    x <- gsub("[^A-Za-z0-9]+", "_", x)
    x <- gsub("_+", "_", x)
    x <- gsub("^_+|_+$", "", x)
    x[x == ""] <- "X"
    
    # Ensure starts with a letter (common restriction in some GIS stacks)
    starts_ok <- grepl("^[A-Za-z]", x)
    x[!starts_ok] <- paste0("X", x[!starts_ok])
  }
  
  # Initial shorten
  base <- substr(x, 1, max_len)
  
  out <- base
  seen <- new.env(parent = emptyenv())
  
  for (i in seq_along(out)) {
    key <- out[i]
    if (!exists(key, envir = seen, inherits = FALSE)) {
      assign(key, 0L, envir = seen)
      next
    }
    
    # Need to create a unique variant with suffix
    n <- get(key, envir = seen, inherits = FALSE) + 1L
    assign(key, n, envir = seen)
    
    repeat {
      suffix <- paste0(sep, n)
      keep <- max_len - nchar(suffix)
      if (keep < 1) {
        # Fallback: no room for base + sep + n -> just use numeric-ish name
        candidate <- substr(paste0("X", n), 1, max_len)
      } else {
        candidate <- paste0(substr(base[i], 1, keep), suffix)
      }
      
      if (!exists(candidate, envir = seen, inherits = FALSE)) {
        out[i] <- candidate
        assign(candidate, 0L, envir = seen)
        break
      }
      n <- n + 1L
    }
  }
  
  out
}