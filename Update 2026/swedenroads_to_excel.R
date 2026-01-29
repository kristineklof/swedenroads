#=================================================================#
#           Export to Excel
#=================================================================#

swedenroads_to_excel <- function(df,
                                 template_path,
                                 output_path,
                                 mapping) {
  stopifnot(is.data.frame(df))
  if (is.list(mapping)) mapping <- unlist(mapping, use.names = TRUE)
  stopifnot(is.character(mapping), !is.null(names(mapping)))
  
  sheetname <- openxlsx::getSheetNames(template_path)[1]
  
  tpl0 <- readxl::read_excel(template_path, sheet = sheetname, n_max = 0)
  tpl_cols <- names(tpl0)
  
  out <- as.data.frame(
    matrix(NA, nrow = nrow(df), ncol = length(tpl_cols)),
    stringsAsFactors = FALSE
  )
  names(out) <- tpl_cols
  
  for (src in names(mapping)) {
    tgt_header <- mapping[[src]]
    
    if (!tgt_header %in% tpl_cols) stop(sprintf("Template missing target header: '%s'", tgt_header))
    if (!src %in% names(df)) stop(sprintf("DataFrame missing source column: '%s'", src))
    
    out[[tgt_header]] <- df[[src]]
  }
  
  keep <- rowSums(!is.na(out)) > 0
  out <- out[keep, , drop = FALSE]
  
  openxlsx::write.xlsx(
    out,
    file = output_path,
    sheetName = sheetname,
    overwrite = TRUE
  )
  
  output_path
}

# df: your data.frame
# template_path: path to the template xlsx
# output_dir: folder where you want the 10 files
# mapping: named character vector (names = df cols, values = template headers)

export_swedenroads_split_10 <- function(df, template_path, output_dir, mapping,
                                        base_name = "swedenroads",
                                        n_parts = 10) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  n <- nrow(df)
  idx <- split(seq_len(n), cut(seq_len(n), breaks = n_parts, labels = FALSE))
  
  out_paths <- character(length(idx))
  for (i in seq_along(idx)) {
    part_df <- df[idx[[i]], , drop = FALSE]
    out_paths[i] <- file.path(output_dir, sprintf("%s_part%02d.xlsx", base_name, i))
    
    swedenroads_to_excel(
      df = part_df,
      template_path = template_path,
      output_path = out_paths[i],
      mapping = mapping
    )
  }
  
  out_paths
}

# Example usage:
# mapping <- c(source_col1="Excel Header 1", source_col2="Excel Header 2")
# paths <- export_swedenroads_split_10(df, "template.xlsx", "exports/", mapping)
# print(paths)