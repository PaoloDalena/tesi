create_txt_tesi <- function(path_to_pdf_dir, path_to_txt_dir) {
  
  # don't waste time if java is not installed
  if (corecage::check_java() == FALSE) {
    stop(
      "Java must be installed and available on your system!"
    )
  } else {}
  
  # checks for missing
  if (missing(path_to_pdf_dir)) {
    stop(
      "A path to the directory where the pdf files are stored must be provided"
    )
  } else {}
  
  if (missing(path_to_txt_dir)) {
    if (interactive()) {
      same <- readline(
        "A path to the directory where you want to store the txt files hasn't
        been provided. Do you want to store the txt files in the same directory
        of pdf files? This may not be a good idea. [Y/N] "
      )
      if (same == "Y") {
        path_to_txt_dir <- path_to_pdf_dir
      } else {
        stop(
          "A path to the directory where you want to store the txt files
          extracted from the pdf files must be provided"
        )
      }
      } else {
        stop(
          "A path to the directory where you want to store the txt files
          extracted from the pdf files must be provided."
        )
    }
      } else {}
  
  # checks for existance of directories
  if (!file.exists(path_to_pdf_dir)) {
    stop(" The pdfs directory doesn't exist.")
  } else {}
  
  if (!file.exists(path_to_txt_dir)) {
    if (interactive()) {
      new <- readline("The output txt directory doesn't exists.
                      Do you want to create a new empty one? [Y/N]")
      if (new == "Y") {
        dir.create(path_to_txt_dir)
      } else {
        stop("The output txt directory doesn't exist.")
      }
    } else {
      stop("The output txt directory doesn't exist.")
    }
}
  
  # need vector with pdf files
  pdf_names <- corecage::pdf_file_names(path_to_pdf_dir)
  
  # have to create the string to send to the cmd in order to use pdfact.jar
  for (i in seq_along(pdf_names)) {
    system(stringr::str_c(
      "java -jar pdfact.jar --format txt --unit words ",
      path_to_pdf_dir, "//", pdf_names[[i]],
      " ",
      path_to_txt_dir, "//", pdf_names[[i]], ".txt"
    ))
  }
}
