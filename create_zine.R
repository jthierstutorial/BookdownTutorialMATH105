install.packages("pdftools")
install.packages("qpdf")
library(pdftools)
library(qpdf)
setwd("~/Documents/School/MATH 105/Bookdown Tutorial/docs")
# Had to make sure the working directory was set to where _main.pdf is for the project,


# This function ensures the PDF has pages as a multiple of 4
ensure_multiple_of_4 <- function(pdf_path) {
  total_pages <- pdf_info(pdf_path)$pages
  blanks_needed <- (4 - total_pages %% 4) %% 4
  
  if (blanks_needed > 0) {
    blank_pdf <- tempfile(fileext = ".pdf")
    pdf_text <- tempfile(fileext = ".pdf")
    
    # Need to create a single blank PDF page to use
    pdf(file = blank_pdf, width = 8.5, height = 11)
    dev.off()
    
    # Appends the blank page to the original PDF
    pdf_combine(c(pdf_path, rep(blank_pdf, blanks_needed)), output = pdf_text)
    return(pdf_text)
  }
  
  return(pdf_path)
}

# Function to calculate zine page order (still a work in progress)
calculate_zine_order <- function(total_pages) {
  front <- c()
  back <- c()
  
  for (i in seq(1, total_pages / 4)) {
    front <- c(front, total_pages - (2 * i - 1) + 1, 2 * i - 1, 
               total_pages - (2 * i) + 1, 2 * i)
    back <- c(back, 2 * i - 1 + 1, total_pages - (2 * i - 1),
              2 * i, total_pages - (2 * i - 1 + 1))
  }
  
  list(front = front, back = back)
}

# Path to your _main.pdf file
pdf_path <- "_main.pdf"

# Ensures the PDF has pages as a multiple of 4
pdf_path <- ensure_multiple_of_4(pdf_path)

# Gets the total number of pages in the adjusted PDF
total_pages <- pdf_info(pdf_path)$pages

# Calculates the zine page order
page_order <- calculate_zine_order(total_pages)

# The following function generates the LaTeX content for the zine
zine_latex <- function(page_order, pdf_path) {
  front_pages <- paste(page_order$front, collapse = ",")
  back_pages <- paste(page_order$back, collapse = ",")
  
  paste0(
    "\\documentclass{article}\n",
    "\\usepackage{pdfpages}\n",
    "\\usepackage[paperwidth=8.5in, paperheight=11in]{geometry}\n",
    "\\geometry{left=0in, right=0in, top=0in, bottom=0in}\n",
    "\\begin{document}\n",
    "\\includepdf[pages={", front_pages, "}, nup=2x2]{", pdf_path, "}\n",
    "\\includepdf[pages={", back_pages, "}, nup=2x2]{", pdf_path, "}\n",
    "\\end{document}\n"
  )
}

# Then we write the .tex file
latex_content <- zine_latex(page_order, pdf_path)
writeLines(latex_content, "zineformat.tex")

# Compile the LaTeX file using pdflatex
system("pdflatex zineformat.tex")

#The file is then outputted as zineformat.pdf when this script is run.