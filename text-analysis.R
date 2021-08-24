### text files

file_vector <- list.files(path = "Data")
file_vector %>% head()
grepl(".pdf", file_vector)

## select only pdf
pdf_list <- file_vector[grepl("pdf", file_vector)]
head(pdf_list)
pdf_text("Data/BUSINESSCENTER.pdf")
