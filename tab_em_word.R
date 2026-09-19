#install.packages(c("officer", "readxl"))
library(officer)
library(readxl)

# Read the data from the Excel file
#dati_excel <- read_excel("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\Error_Measures_cripto.xlsx")
dati_excel <- read_excel("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\Error_Measures_Fiat.xlsx")

# Create a new Word document
doc <- read_docx()

# Add a table to the Word document using the Excel data
doc <- doc %>%
  body_add_table(value = dati_excel, style = "table_template" ) 

# Save the Word document

#print(doc, target = "C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\em_cripto.docx")
print(doc, target = "C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\em_fiat.docx")

