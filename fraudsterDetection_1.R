#### Install Zip package when running on the RStudio for the first time.
###install.packages('zip')
###install.packages('dplyr')
library(dplyr)

process_text <- function(input_text) {
  cat("Processing text:", input_text, "\n")
}

input_text <- "default_value" 
if (!is.null(storedText())) {
  input_text <- storedText()
}

process_text(input_text)
testFileURL = input_text

### Download and unzip required files for milestone2
dropBoxURL =
  "https://www.dropbox.com/sh/g4njljbb299t0tp/AAAYIwsvBoYahdiAAuawngpGa?dl=1"
destFile = "downloadMile2.zip"
curl::curl_download(dropBoxURL,destFile)
zip::unzip(destFile, exdir = "Milestone2_Data")

### Download and unzip required test input files for milestone2
#testFileURL = readline(prompt = "Enter the sample input URL: ")
inputDestFile = "sampleFraudTestInput.zip"
curl::curl_download(testFileURL,inputDestFile)
zip::unzip(destFile, exdir = "Milestone2_InputData")

file_names = list.files("Milestone2_InputData")
file_names_without_extension = sub("\\..*", "", file_names)
inputCustID = data.frame(custID = file_names_without_extension)

liveCustomerList = read.csv(file.path("Milestone2_Data", "liveCustomerList.csv"))

merged_Data = merge(inputCustID, liveCustomerList, by = "custID", all.x = TRUE)
merged_Data$fullName = paste(merged_Data$firstName, merged_Data$lastName, sep= " ") %>% toupper()

liveFraudList = read.csv(file.path("Milestone2_Data", "liveFraudList.csv"))
liveFraudList$fullName = paste(liveFraudList$firstName, liveFraudList$lastName, sep= " ")

commonData = merged_Data %>% left_join(liveFraudList, by = 'fullName')

# Filter out rows with custID matching the extra file names
extra_files = c("liveCustomerList", "liveFraudList", 
                "sampleFraudSolution", "sampleFraudSolutionX", "sampleFraudTestInput")
commonData = commonData[!commonData$custID %in% extra_files, ]

commonData$fraudster = ifelse(!is.na(commonData$firstName.y), 1, 0)

commonData = commonData %>% select(custID,fraudster)

outputFileName = sub(".*/", "", testFileURL)  # Remove everything before the last '/'
outputFileName = sub("\\?.*", "", outputFileName)  # Remove everything after '?'
outputFileName = sub("\\..*", "", outputFileName)
outputFileName = paste0(outputFileName,".csv")
write.csv(commonData, file = outputFileName, row.names = FALSE)
