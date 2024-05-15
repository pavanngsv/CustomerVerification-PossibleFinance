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

### Download and unzip required files for milestone3
dropBoxURL =
  "https://www.dropbox.com/sh/rlrv8z10ahyhl8l/AADQs5cFZKxKHUVlyNeAkGjua?dl=1"
destFile = "downloadMile3.zip"
curl::curl_download(dropBoxURL,destFile)
zip::unzip(destFile, exdir = "Milestone3_Data")

### Download and unzip required test input files for milestone3
#testFileURL = readline(prompt = "Enter the sample input URL: ")
destFile = "sampleBankAcctInput.csv"
curl::curl_download(testFileURL,destFile)

liveCustomerList = read.csv(file.path("Milestone3_Data", "liveCustomerList.csv"))
liveBankAcct = read.csv(file.path("Milestone3_Data", "liveBankAcct.csv"))

custBankInfo = inner_join(liveCustomerList, liveBankAcct, by = c("firstName", "lastName"))

sampleBankAcctInput = read.csv("sampleBankAcctInput.csv")

flagIdentify = left_join(sampleBankAcctInput, custBankInfo, by=c("custID" = "custID", "loginAcct"="bankAcctID"))
flagIdentify$rightAcctFlag = ifelse(!is.na(flagIdentify$firstName) & !is.na(flagIdentify$lastName), 1, 0)

flagIdentify = flagIdentify %>% select(custID,rightAcctFlag)

outputFileName = sub(".*/", "", testFileURL)  # Remove everything before the last '/'
outputFileName = sub("\\?.*", "", outputFileName)
write.csv(flagIdentify, file = outputFileName, row.names = FALSE)

