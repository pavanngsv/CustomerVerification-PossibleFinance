#### Install Zip package when running on the RStudio for the first time.
###install.packages('zip')
###install.packages('dplyr')
###install.packages('gridExtra')
library(dplyr)
library(ggplot2)
library(gridExtra)

process_text <- function(input_text) {
  cat("Processing text:", input_text, "\n")
}

input_text <- "default_value" 
if (!is.null(storedText())) {
  input_text <- storedText()
}

process_text(input_text)
testFileURL = input_text

### Download and unzip required files for milestone4
dropBoxURL =
  "https://www.dropbox.com/sh/mqpdo3m9tvl9q86/AAD4Qe8Nip5s7o2Ty-DMbl4Ta?dl=1"
destFile = "downloadMile4.zip"
curl::curl_download(dropBoxURL,destFile)
zip::unzip(destFile, exdir = "Milestone4_Data")

### Download and unzip required test input files for milestone4
#testFileURL = readline(prompt = "Enter the sample input URL: ")
destFile = "bankAcctsForViz.csv"
curl::curl_download(testFileURL,destFile)

#import the required data into RStudio
startBalance = read.csv(file.path("Milestone4_Data", "startBalance.csv"))
bankTransac = read.csv(file.path("Milestone4_Data", "bankTransactions.csv"))
reqAcct = read.csv("bankAcctsForViz.csv")

#Using join method, get the transaction data of the required bank accounts
reqAcctStartBalance = left_join(reqAcct, startBalance, by = c("bankAcctID"))
reqAcctTransac = left_join(reqAcct, bankTransac, by = c("bankAcctID"))

#Update the column names and bind the start balance and account transaction data
names(reqAcctTransac)[names(reqAcctTransac) == "transAmount"] = "amt"
reqAcctBalances = bind_rows(reqAcctStartBalance, reqAcctTransac)

#Summarize the transactions and get the cumulative end of the day account balance
eodBalance = reqAcctBalances %>% arrange(date) %>% group_by(date, bankAcctID) %>% summarise(eodBalances= sum(amt))
eodBalanceCum = eodBalance %>% group_by(bankAcctID) %>% mutate(cummulativeSum = cumsum(eodBalances))

#filter the data set to see the EOD balances according to the required time period
eodBalance2020 = eodBalanceCum %>% filter(date >= as.Date("2020-03-01") & date <= as.Date("2020-04-30"))
eodBalance2020$date = as.Date(eodBalance2020$date)
bankAcctID_value <- unique(eodBalance2020$bankAcctID)
eodBalance2020$bankAcctID = as.factor(eodBalance2020$bankAcctID)

#
positiveTransac = reqAcctBalances %>% filter(amt > 0) %>% group_by(date, bankAcctID) %>% summarise(positive_transactions = sum(amt))
positiveTransac2020 = positiveTransac %>% filter(date >= as.Date("2020-03-01") & date <= as.Date("2020-04-30")) %>% filter(positive_transactions > 200)
positiveTransac2020$date = as.Date(positiveTransac2020$date)
positiveTransac2020$bankAcctID = as.factor(positiveTransac2020$bankAcctID)

#Create a vector of colors to assign to different bank accounts projected in the plot
my_colors = c("purple", "darkgreen", "darkorange")

annotations <- data.frame(
  annotation_text = c("Significant Transactions over $200")
)

eodBalancesPlot = eodBalance2020 %>% ggplot(aes(x = date, y = cummulativeSum, color=bankAcctID)) +
  geom_step() +
  labs(title = paste("EOD Balances by Account between March - April 2020"),
       x = "Date",
       y = "EOD Balance")+
  scale_color_manual(values = my_colors)+
  labs(color = "ACCOUNTS") +
  theme_minimal()

positiveTransacPlot = positiveTransac2020 %>% ggplot(aes(x = date, y = positive_transactions, fill=bankAcctID)) +
  geom_bar(stat = "identity",alpha = 0.7, width = 0.5) +
  labs(title = paste("Significant Transactions by Account between March - April 2020"),
       x = "Date",
       y = "Total Daily Deposits")+
  geom_text(aes(label = format(date, "%a %m/%d")), vjust = -0.5, size = 3, color = "black") +
  geom_text(aes(label = paste0("$", positive_transactions)), vjust = -2, size = 3, color = "black") +
  facet_wrap(~bankAcctID, scales = "free_y", ncol = 1, strip.position = "top") +
  scale_fill_manual(values = my_colors)+
  labs(color = "ACCOUNTS") +
  ylim(0, max(positiveTransac2020$positive_transactions)+800) +
  geom_hline(yintercept = 200, linetype = "dashed", color = "black") +
  geom_text(aes(x = as.Date("2020-03-01"), y = 200, label = "Significant Transactions over $200"), hjust = -0.1, vjust = -0.5, size = 4) +
  theme_bw()

outputPlot = grid.arrange(eodBalancesPlot, positiveTransacPlot, nrow = 2)


# Remove everything before the last '/'
outputFileName = sub(".*/([0-9]+)\\.csv.*", "\\1", testFileURL)
outputFileName = paste0(outputFileName, ".pdf")
ggsave(outputFileName,outputPlot,height=12, width=20)
