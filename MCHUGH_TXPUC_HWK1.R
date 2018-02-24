MCHUGH_TXPUC_HWK1 <- function(txpuc_month, txpuc_year){

  #enter month as three letter code, e.g. 'Jun'
  #enter year as last two digits, e.g. '07'
  
  #load libraries
  library(pdftools)
  library(xml2)
  library(rvest)

  #build url
  txpuc_url <- paste0("https://www.puc.texas.gov/industry/electric/rates/RESrate/rate", txpuc_year, "/", txpuc_month, txpuc_year, "Rates.pdf")
  print(txpuc_url)

  #download and parse pdf
  download.file(url = txpuc_url, destfile = paste0("MCHUGH_TXPUC_HWK1_",txpuc_month,"_",txpuc_year,".pdf"), mode = "wb")
  txpuc_txt <- pdf_text(paste0("MCHUGH_TXPUC_HWK1_",txpuc_month,"_",txpuc_year,".pdf"))
  txpuc_txt2 <- strsplit(txpuc_txt, "\n")
  txpuc_pgs <- length(txpuc_txt2)
  
  #create matrix
  x <- matrix(vector(), 0, 7, dimnames=list(c(), c('Plan', 'kWh500', 'kWh1000', 'kWh1500', 'kWh2000', 'year', 'month')))
  
  #for loops
  for(i in 1:txpuc_pgs){
    y <- matrix(vector(), 0, 5, dimnames=list(c(), c('Plan', 'kWh500', 'kWh1000', 'kWh1500', 'kWh2000')))
    page_lines <- length(txpuc_txt2[[i]])
    for(j in 1:page_lines){
      txpuc_line <- strsplit(txpuc_txt2[[i]][j], split = " ")
      txpuc_line <- txpuc_line[[1]][!txpuc_line[[1]] == ""]
      #if statements to pull all the lines with plans and usage info
      if(length(txpuc_line) == 5 & nchar(txpuc_line[2]) == 5 & nchar(txpuc_line[3]) == 5 & nchar(txpuc_line[4]) == 5 & nchar(txpuc_line[5]) == 5){
        y <- rbind(y, txpuc_line)
      } else if(length(txpuc_line) == 6 & nchar(txpuc_line[3]) == 5 & nchar(txpuc_line[4]) == 5 & nchar(txpuc_line[5]) == 5 & nchar(txpuc_line[6]) == 5){
        txpuc_line2 <- c(paste(txpuc_line[1], txpuc_line[2]), txpuc_line[3],txpuc_line[4],txpuc_line[5],txpuc_line[6])
        y <- rbind(y, txpuc_line2)
      } else if(length(txpuc_line) == 7 & nchar(txpuc_line[4]) == 5 & nchar(txpuc_line[5]) == 5 & nchar(txpuc_line[6]) == 5 & nchar(txpuc_line[7]) == 5){
        txpuc_line3 <- c(paste(txpuc_line[1], txpuc_line[2], txpuc_line[3]),txpuc_line[4],txpuc_line[5],txpuc_line[6],txpuc_line[7])
        y <- rbind(y, txpuc_line3)
      } else if(length(txpuc_line) == 8 & nchar(txpuc_line[5]) == 5 & nchar(txpuc_line[6]) == 5 & nchar(txpuc_line[7]) == 5 & nchar(txpuc_line[8]) == 5){
        txpuc_line4 <- c(paste(txpuc_line[1], txpuc_line[2], txpuc_line[3],txpuc_line[4]),txpuc_line[5],txpuc_line[6],txpuc_line[7],txpuc_line[8])
        y <- rbind(y, txpuc_line4)
      } else if(length(txpuc_line) == 9 & nchar(txpuc_line[6]) == 5 & nchar(txpuc_line[7]) == 5 & nchar(txpuc_line[8]) == 5 & nchar(txpuc_line[9]) == 5){
        txpuc_line5 <- c(paste(txpuc_line[1], txpuc_line[2], txpuc_line[3],txpuc_line[4],txpuc_line[5]),txpuc_line[6],txpuc_line[7],txpuc_line[8], txpuc_line[9])
        y <- rbind(y, txpuc_line5)
      } else if(length(txpuc_line) == 10 & nchar(txpuc_line[7]) == 5 & nchar(txpuc_line[8]) == 5 & nchar(txpuc_line[9]) == 5 & nchar(txpuc_line[10]) == 5){
        txpuc_line6 <- c(paste(txpuc_line[1], txpuc_line[2], txpuc_line[3],txpuc_line[4],txpuc_line[5],txpuc_line[6]),txpuc_line[7],txpuc_line[8], txpuc_line[9], txpuc_line[10])
        y <- rbind(y, txpuc_line6)
      }
    }
  }
  
  #create dataframe
  y <- as.data.frame(y) #convert
  y$year <- txpuc_year #append year
  y$month <- txpuc_month #append month
  x <- rbind(x,y) #append dataframe
  x <- as.data.frame(x) #convert
  rownames(x) <- NULL #cancel
  x = transform(x, kWh500 = as.numeric(as.character(kWh500)), kWh1000 = as.numeric(as.character(kWh1000)), kWh1500 = as.numeric(as.character(kWh1500)), kWh2000 = as.numeric(as.character(kWh2000)))

  #write csv
  write.csv(x, file = paste0("MCHUGH_TXPUC_HWK1_",txpuc_month,"_",txpuc_year,".csv"), row.names = FALSE)
}