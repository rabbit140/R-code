## NEED TO NOT OVERWRITE DATA IN TARGET COLUMNS!

# Make "n/a" strings NA values

for (i in 1:ncol(crunch)){
      crunch[,i] <- gsub("n/a", NA, crunch[,i])
}

# Emails tidy-up (ncol = 10)

#Exclude "Email" column from processing
ncol <- seq(1,ncol(crunch))[-10]

for(i in ncol){
      crunch[grep("@",crunch[,i]),"Email"] <- crunch[grep("@",crunch[,i]),i]
      #Remove emails from improper columns
      crunch[grep("@",crunch[,i]), i] <- NA
}

# Phone numbers tidy-up

#Exclude "Phone.Number" column from processing
ncol <- seq(1,ncol(crunch))[-9]

for(i in ncol){
      #Rows matching regex
      numberRows <- grep("\\+?\\.?\\-?\\(?\\d+?\\)?", crunch[,i])
      #Rows matching characters (leave them out)
      charRows <- grep("[a-zA-Z]", crunch[,i])
      #Exclude rows with characters from numberRows
      numberRows <- numberRows[-charRows]
      crunch[grep("\\+?\\.?\\-?\\(?\\d+?\\)?",crunch[,i]),"Phone.Number"] <- crunch[grep("\\+?\\.?\\-?\\(?\\d+?\\)?",crunch[,i]),i]
      #Remove phone numbers from improper columns
      crunch[numberRows, i] <- NA
}