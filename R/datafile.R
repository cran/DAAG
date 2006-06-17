"datafile" <-
function (file = "fuel") 
{
    if (file == "fuel") {
        cat("year  carbon", "1800   8", "1850   54", "1900   534", 
            "1950   1630", "2000   6611", file = "fuel.txt", 
            sep = "\n")
        print(paste("File fuel.txt is now in directory", getwd()))
    }
    if (file == "fuel.csv") {
        cat("year,carbon", "1800,8", "1850,54", "1900,534", "1950,1630", 
            "2000,6611", file = "fuel.csv", sep = "\n")
        print(paste("File fuel.csv is now in directory", getwd()))
    }
    if (file == "oneBadRow") {
        cat("10 9 17  # First of 7 lines", "11 13 1 6", "9 14 16", 
            "12 15 14", "8 15 15", "9 13 12", "7 14 18", file = "oneBadRow.txt", 
            sep = "\n")
        print(paste("File oneBadRow.txt is now in directory", 
            getwd()))
    }
    if (file == "scan-demo") {
        cat("First of 4 lines", "a 2 3", "b 11 13", "c 9 7", 
            file = "scan-demo.txt", sep = "\n")
        print(paste("File scan-demo.txt is now in directory", 
            getwd()))
    }
    if (file == "bostonc") {
         cat(bostonc[1:9], file="bostonc.txt", sep="\t", fill=TRUE) 
         cat("\n", file="bostonc.txt", sep="\t", append=TRUE) 
         cat(bostonc[-c(1:9)], file="bostonc.txt", sep="\t",fill=TRUE,append=TRUE) 
        print(paste("File bostonc.txt is now in directory", getwd()))
    }
    invisible()
}

