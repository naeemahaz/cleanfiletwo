cleanme <- function(dataname){

                    #SAVE THE ORIGINAL FILE
                    oldfile <- write.csv(dataname, file = "oldfile.csv", row.names = FALSE, na = "")

                    #CLEAN THE FILE. SAVE THE CLEAN. IMPORT THE CLEAN FILE. CHANGE THE TO A DATAFRAME.
                    cleandata <- dataname[complete.cases(dataname),]
                    cleanfile <- write.csv(cleandata, file = "cleanfile.csv", row.names = FALSE, na = "")
                    cleanfileread <- read.csv(file = "cleanfile.csv")
                    cleanfiledata <- as.data.frame(cleanfileread)

                    #SUBSETTING THE DATA TO TYPES

                    logicmeint <- cleanfiledata[,sapply(cleanfiledata,is.integer)]
                    logicmedouble <- cleanfiledata[,sapply(cleanfiledata,is.double)]
                    logicmefactor <- cleanfiledata[,sapply(cleanfiledata,is.factor)]
                    logicmenum <- cleanfiledata[,sapply(cleanfiledata,is.numeric)]
                    #logicmelogic <- cleanfiledata[,sapply(cleanfiledata,is.logical)]
                    mainlogicmefactors <- cleanfiledata[,sapply(cleanfiledata,is.factor) | sapply(cleanfiledata,is.numeric)]

                    #VIEW ALL FILES
                    View(cleanfiledata)
                    View(logicmeint)
                    View(logicmedouble)
                    View(logicmenum)
                    View(mainlogicmefactors)

                    #ANALYTICS OF THE MAIN DATAFRAME
                    cleansum <- summary(cleanfiledata)
                    print(cleansum)

                    cleandec <- describe(cleanfiledata)
                    print(cleandec)


                    #ANALYTICS OF THE FACTOR DATAFRAME
                    factorsum <- summary(logicmefactor)
                    print(factorsum)

                    factordec <- describe(logicmefactor)
                    print(factordec)

                    #ANALYTICS OF THE NUMBER DATAFRAME
                    numbersum <- summary(logicmenum)
                    print(numbersum)



                    numberdec <- describe(logicmefactor)
                    print(numberdec)

                    #CONVERT MAT
                    matrixdouble <- as.matrix(logicmedouble)
                    View(matrixdouble)
                    matrixnum <- as.matrix(logicmenum)
                    View(matrixnum)
                    print(summary.matrix(matrixdouble))
                    print(summary.matrix(matrixnum))





                    #HISTOGRAM PLOTS OF ALL TYPES
                    barplot(matrixdouble)
                    barplot(matrixnum)
                    hist(logicmedouble)
                    hist(logicmenum)
                    plot(logicmefactor)

                    save(cleanfiledata,matrixnum, matrixdouble, mainlogicmefactors, logicmeint, logicmedouble, logicmefactor, logicmenum, numberdec, numbersum, factordec, factorsum, cleandec, oldfile, cleandata, cleanfile, cleanfileread,   file = "cleanmework.RData")
                    load("cleanmework.RData")




}
