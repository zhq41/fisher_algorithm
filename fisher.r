#tugas implementasi fisher
library(matlib)

#data menggunakan iris
data1 <- iris
Y <- data1[,-5]
lbl <- as.numeric(iris[,5])

#iris ada empat fitur dan ada satu target (tiga kelas)
# setosa, virginica, versicolor

#step 1
# hitung mean dari tiap fitur terhadap masing-masing kelas
meanClass <- data.frame(u1= numeric(0), 
                     u2= numeric(0), 
                     u3= numeric(0),
                     u4= numeric(0))

#kelas setosa
meanClass <- rbind(meanClass, c(mean(data1[data1$Species=="setosa",1]),
                                mean(data1[data1$Species=="setosa",2]),
                                mean(data1[data1$Species=="setosa",3]),
                                mean(data1[data1$Species=="setosa",4])))

#kelas virginica
meanClass <- rbind(meanClass, c(mean(data1[data1$Species=="virginica",1]),
                                mean(data1[data1$Species=="virginica",2]),
                                mean(data1[data1$Species=="virginica",3]),
                                mean(data1[data1$Species=="virginica",4])))

#kelas versicolor
meanClass <- rbind(meanClass, c(mean(data1[data1$Species=="versicolor",1]),
                                mean(data1[data1$Species=="versicolor",2]),
                                mean(data1[data1$Species=="versicolor",3]),
                                mean(data1[data1$Species=="versicolor",4])))


#step 2 : hitung scatter matriks
# within-class scatter matrix (Sw)
 # hitung Si
   SiData <- data.frame(u1= numeric(0), 
                        u2= numeric(0), 
                        u3= numeric(0),
                        u4= numeric(0))
   #kelas setosa
   SiData <- rbind(SiData, c(sum((data1[data1$Species=="setosa",1]-meanClass[1,1])*t(data1[data1$Species=="setosa",1]-meanClass[1,1])),
                             sum((data1[data1$Species=="setosa",2]-meanClass[1,2])*t(data1[data1$Species=="setosa",2]-meanClass[1,2])),
                             sum((data1[data1$Species=="setosa",3]-meanClass[1,3])*t(data1[data1$Species=="setosa",3]-meanClass[1,3])),
                             sum((data1[data1$Species=="setosa",4]-meanClass[1,4])*t(data1[data1$Species=="setosa",4]-meanClass[1,4]))
                            )
                  )

   #kelas virginica
   SiData <- rbind(SiData, c(sum((data1[data1$Species=="virginica",1]-meanClass[2,1])*t(data1[data1$Species=="virginica",1]-meanClass[2,1])),
                             sum((data1[data1$Species=="virginica",2]-meanClass[2,2])*t(data1[data1$Species=="virginica",2]-meanClass[2,2])),
                             sum((data1[data1$Species=="virginica",3]-meanClass[2,3])*t(data1[data1$Species=="virginica",3]-meanClass[2,3])),
                             sum((data1[data1$Species=="virginica",4]-meanClass[2,4])*t(data1[data1$Species=="virginica",4]-meanClass[2,4]))
                            )
                   )
   
   #kelas versicolor
   SiData <- rbind(SiData, c(sum((data1[data1$Species=="versicolor",1]-meanClass[3,1])*t(data1[data1$Species=="versicolor",1]-meanClass[3,1])),
                             sum((data1[data1$Species=="versicolor",2]-meanClass[3,2])*t(data1[data1$Species=="versicolor",2]-meanClass[3,2])),
                             sum((data1[data1$Species=="versicolor",3]-meanClass[3,3])*t(data1[data1$Species=="versicolor",3]-meanClass[3,3])),
                             sum((data1[data1$Species=="versicolor",4]-meanClass[3,4])*t(data1[data1$Species=="versicolor",4]-meanClass[3,4]))
                            )
                  )
   
   # hitung Sw
   SwData <- c(sum(SiData[,1]),
               sum(SiData[,2]),
               sum(SiData[,3]),
               sum(SiData[,4]))
   
   
# between-class scatter matrix
  # hitung overall mean dari tiap fitur terhadap tiap kelas
   meanOverallClass <- data.frame(u1= numeric(0), u2= numeric(0), u3= numeric(0),u4= numeric(0))
   meanOverallClass <- rbind(meanOverallClass, c(mean(data1[,1]), mean(data1[,2]),
                                                 mean(data1[,3]), mean(data1[,4])))
   
  # hitung jumlah data per fitur untuk per kelas
   Ni <- c(nrow(data1[data1$Species=="setosa",]),
           nrow(data1[data1$Species=="virginica",]),
          nrow(data1[data1$Species=="versicolor",]))
   
   # hitung selisih meanClass-overallmeanclass untuk total tiap data kelas
   meanClass1 <- data.frame(u1= numeric(0), u2= numeric(0), u3= numeric(0),u4= numeric(0))
   meanClass1 <- rbind(meanClass1, (meanClass[1,]-meanOverallClass)*t(meanClass[1,]-meanOverallClass)*Ni[1])
   meanClass1 <- rbind(meanClass1, (meanClass[2,]-meanOverallClass)*t(meanClass[2,]-meanOverallClass)*Ni[2])
   meanClass1 <- rbind(meanClass1, (meanClass[3,]-meanOverallClass)*t(meanClass[3,]-meanOverallClass)*Ni[3])
   
   # hitung Sb
   SbData <- c(sum(meanClass1[,1]),
               sum(meanClass1[,2]),
               sum(meanClass1[,3]),
               sum(meanClass1[,4]))
   
# step 3 : cari dua fitur yang paling dominan
   dataSw <- as.matrix(rbind(SiData,SwData))
   dataSb <- as.matrix(rbind(meanClass1,SbData))
   Heigen <- eigen(chol2inv(dataSw)*dataSb)
   cek1 <- (chol2inv(dataSw)*dataSb)*Heigen$vectors
   cek2 <- Heigen$values*Heigen$vectors
   
   #pilih 2 fitur dg eigen value terbesar
   EigenValues <- data.frame(idx= numeric(0), values= numeric(0))
   for(i in 1:length(Heigen$values)){
     EigenValues <- rbind(EigenValues, c(i,Heigen$values[i]))
   }
   colnames(EigenValues) <- c("idx","values")
   EigenValues <- EigenValues[rev(order(EigenValues$values)),]
   
   Wdata <- Heigen$vectors[EigenValues[1,1],]
   Wdata <- cbind(Wdata,Heigen$vectors[EigenValues[2,1],])

# step 4 : plot data
   plot(Wdata[1:3,])
   asasa <- data1[,1:2]*Wdata[1:3,]
   plot(asasa[,EigenValues[1,1]],asasa[,EigenValues[2,1]])
   
   
