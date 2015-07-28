rm(list = ls())
library(igraph)

if(interactive()){
  ############################# cutted ############################# 
  pathInC <- "./../../../data/toyData/results/5_Diffusion_closeness/cutted/diffusion_controls.csv"
  pathInP <- "./../../../data/toyData/results/5_Diffusion_closeness/cutted/diffusion_patients.csv"
  
  c.data <- read.csv(pathInC)
  p.data <- read.csv(pathInP)
  
  m_mean1 <- matrix(ncol = 2, nrow = 3)
  colnames(m_mean1) <- c("controls", "patients")
  rownames(m_mean1) <- c("time", "strong", "weak")
  for(i in 1:3){ # controls patients
      m_mean1[i,1] <- mean(c.data[,i+1])
      m_mean1[i,2] <- mean(p.data[,i+1])
  }
 
  m_var1 <- matrix(ncol = 2, nrow = 3)
  colnames(m_var1) <- c("controls", "patients")
  rownames(m_var1) <- c("time", "strong", "weak")
  for(i in 1:3){ # controls patients
    m_var1[i,1] <- var(c.data[,i+1])
    m_var1[i,2] <- var(p.data[,i+1])
  }
  
  m_mean1
  m_var1
  
  
  ############################# t test cutted ############################# 
  pathInC <- "./../../../data/toyData/results/5_Diffusion_closeness/t_test_cutted/diffusion_controls.csv"
  pathInP <- "./../../../data/toyData/results/5_Diffusion_closeness/t_test_cutted/diffusion_patients.csv"
  
  c.data <- read.csv(pathInC, header=F)
  p.data <- read.csv(pathInP, header=F)

  
  m_mean2 <- matrix(ncol = 2, nrow = 3)
  colnames(m_mean2) <- c("controls", "patients")
  rownames(m_mean2) <- c("time", "strong", "weak")
  for(i in 1:3){ # controls patients
    m_mean2[i,1] <- mean(c.data[,i+1])
    m_mean2[i,2] <- mean(p.data[,i+1])
  }
  
  m_var2 <- matrix(ncol = 2, nrow = 3)
  colnames(m_var2) <- c("controls", "patients")
  rownames(m_var2) <- c("time", "strong", "weak")
  for(i in 1:3){ # controls patients
    m_var2[i,1] <- var(c.data[,i+1])
    m_var2[i,2] <- var(p.data[,i+1])
  }

  sd2 <- matrix(ncol = 2, nrow = 3)
  colnames(sd2) <- c("controls", "patients")
  rownames(sd2) <- c("time", "strong", "weak")
  for(i in 1:3){ # controls patients
    sd2[i,1] <- sd(c.data[,i+1])
    sd2[i,2] <- sd(p.data[,i+1])
  } 
  m2 <- t(as.matrix(sd2))
  barplot(m2, col=c("darkblue","red"), legend = rownames(m1), ylim = c(0,100), beside = TRUE)
  m2 <- t(as.matrix(m_mean2))
  barplot(m2, col=c("darkblue","red"), legend = rownames(m1), ylim = c(0,400), beside = TRUE)
  
  m_mean2
  m_var2
  
  
  ############################# MST ############################# 
  pathInC <- "./../../../data/toyData/results/5_Diffusion_closeness/t_test_MST/diffusion_controls.csv"
  pathInP <- "./../../../data/toyData/results/5_Diffusion_closeness/t_test_MST/diffusion_patients.csv"
  
  c.data <- read.csv(pathInC)
  p.data <- read.csv(pathInP)
  
  m_mean3 <- matrix(ncol = 2, nrow = 3)
  colnames(m_mean3) <- c("controls", "patients")
  rownames(m_mean3) <- c("time", "strong", "weak")
  for(i in 1:3){ # controls patients
    m_mean3[i,1] <- mean(c.data[,i+1])
    m_mean3[i,2] <- mean(p.data[,i+1])
  }
  
  m_var3 <- matrix(ncol = 2, nrow = 3)
  colnames(m_var3) <- c("controls", "patients")
  rownames(m_var3) <- c("time", "strong", "weak")
  for(i in 1:3){ # controls patients
    m_var3[i,1] <- var(c.data[,i+1])
    m_var3[i,2] <- var(p.data[,i+1])
  }
  
  m_mean3
  m_var3
  
  m_mean1
  m_var1
  m_mean2
  m_var2
  m_mean3
  m_var3

  write.csv(sd2, file="./res/sd2.csv")
  
#   write.csv(m_mean1, file="./res/m_mean1.csv")
#   write.csv(m_mean2, file="./res/m_mean2.csv")
#   write.csv(m_mean3, file="./res/m_mean3.csv")
#   write.csv(m_var1, file="./res/m_var1.csv")
#   write.csv(m_var2, file="./res/m_var2.csv")
#   write.csv(m_var3, file="./res/m_var3.csv")
}
