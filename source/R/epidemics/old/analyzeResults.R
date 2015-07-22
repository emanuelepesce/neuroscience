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
  
}
