rm(list = ls())

if (length(dev.list())) {
  dev.off()
}

library("flextable")

epsilons <- c(
  1048.17,
  1066.05,
  1083.93,
  1101.81,
  1119.69,
  1137.57,
  1155.45,
  1173.33,
  1191.21,
  1209.09,
  1226.97,
  1244.85,
  1262.73,
  1280.61,
  1298.49,
  1316.37,
  1334.25,
  1352.13,
  1370.01,
  1387.89,
  1405.77,
  1423.65,
  1441.53,
  1459.41,
  1477.29,
  1495.17,
  1513.05,
  1530.93,
  1548.81,
  1566.69,
  1584.57,
  1602.45,
  1620.33,
  1638.21,
  1656.09,
  1673.97,
  1691.85,
  1709.73,
  1727.61,
  1745.4
)

set.seed(2)

f1 <- function(solution) {
  f1_res <- 0
  
  for (i in 1:length(solution)) {
    f1_res <- f1_res + (solution[i] - 1)
  }
  
  return (f1_res)
}

f2 <- function(solution, p_ij, d_i) {
  f2_res <- 0
  
  for (i in 1:length(solution)) {
    f2_res <- f2_res + p_ij[i, solution[i]] * d_i[i]
  }
  
  return (f2_res)
}

a3 <- function(solution) {
  # razao entre o numero de M1 e M3
  a3_res <- 0
  
  # protecao divisao por zero
  if (length(which(solution == 3)) == 0) {
    a3_res <- 100
  } else {
    a3_res <- length(which(solution == 1)) / length(which(solution == 3))
  }
  
  return (a3_res)
}

a4 <- function(solution, equipdb) {
  # soma dos cluster 1 em M3 + os do cluster 4 em M1
  a4_res <- 0
  
  # pega os equipamentos do cluster 1
  eqps_c1 <- which(equipdb[,3] == 1)
  for (idx in eqps_c1) {
    if (solution[idx] == 3) {
      a4_res <- a4_res + 1
    }
  }
  
  # pega os equipamentos do cluster 4
  eqps_c4 <- which(equipdb[,3] == 4)
  for (idx in eqps_c4) {
    if (solution[idx] == 1) {
      a4_res <- a4_res + 1
    }
  }
  
  return (a4_res)
}

CLUSTER_COLORS <- c("lightblue", "lightgreen", "lightpink", "white")
CLUSTER_NAMES <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")

equipdb_path <- "C:\\dev\\td-2025-1\\arquivos_tc\\EquipDB.csv"
clusterdb_path <- "C:\\dev\\td-2025-1\\arquivos_tc\\ClusterDB.csv"
mpdb_path <- "C:\\dev\\td-2025-1\\arquivos_tc\\MPDB.csv"

equipdb <- read.csv(equipdb_path, header = FALSE, sep = ",")
clusterdb <- read.csv(clusterdb_path, header = FALSE, sep = ",")
mpdb <- read.csv(mpdb_path, header = FALSE, sep = ",")

csv_path <- "C:\\dev\\td-2025-1\\tc2-refactor-epsilon\\resultados_pareto_epsilon_rest_penalidade_2025-06-23_01-34-10.csv"
# csv_path <- "C:\\dev\\td-2025-1\\f2_simplex_csv.csv"

data <- as.matrix(read.csv(csv_path))

data <- cbind(rep(epsilons, 5), data)

N <- nrow(equipdb)
J <- 3
number_of_solutions <- 20

# seleciona number_of_solutions aleatorias
solutions <- data[sample(1:nrow(data), number_of_solutions), ]

# custo de falha do equipamento i
d_i <- equipdb[,4]

# probabilidade de falha p_ij
weibull_dist <- function(t, eta, beta) {
  return(1 - exp(-(t/eta)^beta))
}

p_ij <- matrix(NA, nrow = N, ncol = J)
f2_eqpto <- matrix(NA, nrow = N, ncol = J)

for (i in 1:N) {
  delta_t = 5
  t0 <- equipdb[i, 2]
  cluster_id <- equipdb[i, 3]
  
  # pega os dados desse cluster
  cluster_data <- clusterdb[which(clusterdb[,1]==cluster_id),]
  eta_cluster <- cluster_data[2]
  beta_cluster <- cluster_data[3]
  
  for (j in 1:J) {
    # pega o dado do fator de risco da manutenção
    k <- mpdb[j, 2]
    
    numerator <- weibull_dist(t0 + k * delta_t, eta_cluster, beta_cluster) - weibull_dist(t0, eta_cluster, beta_cluster)
    denominator <- 1 - weibull_dist(t0, eta_cluster, beta_cluster)
    
    p_ij[i, j] <- as.numeric(numerator) / as.numeric(denominator)
    f2_eqpto[i, j] <- p_ij[i, j] * d_i[i]
  }
}

all_data <- matrix(NA, nrow = N, ncol = 7)
for (i in 1:N) {
  all_data[i, 1] <- equipdb[i, 1]
  all_data[i, 2] <- equipdb[i, 3]
  all_data[i, 3] <- f2_eqpto[i, 1]
  all_data[i, 4] <- f2_eqpto[i, 2]
  all_data[i, 5] <- f2_eqpto[i, 3]
  all_data[i, 6] <- equipdb[i, 4]
}

counter <- 0
attributes_data <- matrix(NA, nrow = number_of_solutions, ncol = 4)
for (i in 1:number_of_solutions) {
   counter <- counter + 1
   epsilon <- solutions[i, 1]
   solution <- solutions[i, 2:ncol(solutions)]
   all_data[, 7] <- as.matrix(solution, ncol = 1)
   
   attributes_data[i, 1] <- f1(solution)
   attributes_data[i, 2] <- f2(solution, p_ij, d_i)
   attributes_data[i, 3] <- a3(solution)
   attributes_data[i, 4] <- a4(solution, equipdb)
  
  # barplot mostrando o numero de equipamentos por cluster com cada tipo de 
  # manutencao
  
  # matriz 4 x 3: elementos i, j indica o numero de equipamentos
  # do cluster i com o plano de manuteção j
  maint_per_cluster <- matrix(NA, ncol = J, nrow = nrow(clusterdb))
  
  for (k in 1:nrow(clusterdb)) {
    for (j in 1:J) {
      maint_per_cluster[k, j] <- length(
        all_data[which(all_data[,7] == j & all_data[, 2] == k)]
      )
    }
  }
  
  # Naming rows and columns
  rownames(maint_per_cluster) < CLUSTER_NAMES
  colnames(maint_per_cluster) <- c("M1", "M2", "M3")
  
  barplot(maint_per_cluster,
          beside = TRUE,       # Side-by-side bars
          col = CLUSTER_COLORS,
          main = paste("Num eqptos x cluster - epsilon = ", epsilon),
          ylab = "Numero de Equipamentos", ylim = c(0, 200),
          xlab = "Plano de Manutenção")
  
  legend("top",          
         legend = CLUSTER_NAMES, fill = CLUSTER_COLORS)
  
  maint_per_cluster <- t(maint_per_cluster)
  
  # Naming rows and columns
  rownames(maint_per_cluster) < c("M1", "M2", "M3")
  colnames(maint_per_cluster) <- CLUSTER_NAMES
  
  MAINT_COLORS <- c("blue", "green", "red")
  
  barplot(maint_per_cluster,
          beside = TRUE,       # Side-by-side bars
          col = MAINT_COLORS,
          main = paste("Num eqptos x cluster - epsilon = ", epsilon, " c = ", counter),
          ylab = "Numero de Equipamentos", ylim = c(0, 200),
          xlab = "Plano de Manutenção")
  
  legend("top",          
         legend = c("M1", "M2", "M3"), fill = MAINT_COLORS)
}

# para cada solucao avaliada, monta as tabelas do AHP
compare_by_f1 <- function(sol1, sol2) {
  diff <- sol1 - sol2
  
  if (diff <= -500) {
    return (9)
  } 
  
  if (diff <= -300) {
    return (5)
  } 
  
  if (diff <= -100) {
    return (3)
  } 
  
  if (diff <= 100 & diff >= -100) {
    return (1)
  }

  if (diff >= 100 & diff < 300) {
    return (1 / 3)
  }
  
  if (diff >= 300 & diff <= 500) {
    return (1 / 5)
  }
  
  if (diff >= 500) {
    return (1 / 9)
  }
}

compare_by_f2 <- function(sol1, sol2) {
  diff <- sol1 - sol2
  
  if (diff <= -500) {
    return (9)
  } 
  
  if (diff <= -300) {
    return (5)
  } 
  
  if (diff <= -100) {
    return (3)
  } 
  
  if (diff <= 100 & diff >= -100) {
    return (1)
  }
  
  if (diff >= 100 & diff < 300) {
    return (1 / 3)
  }
  
  if (diff >= 300 & diff <= 500) {
    return (1 / 5)
  }
  
  if (diff >= 500) {
    return (1 / 9)
  }
}

compare_by_f3 <- function(sol1, sol2) {
  # normalize eles
  sol1_norm <- 0
  sol2_norm <- 0
  
  if (sol1 >= 1) {
    sol1_norm <- sol1
  } else {
    sol1_norm <- 1 / sol1
  }
  
  if (sol2 >= 1) {
    sol2_norm <- sol2
  } else {
    sol2_norm <- 1 / sol2
  }
  
  # agora compara a distancia deles ao 1
  sol1_dist <- abs(sol1_norm - 1)
  sol2_dist <- abs(sol2_norm - 1)
  
  # compara a diff entre as distancias
  diff <- sol1_dist - sol2_dist
  
  if (diff <= -5) {
    return (9)
  } 
  
  if (diff <= -3) {
    return (5)
  } 
  
  if (diff <= -1) {
    return (3)
  } 
  
  if (diff <= 1 & diff >= -1) {
    return (1)
  }
  
  if (diff >= 1 & diff <= 3) {
    return (1 / 3)
  }
  
  if (diff >= 3 & diff <= 5) {
    return (1 / 5)
  }
  
  if (diff >= 5) {
    return (1 / 9)
  }
}

compare_by_f4 <- function(sol1, sol2) {
  # quanto maior, melhor
  diff <- sol1 - sol2

  if (diff <= -5) {
    return (1 / 9)
  } 
  
  if (diff <= -3) {
    return (1 / 5)
  } 
  
  if (diff <= -1) {
    return (1 / 3)
  } 
  
  if (diff <= 1 & diff >= -1) {
    return (1)
  }
  
  if (diff >= 1 & diff <= 3) {
    return (3)
  }
  
  if (diff >= 3 & diff <= 5) {
    return (5)
  }
  
  if (diff >= 5) {
    return (9)
  }
}

# atributo 1: f1
f1_table <- matrix(NA, ncol = number_of_solutions, nrow = number_of_solutions)
f1_solutions <- attributes_data[ ,1]
for (i in 1:number_of_solutions) {
  for (j in 1:number_of_solutions) {
    if (i == j) {
      f1_table[i, j] <- 1
    }
    
    if (i > j) { # somente o triangulo inferior
      f1_table[i, j] <- compare_by_f1(f1_solutions[i], f1_solutions[j])
    }
  }
}

# completa o triangulo superior
for (i in 1:number_of_solutions) {
  for (j in 1:number_of_solutions) {
    if (j > i) { # somente o triangulo inferior
      f1_table[i, j] <- 1 / f1_table[j, i]
    }
  }
}

# calcula as coisas do slide - f1
eigvecs <- eigen(f1_table)$vectors
eigvals <- eigen(f1_table)$values
priority_vec_f1 <- eigvecs[,1] / sum(eigvecs[,1])
ic_f1 <- (eigvals[1] - number_of_solutions) / (number_of_solutions - 1)


# atributo 2: f2
f2_table <- matrix(NA, ncol = number_of_solutions, nrow = number_of_solutions)
f2_solutions <- attributes_data[ ,2]
for (i in 1:number_of_solutions) {
  for (j in 1:number_of_solutions) {
    if (i == j) {
      f2_table[i, j] <- 1
    }
    
    if (i > j) { # somente o triangulo inferior
      f2_table[i, j] <- compare_by_f2(f2_solutions[i], f2_solutions[j])
    }
  }
}

# completa o triangulo superior
for (i in 1:number_of_solutions) {
  for (j in 1:number_of_solutions) {
    if (j > i) { # somente o triangulo inferior
      f2_table[i, j] <- 1 / f2_table[j, i]
    }
  }
}

# calcula as coisas do slide - f2
eigvecs <- eigen(f2_table)$vectors
eigvals <- eigen(f2_table)$values
priority_vec_f2 <- eigvecs[,1] / sum(eigvecs[,1])
ic_f2 <- (eigvals[1] - number_of_solutions) / (number_of_solutions - 1)


# atributo 3: f3
f3_table <- matrix(NA, ncol = number_of_solutions, nrow = number_of_solutions)
f3_solutions <- attributes_data[ ,3]
for (i in 1:number_of_solutions) {
  for (j in 1:number_of_solutions) {
    if (i == j) {
      f3_table[i, j] <- 1
    }
    
    if (i > j) { # somente o triangulo inferior
      f3_table[i, j] <- compare_by_f3(f3_solutions[i], f3_solutions[j])
    }
  }
}

for (i in 1:number_of_solutions) {
  for (j in 1:number_of_solutions) {
    if (j > i) { # somente o triangulo inferior
      f3_table[i, j] <- 1 / f3_table[j, i]
    }
  }
}

# calcula as coisas do slide - f3
eigvecs <- eigen(f3_table)$vectors
eigvals <- eigen(f3_table)$values
priority_vec_f3 <- eigvecs[,1] / sum(eigvecs[,1])
ic_f3 <- (eigvals[1] - number_of_solutions) / (number_of_solutions - 1)

# atributo 4: f4
f4_table <- matrix(NA, ncol = number_of_solutions, nrow = number_of_solutions)
f4_solutions <- attributes_data[ ,4]
for (i in 1:number_of_solutions) {
  for (j in 1:number_of_solutions) {
    if (i == j) {
      f4_table[i, j] <- 1
    }
    
    if (i > j) { # somente o triangulo inferior
      f4_table[i, j] <- compare_by_f4(f4_solutions[i], f4_solutions[j])
    }
  }
}

for (i in 1:number_of_solutions) {
  for (j in 1:number_of_solutions) {
    if (j > i) { # somente o triangulo inferior
      f4_table[i, j] <- 1 / f4_table[j, i]
    }
  }
}

# calcula as coisas do slide - f4
eigvecs <- eigen(f4_table)$vectors
eigvals <- eigen(f4_table)$values
priority_vec_f4 <- eigvecs[,1] / sum(eigvecs[,1])
ic_f4 <- (eigvals[1] - number_of_solutions) / (number_of_solutions - 1)

# tabela de comparação entre os critérios: 4 x 4
# priorizando f1
att_table <- matrix(c(
  1, 5, 5, 3,
  1/5, 1, 5, 3,
  1/5, 1/5, 1, 1/3,
  1/3, 1/3, 3, 1
), ncol = 4, nrow = 4, byrow = T)

# priorizando f2
# att_table <- matrix(c(
#   1, 1 / 5, 5, 3,
#   5, 1, 5, 3,
#   1/5, 1/5, 1, 1/3,
#   1/3, 1/3, 3, 1
# ), ncol = 4, nrow = 4, byrow = T)

# priorizando f3
att_table <- matrix(c(
  1, 1, 1 / 7, 1,
  1, 1, 1 / 7, 1,
  7, 7, 1, 5,
  1, 1, 1 / 5, 1
), ncol = 4, nrow = 4, byrow = T)

# priorizando f4
# att_table <- matrix(c(
#   1, 1 / 5, 1 / 5, 1 / 5,
#   5, 1, 1 / 5, 1 / 5,
#   5, 5, 1, 1 / 3,
#   5, 5, 3, 1
# ), ncol = 4, nrow = 4, byrow = T)

# priorizando todos igual
# att_table <- matrix(rep(1, 16), ncol = 4, nrow = 4, byrow = T)

# calcula as coisas do slide - att_table
eigvecs <- eigen(att_table)$vectors
eigvals <- eigen(att_table)$values
priority_vec_att <- eigvecs[,1] / sum(eigvecs[,1])
ic_att <- (eigvals[1] - 4) / (4 - 1)

# calcula a prioridade global de cada uma das 20 solucoes
global_priorities <- c()
for (i in 1:number_of_solutions) {
  global_priorities <- c(
    global_priorities, 
    priority_vec_f1[i] * priority_vec_att[1] + 
      priority_vec_f2[i] * priority_vec_att[2] +
      priority_vec_f3[i] * priority_vec_att[3] +
      priority_vec_f4[i] * priority_vec_att[4]
  )
}
chosen_sol <- which.max(global_priorities)
print(chosen_sol)

# df <- data.frame(
#   seq(1, 20, 1), 
#   attributes_data[, 1],
#   round(attributes_data[, 2]),
#   round(attributes_data[, 3], 2),
#   attributes_data[, 4]
# )
# colnames(df) <- c("Solução", "f1", "f2", "A3", "A4")
# ft <- flextable(df)
# ft <- align(ft, align = "center", part = "all")
# ft

df <- data.frame(
  c("f1", "f2", "A3", "A4"), 
  round(att_table, 2)
)
colnames(df) <-  c("Att", "f1", "f2", "A3", "A4")
ft <- flextable(df)
ft <- align(ft, align = "center", part = "all")
ft

plot(
  NULL,
  main = "Soluções AHP na Fronteira Pareto",
  xlab = "f1",
  ylab = "f2",
  ylim = c(1048.17, 1745),
  xlim = c(0, 1000)
)

points(attributes_data[,1], attributes_data[,2], col = "blue", lwd = 3)
points(f1_solutions[13], f2_solutions[13], col = "green", lwd = 6, pch = 3)
points(f1_solutions[4], f2_solutions[4], col = "red", lwd = 6, pch = 3)
legend("topright",                     # Position
       legend = c("Fronteira", "Solução 1 AHP", "Solução 2 AHP"),
       col = c("blue", "red", "green"),        # Colors
       pch = c(1, 3, 3))                      # Line type (solid lines))                       # Line width

