rm(list = ls())

if (length(dev.list())) {
  dev.off()
}

library("slam")
library("gurobi")
library("flextable")

set.seed(203)

CLUSTER_COLORS <- c("lightblue", "lightgreen", "lightpink", "white")
CLUSTER_NAMES <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")

equipdb_path <- "C:\\dev\\td-2025-1\\arquivos_tc\\EquipDB.csv"
clusterdb_path <- "C:\\dev\\td-2025-1\\arquivos_tc\\ClusterDB.csv"
mpdb_path <- "C:\\dev\\td-2025-1\\arquivos_tc\\MPDB.csv"

equipdb <- read.csv(equipdb_path, header = FALSE, sep = ",")
clusterdb <- read.csv(clusterdb_path, header = FALSE, sep = ",")
mpdb <- read.csv(mpdb_path, header = FALSE, sep = ",")

N <- nrow(equipdb)
J <- 3
number_of_solutions <- 20

csv_data <- matrix(NA, ncol = N, nrow = number_of_solutions)

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

# ve a distribuicao dos custos de falha esperados por cluster
boxplot(all_data[which(all_data[,2] == 1), 3], 
        all_data[which(all_data[,2] == 2), 3], 
        all_data[which(all_data[,2] == 3), 3],
        all_data[which(all_data[,2] == 4), 3],
        names = CLUSTER_NAMES,
        col = CLUSTER_COLORS,
        main = "f2 x Cluster",
        ylab = "f2")


# conta quantos equipamentos por cluster nos temos
eqptos_per_cluster <- c()
for (c in 1:4) {
  eqptos_per_cluster <- c(eqptos_per_cluster, length(which(all_data[,2] == c)))
}

gfg <- data.frame(x = eqptos_per_cluster,
                  grp = CLUSTER_NAMES,
                  subgroup = LETTERS[1:1])
gfg <- reshape(gfg,idvar = "subgroup",
               timevar = "grp",
               direction = "wide")

row.names(gfg) <- gfg$subgroup
gfg <- gfg[ , 2:ncol(gfg)]
colnames(gfg) <- CLUSTER_NAMES
gfg <- as.matrix(gfg)
barplot(height = gfg,beside = TRUE, col = CLUSTER_COLORS)

# ----------- Criar modelo Gurobi -----------
model <- list()

# Sentido da função objetivo (minimização)
model$modelsense <- "min"
numVariaveis <- N * J
numRestricoes <- N

# ----------- Coeficientes da função objetivo -----------
f2_costsVec <- c()
f1_costsVec <- c()

# as N * J posicoes do vetor recebem p_ij * d_i
# ordem: p11 * d1, p12 * d1, ..., p21 * d2, ....

for (i in 1:N) {
  for (j in 1:J) {
    cost <- p_ij[i, j] * d_i[i]
    f2_costsVec <- append(f2_costsVec, cost)
  }
}

for (i in 1:N) {
  for (j in 1:J) {
    cost <- j - 1 # custos sao 0, 1 e 2
    f1_costsVec <- append(f1_costsVec, cost)
  }
}

# model$obj <- costsVec

# ----------- Coeficientes da matriz A e vetor b das restrições -----------
# tem que configurar o vetor b (rhs), matriz A (A), igualdade/desigualdade
# (sense) e domínio das variáveis (vtype)

# restrições tipo 1: N * J equações de igualdade para sum x_ij = 1 para todo i
A <- matrix(0, nrow = numRestricoes, ncol = numVariaveis)
b <- rep(1, numRestricoes)

for (i in 1:N) {
  A[i, 3 * (i-1) + 1] = 1
  A[i, 3 * (i-1) + 2] = 1
  A[i, 3 * (i-1) + 3] = 1
}

# define as igualdades/desigualdades
senses <- rep("=", numRestricoes)

# define os tipos das variaveis: são binarias
vtypes <- rep("B", N * J)

# joga tudo no modelo do gurobi
model$rhs <- b
model$A <- A
model$sense <- senses
model$vtypes <- vtypes

# Configuração dos parâmetros do Gurobi
params <- list()
params$outputflag <- 0 # Suprimir saídas do Gurobi

# ----------- Foramatação de dados finalizado: chama o Solver -----------

w1 <- runif(number_of_solutions, min = 0, max = 1)
w2 <- 1 - w1

f1_arr <- c()
f2_arr <- c()

min_f1 = 0
max_f1 = 1000
min_f2 = 1048.17 
max_f2 = 1745.49 

counter <- 0
solutions <- matrix(NA, nrow = number_of_solutions, ncol = N * J)
for (i in 1:length(w1)) {
  counter <- counter + 1
  
  # so o denominador da normalizacao entra, a outra parcela da soma nao 
  # tem variavel e nao interfere na minimizacao
  model$obj <- w1[i] * (1 / (max_f1 - min_f1)) * f1_costsVec +
    w2[i] * (1 / (max_f2 - min_f2)) * f2_costsVec
  
  # model$obj <- w1[i] * f1_costsVec + w2[i] * f2_costsVec
  
  # Resolver o problema com Gurobi
  result <- gurobi(model, params)
  solution <- result$x
  optimalValue <- result$objval
  
  solutions[i, ] <- solution
  
  # formata ela para a analise que precisamos
  for (k in 1:N) {
    base_idx <- 3 * (k - 1) + 1
    chosen_maintenance <- NA
    
    if (solution[base_idx] == 1) {
      chosen_maintenance <- 1
    } else if (solution[base_idx + 1] == 1) {
      chosen_maintenance <- 2
    } else {
      chosen_maintenance <- 3
    }
    
    all_data[k, 7] <- chosen_maintenance
  }
  
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
          main = paste("Num eqptos x cluster - w1 = ", round(w1[i], 4), " w2 = ", round(w2[i], 4)),
          ylab = "Numero de Equipamentos", ylim = c(0, 200))
  
  legend("top",          
         legend = CLUSTER_NAMES, fill = CLUSTER_COLORS)
}
