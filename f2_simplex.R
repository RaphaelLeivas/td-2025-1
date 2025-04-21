rm(list = ls())

if (length(dev.list())) {
  dev.off()
}

library("slam")
library("gurobi")
library("flextable")

set.seed(203)

equipdb_path <- "C:\\dev\\td-2025-1\\arquivos_tc\\EquipDB.csv"
clusterdb_path <- "C:\\dev\\td-2025-1\\arquivos_tc\\ClusterDB.csv"
mpdb_path <- "C:\\dev\\td-2025-1\\arquivos_tc\\MPDB.csv"

equipdb <- read.csv(equipdb_path, header = FALSE, sep = ",")
clusterdb <- read.csv(clusterdb_path, header = FALSE, sep = ",")
mpdb <- read.csv(mpdb_path, header = FALSE, sep = ",")

N <- nrow(equipdb)
J <- 3

# custo de falha do equipamento i
d_i <- equipdb[,4]

# probabilidade de falha p_ij
weibull_dist <- function(t, eta, beta) {
  return(1 - exp(-(t/eta)^beta))
}

p_ij <- matrix(NA, nrow = N, ncol = J)

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
  }
}

# ----------- Criar modelo Gurobi -----------
model <- list()

# Sentido da função objetivo (minimização)
model$modelsense <- "min"
numVariaveis <- N * J
numRestricoes <- N

# ----------- Coeficientes da função objetivo -----------
costsVec <- c()

# as N * J posicoes do vetor recebem p_ij * d_i
# ordem: p11 * d1, p12 * d1, ..., p21 * d2, ....

for (i in 1:N) {
  for (j in 1:J) {
    cost <- p_ij[i, j] * d_i[i]
    costsVec <- append(costsVec, cost)
  }
}

model$obj <- costsVec

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

# Resolver o problema com Gurobi
result <- gurobi(model, params)
solution <- result$x
optimalValue <- result$objval

# reescreve a solução em matriz para ser possivel interpretar
current_row <- 0
sol_matrix <- matrix(NA, nrow = N, ncol = J)
for (i in 1:length(solution)) {
  if ((i-1) %% 3 == 0) {
    current_row <- current_row + 1
  }
  
  sol_matrix[current_row, ((i-1) %% 3) + 1] <- solution[i]
}

# na f2, a solução encontrada é colocar o plano mais caro para todos
# na f1, a solução é colocar a mais barata para todos
# a multiobjetivo vai conciliar as duas para chegar num meio termo multicritério

# note que nao era trivial que a solução ótima era colocar a mais cara para todas,
# foi bom ter o simplex para confirmar essa hipótese.

label_column <- c()

for (i in 1:N) {
  label_column <- c(label_column, paste("Equipamento ", i))
}

df <- data.frame(label_column, sol_matrix)
colnames(df) <- c("Eqpto", "j = 1", "j = 2", "j = 3")
ft <- flextable(df)
ft <- add_header_row(
  x = ft, values = c("Equipamento", "Manutenção"),
  colwidths = c(1, J))


