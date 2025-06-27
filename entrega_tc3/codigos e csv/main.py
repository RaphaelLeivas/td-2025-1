import numpy as np
import matplotlib.pyplot as plt
from myVNS import MyVNS
from myProblem import MyProblem
from datetime import datetime
import csv
import pandas as pd

class Struct:
    pass

num_execucoes = 5
max_num_sol_avaliadas = 1000
kmax = 3

convergencias = []
melhores_fitness = []

# gera os epsilons
min_f2 = 1048.17
max_f2 = 1745.49
number_of_epsilons = 40
epsilons = np.linspace(min_f2, max_f2, number_of_epsilons)

historico_fronteira = Struct()
historico_fronteira.f1 = []
historico_fronteira.f2 = []
solucoes_pareto = []

def is_convergence_good(history, tol_size, tol):
    if len(history) < 10:
        return False  # or handle this case as needed
       
    last_tol = history[-tol_size:]  # Get the last tol_size elements
    std = np.std(last_tol)

    return std < tol

for execucao in range(num_execucoes):
    print(f'Rodando execução {execucao + 1}...')

    f1_solucoes = []
    f2_solucoes = []

    for t in range(number_of_epsilons):
        epsilon = epsilons[t]
        print(f'Rodando numero {t + 1} ε = {epsilon:.2f} em {datetime.now()}')

        # Cria instâncias dos objetos
        myProblem = MyProblem()
        myVNS = MyVNS()

        # Gera os dados do problema
        probdata = myProblem.probdef()
        probdata.epsilon = epsilon

        # Gera uma solução inicial
        sol_inicial = myProblem.sol_inicial(probdata, apply_constructive_heuristic=False)

        # Escolhe a função objetivo
        func_obj = myProblem.fobj_epsilon_restrito  # Troque para fobj1 se quiser

        fitness_sol_inicial = func_obj(sol_inicial, probdata)
        num_sol_avaliadas = 1

        historico = Struct()

        historico.sol = []
        historico.fit = []
        historico.sol.append(sol_inicial)
        historico.fit.append(myProblem.fobj1(sol_inicial, probdata))

        sol_atual = sol_inicial
        fitness_atual = fitness_sol_inicial

        # loop principal (BVNS)
        tol = 10**(-5) # desvio padrao
        tol_iteractions = 100

        # se nas ultimas tol_iteractions a diferenca entre os fitness for menor que tol, para

        while num_sol_avaliadas < max_num_sol_avaliadas:
            k = 1
            previous_fitness = 0
            stop_flag = False
            while k <= kmax:
                num_sol_avaliadas += 1

                # Gera uma solução candidata na k-ésima vizinhança de x 
                sol_candidata = myVNS.shake(sol_atual, k, probdata)

                # faz uma busca local a partir da solução candidata do shake
                # sol_local = myVNS.firstImprovement(sol_candidata, probdata, func_obj, k)
                sol_local = myVNS.bestImprovement(sol_candidata, probdata, func_obj, k)

                # Atualiza solução corrente e estrutura de vizinhança (se necessário)
                x, k = myVNS.neighborhoodChange(sol_atual, sol_local, k, func_obj, probdata)

                sol_atual = x
                fitness_atual = func_obj(sol_atual, probdata)

                # dados para plot
                historico.sol.append(sol_atual)
                historico.fit.append(myProblem.fobj1(sol_atual, probdata))

                # checa a convergencia
                if is_convergence_good(historico.fit, tol_iteractions, tol):
                    stop_flag = True
                    break
                else:
                    previous_fitness = myProblem.fobj1(sol_atual, probdata)

            if stop_flag: 
                break

        # melhor solucao encontrada:
        f1_solucoes.append(myProblem.fobj1(sol_atual, probdata))
        f2_solucoes.append(myProblem.fobj2(sol_atual, probdata))
        solucoes_pareto.append(sol_atual)

    historico_fronteira.f1.append(f1_solucoes)
    historico_fronteira.f2.append(f2_solucoes)


dados_resultado = []

for i in range(len(solucoes_pareto)):
    linha = {}
    solucao = solucoes_pareto[i]
    for j, valor in enumerate(solucao):
        linha[f'x{j}'] = valor+1
    dados_resultado.append(linha)

df_resultado = pd.DataFrame(dados_resultado)

# Gera nome do arquivo com data e hora atual
datahora = datetime.now().strftime('%Y-%m-%d_%H-%M-%S')
nome_arquivo = f'resultados_pareto_epsilon_rest_penalidade_{datahora}.csv'

# Salva o DataFrame como CSV
df_resultado.to_csv(nome_arquivo, index=False, header=False)
print(f'\nArquivo salvo: {nome_arquivo}')


# Plotagem dos resultados
plt.figure(figsize=(10, 6))
for i in range(num_execucoes):
    plt.plot(historico_fronteira.f1[i], historico_fronteira.f2[i], 'o', label=f'Execução {i+1}')
plt.title('Fronteiras de Pareto - Método ε-restrito (5 execuções)')
plt.xlabel('f1 (Custo de manutenção)')
plt.ylabel('f2 (Impacto de falha)')
plt.grid(True)
plt.legend()
plt.show()