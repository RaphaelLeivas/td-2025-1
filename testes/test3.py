
import matplotlib.pyplot as plt
import numpy as np
import csv

class Struct:
    pass

historico = Struct()
historico.f1 = []
historico.f2 = []


num_execucoes = 5
min_f2 = 1048.17
max_f2 = 1745.49
number_of_epsilons = 20

data = []

for i in range(num_execucoes):
    f1_solucoes = []
    f2_solucoes = []

    for t in range(number_of_epsilons):
        f1_sol = np.random.uniform(min_f2, max_f2)
        f2_sol = np.random.uniform(0, 1000)

        f1_solucoes.append(f1_sol)
        f2_solucoes.append(f2_sol)

    historico.f1.append(f1_solucoes)
    historico.f2.append(f2_solucoes)
    data.append(f1_solucoes)
    data.append(f2_solucoes)


data = [
    ['Name', 'Age', 'City'],
    ['Alice', 25, 'New York'],
    ['Bob', 30, 'Los Angeles'],
    ['Charlie', 35, 'Chicago']
]
    
with open('fronteiras_epsilon.csv', mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(data)
    file.close()




plt.figure(figsize=(10, 6))
for i in range(num_execucoes):
    plt.plot(historico.f1[i], historico.f2[i], 'o', label=f'Execução {i+1}')
plt.title('Fronteiras de Pareto - Método ε-restrito (5 execuções)')
plt.xlabel('f1 (Custo de manutenção)')
plt.ylabel('f2 (Impacto de falha)')
plt.grid(True)
plt.legend()
plt.show()




