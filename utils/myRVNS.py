class MyRVNS:
    def shake(self, x, k, probdata):
        # o objetivo do shake é escolher uma solução aleatoria da k-esima estrutura de vizinhança
        
        y = copy.deepcopy(x)

        ridx1 = np.random.randint(0, probdata.n)
        ridx2 = np.random.randint(0, probdata.n)
        ridx3 = np.random.randint(0, probdata.n)
        
        # trocar as manutenções de lugar não afeta o custo o objetivo
        if k == 1:             # troca o plano de manutenção de uma maquina aleatoria por um mais barato
            y.solution[ridx1] = x.solution[ridx1] - 1 if x.solution[ridx1] > 0 else x.solution[ridx1]
        elif k == 2:           # de duas aleatorias
            y.solution[ridx1] = x.solution[ridx1] - 1 if x.solution[ridx1] > 0 else x.solution[ridx1]
            y.solution[ridx2] = x.solution[ridx2] - 1 if x.solution[ridx2] > 0 else x.solution[ridx2]
        elif k == 3:           # de três aleatorias
            y.solution[ridx1] = x.solution[ridx1] - 1 if x.solution[ridx1] > 0 else x.solution[ridx1]
            y.solution[ridx2] = x.solution[ridx2] - 1 if x.solution[ridx2] > 0 else x.solution[ridx2]
            y.solution[ridx3] = x.solution[ridx3] - 1 if x.solution[ridx3] > 0 else x.solution[ridx3]
        
        return y

    def neighborhoodChange(self, x, y, k):
        # toma a decisao de mudar de vizinhança, comparando a solução imcubente x com a solução do shake y
        
        if y.fitness < x.fitness:
            x = copy.deepcopy(y)
            k = 1
        else:
            k += 1
            
        return x, k