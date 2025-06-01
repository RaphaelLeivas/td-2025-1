import numpy as np
import copy

class Struct:
    pass

class MyVNS:
    # o objetivo do shake é escolher uma solução aleatoria da k-esima estrutura de vizinhança
    def shake(self, x, k, probdata):
        if not isinstance(x, np.ndarray):
            raise Exception("x is not an array at MyProblem.fobj2")

        y = copy.deepcopy(x)

        ridx_full = np.random.randint(0, probdata.n)
        idx_large = np.where(y > 0)
        ridx_large = np.random.randint(0, len(idx_large), size=2)
        ridx1 = ridx_large[0]
        ridx2 = ridx_large[1]
        
        if k == 1:             # troca o plano de um aleatorio por um mais barato
            y[ridx_full] = 0 if y[ridx_full] == 0 else y[ridx_full] - 1
        elif k == 2:           # troca o plano de um que esta caro por um mais barato
            y[ridx1] = 0 if y[ridx1] == 0 else y[ridx1] - 1
        elif k == 3:           # Mudança de um bloco de equipamentos para outro plano (sair de mínimo local)
            y[ridx1] = 0 if y[ridx1] == 0 else y[ridx1] - 1
            y[ridx2] = 0 if y[ridx2] == 0 else y[ridx2] - 1

        return y

    # toma a decisao de mudar de vizinhança, comparando a solução imcubente x com a solução do shake y
    def neighborhoodChange(self, x, y, k, func_obj, probdata):
        if func_obj(y, probdata) < func_obj(x, probdata):
            x = copy.deepcopy(y)
            k = 1
        else:
            k += 1
            
        return x, k
        
    
    # retorna a primeira melhor solução que achar na estrutura de vizinhança local à solução x
    def firstImprovement(self, x, probdata, fobj, k):
        neighborhood_size = 100

        for j in range(neighborhood_size):
            current_solution = self.shake(x, k, probdata)
            
            if fobj(current_solution, probdata) < fobj(x, probdata):
                return current_solution

        # se nao achou ninguem melhor, retorna a solução original mesmo mesmo
        return x    
    
    # retorna a melhor solução que achar na estrutura de vizinhança local à solução x
    def bestImprovement(self, x, probdata, fobj, k):
        neighborhood_size = 10
        best_solution = copy.deepcopy(x)

        for j in range(neighborhood_size):
            current_solution = self.shake(x, k, probdata)
            
            if fobj(current_solution, probdata) < fobj(best_solution, probdata):
                best_solution = current_solution

        return best_solution   