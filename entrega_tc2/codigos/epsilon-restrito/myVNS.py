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
        ridx1 = np.random.randint(0, probdata.n)
        ridx2 = np.random.randint(0, probdata.n)
        blockSize = 50
        startIndex = np.random.randint(0, probdata.n - blockSize)
        blockIndices = range(startIndex, startIndex + blockSize)

        if k == 1:
            y[ridx1] = np.random.choice([j for j in [0, 1, 2] if j != x[ridx1]])
        elif k == 2:
            y[ridx1], y[ridx2] = x[ridx2], x[ridx1]
        elif k == 3:
            currentPlans = [x[i] for i in blockIndices]
            currentPlan = max(set(currentPlans), key=currentPlans.count)
            newPlan = np.random.choice([j for j in [0, 1, 2] if j != currentPlan])
            for i in blockIndices:
                y[i] = newPlan

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