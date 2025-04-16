class MyRVNS:
    def shake(self, x, k, probdata):
        
        y = copy.deepcopy(x)
        r = np.random.permutation(probdata.n)
        
        if k == 1:             # exchange two random positions
            y.solution[r[0]] = x.solution[r[1]]
            y.solution[r[1]] = x.solution[r[0]]        
        elif k == 2:           # exchange three random positions
            y.solution[r[0]] = x.solution[r[1]]
            y.solution[r[1]] = x.solution[r[2]]
            y.solution[r[2]] = x.solution[r[0]]
        elif k == 3:           # shift positions     
            z = y.solution.pop(r[0])
            y.solution.insert(r[1],z)
        
        return y

    def neighborhoodChange(self, x, y, k):
        
        if y.fitness < x.fitness:
            x = copy.deepcopy(y)
            k = 1
        else:
            k += 1
            
        return x, k