import numpy as np
import csv

class Struct:
    pass

class MyProblem:
    '''  
    Modelamos a solução x como a sequência de manutenções atribuídas em um vetor. exemplo
    
        eq1 eq2 ... eq500
    x = [2   0  ...   1]

        nesse exemplo, o equipamento eq1 executa a manutenção que tem custo 2, o equipamento eq2 manutenção que tem custo 0, ...
    '''
            
    def fobj1(self, x, probdata):
        if not isinstance(x, np.ndarray):
            raise Exception("x is not an array at MyProblem.fobj1")
        
        # é só somar o vetor que temos a solução
        return np.sum(x)
    
    def fobj2(self, x, probdata):
        if not isinstance(x, np.ndarray):
            raise Exception("x is not an array at MyProblem.fobj2")
        
        # é só somar o os valores de dipij variando o valor de j
        fitness = 0
        for i in range(0,probdata.n):
            j = x[i]
            fitness = fitness + probdata.dipij[i,j]
        return fitness
    

    def fobj_epsilon_restrito(self, x, probdata):
        # é só somar o os valores de dipij variando o valor de j
        fitness=0
        for i in range(0,probdata.n):
            fitness = fitness + np.sum(x) # f1

        # soma o termo de penalidade (slide 9 02-ot-restrita)
        u = 100
        gx = self.fobj2(x, probdata) - probdata.epsilon # f2 <= epsilon ===> f2 - epsilon <= 0

        fitness = fitness + u * (max(0, gx))**2

        return fitness


    def sol_inicial(self, probdata, apply_constructive_heuristic):
        solution = None

        if apply_constructive_heuristic == False:        
            # Constrói solução inicial aleatoriamente
            solution = np.random.randint(0, 2 + 1, size=(probdata.n))
        
        else:
            # Constrói solução inicial usando uma heurística construtiva
            solution = np.random.randint(0, 2 + 1, size=(probdata.n))
            for i in range(probdata.n):        
                var = np.var(probdata.dipij[i,:]) 
                if var>0.5: #limiar da variância
                    solution[i]=2
                else:
                    solution[i]=0

        return solution
    
    def probF(self,eta,beta,t):
        return (1-np.exp(-(t/eta)**(beta)))
    
    def probdef(self):
        n = 500 # número de equipamentos

        # extrai os dados do CSV e salva na matriz
        equip_db = np.zeros(shape=(n, 4))

        with open('../arquivos_tc/EquipDB.csv', newline='') as csvfile:
            spamreader = csv.reader(csvfile, delimiter=' ', quotechar='|')

            for idx, row in enumerate(spamreader):
                row_array = ', '.join(row).split(',')
                data_array = []
                
                for item in row_array:
                    data_array.append(float(item))

                equip_db[idx] = np.array(data_array)

        # agora le o arquivo mpdb
        mpdb = np.zeros(shape=(3, 3))
    	
        # custos do problema sao a ultima coluna do mpdb
        c = np.zeros(3)

        with open('../arquivos_tc/MPDB.csv', newline='') as csvfile:
            spamreader = csv.reader(csvfile, delimiter=' ', quotechar='|')

            for idx, row in enumerate(spamreader):
                row_array = ', '.join(row).split(',')
                data_array = []
                
                for item in row_array:
                    data_array.append(float(item))

                mpdb[idx] = np.array(data_array)
                c[idx] = data_array[len(data_array) - 1]

        # agora le o arquivo cluster
        cluster = np.zeros(shape=(4, 3))
    	
        with open('../arquivos_tc/ClusterDB.csv', newline='') as csvfile:
            spamreader = csv.reader(csvfile, delimiter=' ', quotechar='|')

            for idx, row in enumerate(spamreader):
                row_array = ', '.join(row).split(',')
                data_array = []
                
                for item in row_array:
                    data_array.append(float(item))

                cluster[idx] = np.array(data_array)
        
        # geração da matriz dipij, cada linha é um equipamento i e cada coluna o dipij do plano de manutenção j
        dipij = np.zeros(shape=(n, 3))

        for i in range(0,n):
            clusterid=int(equip_db[i,2]-1)
            eta=cluster[clusterid,1]
            beta=cluster[clusterid,2]
            t0=equip_db[i,1]
            di=equip_db[i,3]
            
            for j in range(0,3):
                k=mpdb[j,1]
                Ft0=self.probF(eta,beta,t0)
                Ft0kt=self.probF(eta,beta,(t0+k*5))
                dipij[i,j]= di*(Ft0kt-Ft0)/(1-Ft0)
                # print(f"i={i}  j={j}  k={k}  eta={eta}  beta={beta}  t0={t0}  Ft0={Ft0}  Ft0kt={Ft0kt}  pij={(Ft0kt-Ft0)/(1-Ft0)}")
        
        probdata = Struct()
        probdata.equip_db = equip_db
        probdata.mpdb = mpdb
        probdata.c = c
        probdata.n = n
        probdata.dipij =dipij
            
        return probdata