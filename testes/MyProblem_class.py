# estrutura de dados para armazenar
class MyProblem:
    def __init__(self, n=10):
        self.n = n

    def fobj(self, x, probdata):
    
        '''
            a1 a2 ... an
        x = [t7 t3 ... t9]
        '''
        
        fitness = 0.0
        for i in np.arange(0,probdata.n,1):
            fitness += probdata.c[i,x.solution[i]]
        
        x.fitness = fitness
        print(fitness)
        return x


    def sol_inicial(self, probdata,apply_constructive_heuristic):
    
        '''  
        Modelou-se uma solução x como a sequência de tarefas atribuídas, respectivamente, aos agentes a1, a2, ... ai, ..., an
        
            a1 a2 ... an
        x = [t7 t3 ... t9]
        '''
        
        if apply_constructive_heuristic == False:        
            # Constrói solução inicial aleatoriamente
            x = Struct()
            x.solution = list(np.random.permutation(probdata.n))
        
        else:
            # Constrói solução inicial usando uma heurística construtiva
            x = Struct()
            x.solution = []
            job = np.argsort(probdata.c.var(axis=0))    # tarefas ordenadas de acordo com a variância dos custos
            for tarefa in job[::-1]:        
                agent = np.argmin(probdata.c[:,tarefa]) # atribui as tarefas em ordem decrescente de variância ao agente de menor custo
                x.solution.insert(agent,tarefa)
            
        return x

    def probdef(self, n=50):
    
        # n: número de tarefas/agentes considerados
            
        '''
        Matriz de custo: c = [  t1 t2 ... tj ... tn
                            a1
                            a2
                            ...
                            ai
                            ...
                            an                    ]
        ''' 
        
        np.random.seed(13)    
        c = np.random.randint(1,100,size=(n,n)) # considera custos inteiros aleatórios no conjunto {1, 2, ..., 99}
        np.random.seed()
        
        probdata = Struct()
        probdata.n = n
        probdata.c = c

        print(n)
            
        return probdata