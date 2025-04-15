%==========================================================================
% Universidade Federal de Minas Gerais
% Escola de Engenharia da UFMG
% Depto. de Engenharia Elétrica
%
% Autor:
%   Lucas S. Batista
%
% Atualização: 27/11/2020
%
% Nota:
%   Aproxima o ponto de ótimo (minimização) de um problema irrestrito
%   usando o RVNS.
% =========================================================================


function [x, jx] = VNS()

%
% Sintaxe
% [x, jx] = VNS()
%
% Exemplo
% [x, jx] = VNS()
%


% Número de estruturas de vizinhanças definidas
kmax = 3;

% Contador do número de avaliações de f(.)
nfe = 0; 

% Gera solução inicial
[x,custo,n] = initialSol();
jx = fobj(x,custo,n);
nfe = nfe + 1;

% Armazena a solução corrente
memoryfile(1,:) = [x(:)' jx];


% Critério de parada
while (nfe < 5000)
    
    k = 1;          
    while (k <= kmax),
        
        % Gera uma solução na k-ésima vizinhança de x
        y   = shake(x,k);
        jy  = fobj(y,custo,n);
        nfe = nfe + 1;        
        
        [x,jx,k] = neighborhoodChange(x,jx, y,jy, k);
        
        % Armazena a solução corrente
        memoryfile(size(memoryfile,1)+1,:) = [x(:)' jx];
    end
end
fprintf('\n')

figure
plot(0:size(memoryfile,1)-1,memoryfile(:,end),'k-','linewidth',2)
xlabel('Número de avaliações')
ylabel('Valor da função objetivo')


