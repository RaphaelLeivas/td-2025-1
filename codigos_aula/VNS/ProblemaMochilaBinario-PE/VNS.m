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
%   Aproxima o ponto de ótimo de um problema irrestrito
%   usando o RVNS.
% =========================================================================


function [x, xretorno, xcusto, capital] = VNS()

%
% Sintaxe
% [x, xretorno, xcusto, capital] = VNS()
%
% Exemplo
% [x, xretorno, xcusto, capital] = VNS()
%


% Número de estruturas de vizinhanças definidas
kmax = 3;

% Contador do número de avaliações de f(.)
nfe = 0; 

% Gera solução inicial
[x,projetos,capital] = initialSol();
[xretorno,xcusto,gx,xrpen] = fobj(x,projetos,capital);
nfe = nfe + 1;

% Armazena a solução corrente
memoryfile(1,:) = [x(:)' xretorno xcusto gx xrpen];


% Critério de parada
while (nfe < 5000)
    
    k = 1;          
    while (k <= kmax)
        
        % Gera uma solução na k-ésima vizinhança de x
        y = shake(x,k);        
        [yretorno,ycusto,gy,yrpen] = fobj(y,projetos,capital);
        nfe = nfe + 1;        
        
        [x,xretorno,xcusto,gx,xrpen,k] = neighborhoodChange(x,xretorno,xcusto,gx,xrpen, y,yretorno,ycusto,gy,yrpen, k);
        
        % Armazena a solução corrente
        memoryfile(size(memoryfile,1)+1,:) = [x(:)' xretorno xcusto gx xrpen];
    end
end
fprintf('\n')

figure, hold on
plt1 = plot(0:size(memoryfile,1)-1,memoryfile(:,end-3),'b-','linewidth',2);
plt2 = plot(0:size(memoryfile,1)-1,memoryfile(:,end-2),'m-','linewidth',2);
plt3 = plot(0:size(memoryfile,1)-1,memoryfile(:,end-1),'r-.','linewidth',2);
hold off
legend([plt1,plt2,plt3],'retorno','custo','g(x)')
xlabel('Número de avaliações')
ylabel('Valores de interesse')
