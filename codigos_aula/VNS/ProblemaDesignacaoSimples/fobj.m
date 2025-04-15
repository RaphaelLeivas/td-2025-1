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
%   Define a função objetivo.
% =========================================================================


function f = fobj(x,custo,n)

% Problema de Designação Simples:
% Considere que existem n tarefas e n agentes, de tal forma
% que cada tarefa deve ser atribuída a um agente e cada
% agente só pode receber uma tarefa. A execução da tarefa i
% pelo agente j tem um custo cij. Formule um problema que
% atribua as tarefas de forma a minimizar o custo total de
% execução.

f = 0;
for i = 1:n,
    f = f + custo(i,x(i));
end
