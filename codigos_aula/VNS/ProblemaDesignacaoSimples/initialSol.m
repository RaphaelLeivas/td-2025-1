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
%   Define a solução inicial.
% =========================================================================


function [x,custo,n] = initialSol()

% Problema de Designação Simples:
% Considere que existem n tarefas e n agentes, de tal forma
% que cada tarefa deve ser atribuída a um agente e cada
% agente só pode receber uma tarefa. A execução da tarefa i
% pelo agente j tem um custo cij. Formule um problema que
% atribua as tarefas de forma a minimizar o custo total de
% execução.

n = 50;

% custo = [   t1 t2 ... tn
%          a1
%          a2
%          ...
%          an              ]
custo = 100*rand(n,n);

%      a1 a2 ... an
% x = [tx ty ... tz]
x = randperm(n);


