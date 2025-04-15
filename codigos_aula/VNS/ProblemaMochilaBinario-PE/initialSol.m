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
%   Define solução inicial.
% =========================================================================


function [x,projetos,capital] = initialSol()

% Problema da Mochila Binário:
% Considere n projetos e um capital de investimento b. O
% projeto j tem um custo aj e um retorno esperado pj.
% Formule um problema que permita selecionar o portfólio
% de projetos que maximiza o retorno sem exceder os recursos
% disponíveis.
    
% projetos = [a1 a2 ... aj ... an
%             p1 p2 ... pj ... pn]
n = 100;
rnd = rand(2,n);
projetos(1,:) = 10*rnd(1,:);
projetos(2,:) = 30*rnd(2,:);
capital  = 0.4*sum(projetos(1,:));

% solução candidata inicial (aleatória)
x = round(rand(1,n));   
