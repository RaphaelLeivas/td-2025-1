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
%   Gera uma nova solução na k-ésima vizinhança de x.
% =========================================================================


function y = shake(x,k)

y = x;
n = length(x);
r = randperm(n);

% if k == 1, change status of 01 random position
% if k == 2, change status of 02 random positions
% if k == 3, change status of 03 random positions
y(r(1:k)) = not(x(r(1:k)));    
end

