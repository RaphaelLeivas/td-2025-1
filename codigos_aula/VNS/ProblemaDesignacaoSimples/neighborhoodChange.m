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
%   Atualiza a solução corrente e a estrutura de vizinhança.
% =========================================================================


function [x,jx,k] = neighborhoodChange(x,jx, y,jy, k)

if jy < jx
    x  = y;
    jx = jy;
    k  = 1;
else
    k  = k + 1;
end
