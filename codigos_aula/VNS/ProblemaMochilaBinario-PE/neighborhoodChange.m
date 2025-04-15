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


function [x,xr,xc,gx,xrpen,k] = neighborhoodChange(x,xr,xc,gx,xrpen, y,yr,yc,gy,yrpen, k)

if yrpen > xrpen
    x  = y;
    xr = yr;
    xc = yc;
    gx = gy;
    xrpen = yrpen;
    k  = 1;
else
    k  = k + 1;
end


