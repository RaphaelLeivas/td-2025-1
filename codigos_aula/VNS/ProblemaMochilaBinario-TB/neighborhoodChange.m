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


function [x,xr,xc,gx,k] = neighborhoodChange(x,xr,xc,gx, y,yr,yc,gy, k)

if ybetterthanx(xr,gx, yr,gy) == true
    x  = y;
    xr = yr;
    xc = yc;
    gx = gy;
    k  = 1;
else
    k  = k + 1;
end
end


function flag = ybetterthanx(xr,gx, yr,gy)

flag = false;
if gx <= 0 && gy <= 0
    if yr > xr
        flag = true;
    end
elseif gx > 0 && gy <= 0
    flag = true;
elseif gx > 0 && gy > 0
    if gy < gx
        flag = true;
    end
end
end
