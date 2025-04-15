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
if k == 1           % exchange two random positions
    y(r(1)) = x(r(2));
    y(r(2)) = x(r(1));
elseif k == 2       % exchage three random positions
    y(r(1)) = x(r(2));
    y(r(2)) = x(r(3));
    y(r(3)) = x(r(1));    
elseif k == 3       % shift positions    
    if r(1) < r(2)
        r1 = r(1); r2 = r(2);
    else
        r1 = r(2); r2 = r(1);
    end    
    y = [y(1:r1-1) y(r1+1:r2) y(r1) y(r2+1:end)];
end
