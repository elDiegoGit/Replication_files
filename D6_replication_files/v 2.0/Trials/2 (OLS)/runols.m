% Perform OLS on the electricity data:
clc;
clear all;

% Load data and define variables:
load electric.txt;
load regressors.txt;
y = electric;
x = regressors;

% Obtain OLS coefficients:

betaGA = (x(:,1:2)'*x(:,1:2))^-1*x(:,1:2)'*y(:,1);
betaFL = (x(:,3:4)'*x(:,3:4))^-1*x(:,3:4)'*y(:,2);
betaAL = (x(:,5:6)'*x(:,5:6))^-1*x(:,5:6)'*y(:,3);
betaTN = (x(:,7:8)'*x(:,7:8))^-1*x(:,7:8)'*y(:,4);
betaLA = (x(:,9:10)'*x(:,9:10))^-1*x(:,9:10)'*y(:,5);
betaMS = (x(:,11:12)'*x(:,11:12))^-1*x(:,11:12)'*y(:,6);

beta = [betaGA betaFL betaAL betaTN betaLA betaMS];

% Forma alternativa de hacerlo con un loop: (pa aprender vio)
%beta = zeros(2,6);
% j=1
% for i = 1:6;
%   beta(:,i) = (x(:,j:j+1)'*x(:,j:j+1))^-1*x(:,j:j+1)'*y(:,i);
%   j=j+1;
% end

% Perform backward interpolation on electrical consumption data:

load back.txt;

yhatGA = back(:,1:2)*betaGA;
yhatFL = back(:,3:4)*betaFL;
yhatAL = back(:,5:6)*betaAL;
yhatTN = back(:,7:8)*betaTN;
yhatLA = back(:,9:10)*betaLA;
yhatMS = back(:,11:12)*betaMS;

yhat = [yhatGA, yhatFL, yhatAL, yhatTN, yhatLA, yhatMS];

% Forma alternativa de hacerlo con loop:
% yhat = zeros(120,6);
%  i=1;
%  for j=1:6;
%      yhat(:,j) = back(:,i:i+1)*beta(:,j)';
%      i=i+1;
%  end
 