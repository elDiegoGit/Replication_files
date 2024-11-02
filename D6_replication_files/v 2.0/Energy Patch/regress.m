% OLS routine to calculate energy coefficients to be used in
% contemporaneous forecasting.
% Copyright (2009) Pedro Silos and Diego Vilán.
%
% --------------Housekeeping: ----------------------

close all;
clear all;
clc;

% Load data:
load ols.txt;

% Define matrices:
X = ols(:,1);
GA = ols(:,2);
FL = ols(:,3);
AL = ols(:,4);
TN = ols(:,5);
LA = ols(:,6);
MS = ols(:,7);

% Obtain vector of OLS coefficients:
cGA = inv(X'*X)* X'*GA;
cFL = inv(X'*X)* X'*FL;
cAL = inv(X'*X)* X'*AL;
cTN = inv(X'*X)* X'*TN;
cLA = inv(X'*X)* X'*LA;
cMS = inv(X'*X)* X'*MS;

coef = [cGA;cFL;cAL;cTN;cLA;cMS]




