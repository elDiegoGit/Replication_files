% Simple data augmentation routine to estimate missing energy data points
% Copyright (2009) Pedro Silos and Diego Vil�n.
%
% --------------Housekeeping: ----------------------

close all;
clear all;
clc;
format bank;

% Define coefficients (via STATA):
% cGA = 21757.57;
% cFL = 10325.35 ;
% cAL =  23424.8;
% cTN = -13347.65 ;
% cLA = 346.8771 ;
% cMS = 8402.445;

% Define coefficients:
cGA =  1.0e+004 * 2.9355;
cFL = 1.0e+004 * 1.6381;
cAL = 1.0e+004 * 2.9814;
cTN = 1.0e+004 * 3.6847;
cLA = 1.0e+004 * 2.7976;
cMS = 1.0e+004 * 1.3714;
    
% Load Energy data:
load ip.txt;

% Forecast:
indx_model = menu('Select how many months to estimate:', '[a] - 1 month', '[b] - 2 months');

if indx_model == 1;
    
GAhat = ip(length(ip))*cGA;
FLhat = ip(length(ip))*cFL;
ALhat = ip(length(ip))*cAL;
TNhat = ip(length(ip))*cTN;
LAhat = ip(length(ip))*cLA;
MShat = ip(length(ip))*cMS;

Results = [GAhat;FLhat;ALhat;TNhat;LAhat;MShat]

display ('NOTE: Remember the ordering of the states is GA, FL, AL, TN, LA, MS.')

elseif indx_model == 2;

Z = [ip(length(ip)-1);ip(length(ip))];

GAhat = Z*cGA;
FLhat = Z*cFL;
ALhat = Z*cAL;
TNhat = Z*cTN;
LAhat = Z*cLA;
MShat = Z*cMS;

Results = [GAhat;FLhat;ALhat;TNhat;LAhat;MShat]

display ('NOTE: Remember the ordering of the states is GA, FL, AL, TN, LA, MS.')

end

    
    