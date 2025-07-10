%% This code estimates a stochastic volatility model
%
% The stochastic volatility process is as follows:
%
% y_t = sqrt(exp(h_t))*u_t, u_t ~ iid N(0,1)
% h_t = alpha_0 + alpha_1*x_t, alpha_1 > 0
% x_t = rho*x_{t-1} + sqrt(1-rho^2)*e_t, e_t ~ iid N(0,1), -1 < rho < 1
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [sv,param,Ex,Vx,rho] = SV(y)

    %% Parameters
    T = length(y);          % Sample size
    % Initial parameter values
    alpha0 = 0;
    alpha1 = 1;             
    x0 = 0;
    rho = 0.5;
    m = 200;                 % Number of nodes of the Gauss-Legendre quadrature
    h = 5;                   % +/- step for initializing density filter
    %% Setting up maximum likelihood estimation
    param0 = [alpha0;alpha1;x0;rho];        % Initial parameters
    lb = [-Inf;0;0;0];
    ub = [Inf;Inf;0;0.999];
    options = optimoptions(@fmincon,'Display','iter','MaxFunctionEvaluations',100000,'MaxIterations',100000);
    param = fmincon(@(param)likelihood(y,T,param,m,h),param0,[],[],[],[],lb,ub,[],options);

    %% Obtaining parameter results
    alpha0 = param(1);
    alpha1 = param(2);
    x0 = param(3);
    rho = param(4);

    %% Obtaining filtered states
    [Ex,Vx] = svfilter(y,T,param,m,h);                            % E(x_t|F_t)
%   stdevt = sqrt(exp(alpha0+alpha1*Ex + 0.5*alpha1^2*Vx));       % square root of stochastic volatility
    sv = exp(alpha0 + alpha1 * Ex );                              % Stochastic volatility

end