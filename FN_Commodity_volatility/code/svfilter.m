function [Ex, Vx] = svfilter(y,T,param,m,h)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function obtains the filtered states, x_t, of the state-space model
%
% y_t = sqrt(exp(alpha0 + alpha1*x_t))*u_t
%
% x_t = rho*x_{t-1} + v_t, rho in [0,1]
%
% u_t, v_t ~ Normal(0,1), independent of each other, x_0 ~ Normal(0,1)
%
% where y is a Tx1 vector, T is a scalar, m is the number of nodes
% for the Gauss-Legendre quadrature and param is the vector of parameters.
% We use a density filter with numerical integration.
%
% Inputs:
% y: demeaned data Tx1
% T: length of y
% m: number of Gauss-Legendre nodes
% h: half the width of the interval for numerical integration
%
% Outputs:
% Ex: filtered expected state x
% Vx: filtered variance of x
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Setting parameters
alpha0 = param(1);
alpha1 = param(2);
x0 = param(3);
rho = param(4);

%% Initializing density filter
[X,W]=lgwt(m,x0-h,x0+h);
% W = W/sum(W);               % Normalizing weights to sum to one
% p0 = W.*normpdf(X);
p0 = W.*normpdf(X)/sum(W.*normpdf(X));
pxx = normpdf(X,rho*X',sqrt(1-rho^2));

%% Obtaining filtered states
Ex = zeros(T,1);
Vx = zeros(T,1);
d1 = zeros(T,1);
d2 = zeros(T,1);
for t = 1:T
    p1 = pxx*p0;
    pyx = normpdf(y(t),0,sqrt(exp(alpha0+alpha1*X)));
    p0 = pyx.*p1.*W;
    ct = sum(p0);
    p0 = p0/ct;
%     X_1 = X;
    Ex(t) = X'*p0;                              % Expectation E(x_t|F_t)
    Vx(t) = ((X-Ex(t)).^2)'*p0;                 % Variance var(x_t|F_t)
    d1(t) = Ex(t);
    d2(t) = Vx(t);
%     [X,W]=lgwt(m,Ex(t)-h,Ex(t)+h);
%     W = W/sum(W);
%     pxx = normpdf(X,rho*X_1',sqrt(1-rho^2));
end
end