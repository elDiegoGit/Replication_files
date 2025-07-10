function l = likelihood(y,T,param,m,h)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function obtains the likelihood function of the model
%
% y_t = sqrt(exp(alpha0 + alpha1*x_t))*u_t
%
% x_t = rho*x_{t-1} + sqrt(1-rho^2)*v_t, -1 < rho < 1
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
% Output:
% l: log-likelihood function
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Setting parameters
alpha0 = param(1);
alpha1 = param(2);
x0 = param(3);
rho = param(4);

%% Initializing density filter
[X,W]=lgwt(m,x0-h,x0+h);    % Gauss-Legendre nodes and weights around x0
% W = W/sum(W);               % Normalizing weights to sum to one
% p0 = W.*normpdf(X);
p0 = W.*normpdf(X)/sum(W.*normpdf(X));
pxx = normpdf(X,rho*X',sqrt(1-rho^2));

%% Obtaining likelihood function
l = 0;
for t = 1:T
    p1 = pxx*p0;                                        % prediction step
    pyx = normpdf(y(t),0,sqrt(exp(alpha0+alpha1*X)));   % conditional density of y
    p0 = pyx.*p1.*W;                                    % updating step
    ct = sum(p0);                                       % likelihood at t                               
    p0 = p0/ct;                                         % normalizing updating step density
    l = l + log(ct);                                    % log-likelihood  
%     X_1 = X;                                            % defining x_{t-1}
%     Ex = X'*p0;                                         % E(x_t|F_t) [update x]
%     [X,W]=lgwt(m,Ex-h,Ex+h);                            % Gauss-Legendre nodes and weights around Ex
%     W = W/sum(W);                                       % normalizing weights
%     pxx = normpdf(X,rho*X_1',sqrt(1-rho^2));            % obtaining p(x_t|x_{t-1})
end
l = -l;
end

