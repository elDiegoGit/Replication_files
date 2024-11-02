% Federal Reserve Bank of Atlanta Dynamic Factor model.
%
% Copyright (2009) by Silos and Vilan.
%

% --------------Housekeeping: ----------------------
close all;
clear all;
clc;


%--- Menu for selecting a model.
indx_model = menu('Select a D6 model:', 'I - Version 1.0', 'II - Version 2.0');

if indx_model == 1;

load data.txt; 
 
nvars = cols(data);
capT = rows(data);
faclag = 2; % # of lags in the factor autoregresssion.
errlag = 2; % # of lags in the errors autoregression

for i=1:nvars
data(:,i) = (data(:,i)-mean(data(:,i)))./std(data(:,i));
end

% Set up number of draws:
Ndraws=150;

facdraw = zeros(capT-faclag,Ndraws);
phidraw = zeros(Ndraws,faclag);
psidraw = zeros(Ndraws,nvars,errlag);
sigmadraw = zeros(Ndraws,nvars);
gamdraw = zeros(Ndraws,nvars);
facvec = zeros(capT-2,3);

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Initial Values   %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%

gamdraw(1,:) = 0.1*ones(nvars,1);
% [0.7 -0.3 -0.05 -0.1];
for i=1:nvars
psidraw(1,i,:) = [0.1 0.1];
end
phidraw(1,:) = [0.9];
 
 
mudraws(1,:) = [-1.1 1.3];
sigmadraw(1,:) = 0.01*ones(nvars,1);

%  [0.2;0.4; 0.1; 0.3];


for j=2:Ndraws;
    
% NOW, THE ESTIMATION PART.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% P R I O R S    %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
m_phi = [0.9 0.05];
v_phi = [0.01 0.0; 0.0 0.001];

% This is for the gammas
b0 = 0.5;
A0 = 0.1;

% This is for the psis
p0 = [0.1;0.1];
B0 = eye(2)*100;

% This is for the variances
nu0 = 6;
delta0 = 0.1;

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Set up state space Form;
%%%%%%%%%%%%%%%%%%%%%%%%%%%

lagvec = zeros(nvars,3);
H = zeros(nvars,3);
for i=1:nvars;
lagvec (i,:) = [1 -psidraw(j-1,i,1) -psidraw(j-1,i,2)];
H(i,:) = gamdraw(j-1,i)*lagvec(i,:); % H matrix
ystar(:,i) = data(3:rows(data),i) - psidraw(j-1,i,1)*data(2:rows(data)-1,i)-psidraw(j-1,i,2)*data(1:rows(data)-2,i);
end

F = [phidraw(j-1,1) phidraw(j-1,2) 0;1 0 0 ; 0 1 0];

R = diag(sigmadraw(j-1,:));
Q = zeros(3,3);
Q(1,1) = 1;
A = zeros(nvars,1);
X = ones(rows(ystar),1);
[Zsave,Psave]= kalman_fed(ystar',F,H,Q,R,X,A);
xtt_m = Zsave';
ptt_m = Psave;
% last period
Amat = chol(Psave(:,cols(Psave)-faclag:cols(Psave)));
zt = Zsave(rows(facvec),:)'+Amat'*randn(cols(Psave(:,cols(Psave)-faclag:cols(Psave))),1);
%zt = mvnrnd(Zsave(rows(facvec),:),Psave(:,cols(Psave)-3+1:cols(Psave)));
facvec(rows(facvec),:)=zt;
dimv = cols(Psave)-faclag-1;
% For other periods
i = rows(facvec)-1;
%  Carter-Kohn
while i>=3;
    jjj = rows(facvec)-1;
    jj=1;
    while jj<=1;
        f_cast0 = zt(jj,1) - F(jj,:)*xtt_m(:,i);
        ss0 = F(jj,:)*ptt_m(:,(i*(faclag+1)-faclag):i*(faclag+1))*F(jj,:)'+Q(jj,jj);
        k_gn0 = ptt_m(:,(i*(faclag+1)-faclag):i*(faclag+1))*F(jj,:)'/ss0;
        xtt_m(:,i) = xtt_m(:,i) + k_gn0*f_cast0;
        ptt_m(:,(i*(faclag+1)-faclag):i*(faclag+1)) = (eye(3)-k_gn0*F(jj,:))*ptt_m(:,(i*(faclag+1)-faclag):(faclag+1)*i);
        jj = jj+1;
    end;
    x_tmp = xtt_m(:,i);
    p_tmp = ptt_m(:,((faclag+1)*i-faclag):(faclag+1)*i);
    Amat = chol(p_tmp);
    zt = x_tmp+Amat'*randn(cols(p_tmp),1);
    %zt = mvnrnd(x_tmp,p_tmp); % This routine gives problems sometimes so I
    %draw the multivariate normal using the cholesky decomposition
    facvec(i,1) = zt(1,1);
    i=i-1;
end
facdraws(:,j) = facvec(:,1);

% Extract factor only.
 fact = facvec(:,1);
 facdraw(:,j) = fact;
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Draw parameters of the factor autoregression%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Xphi = [fact(faclag:rows(fact)-1) fact(1:rows(fact)-faclag)];


a_bar = inv(inv(v_phi)+Xphi'*Xphi)*(inv(v_phi)*m_phi'+Xphi'*fact(faclag+1:rows(fact)));
A_bar = inv(inv(v_phi)+Xphi'*Xphi);

accept = 0;
while (accept==0);
cand = mvnrnd(a_bar,A_bar);
coef = [-flipud(cand');1];            %check stationarity 
    root = roots(coef);
    rootmod = abs(root);
    if (min(rootmod)>1.001)          % Pass stationarity
        accept=1;
        phidraw(j,:) = cand;
    else
        accept =0;
    end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Draw Parameters of observable regressions %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 yest = data(errlag+1:rows(data),:);
 
 for i=1:nvars
  %%% Draw the gammas given psis and Sigma2
 % Construct Ystar and Xstar;
 Ystar = yest(errlag+1:rows(yest),i)-psidraw(j-1,i,1)*yest(errlag:rows(yest)-1,i)-psidraw(j-1,i,2)*yest(1:rows(yest)-2,i);
 Xstar = [facvec(errlag+1:rows(yest),1)-psidraw(j-1,i,1)*facvec(errlag:rows(yest)-1,1)-psidraw(j-1,i,2)*facvec(1:rows(yest)-2,1)];
  %Posterior mean(b1) and Posterior Covariance Matrix (A1)
 b1 = inv(inv(A0)+sigmadraw(j-1,i)*Xstar'*Xstar)*(inv(A0)*b0+sigmadraw(j-1,i)*Xstar'*Ystar);
 A1 = inv(inv(A0)+sigmadraw(j-1,i)*Xstar'*Xstar);
 if (i==1)&(b1<0)
     b1=0;
 else
 end
 
 %Sample Beta
 if (i==2) 
     accept=0;
     while (accept==0)
     cand = mvnrnd(b1,A1)';
     if (cand(1)>0.0) 
         gamdraw(j,i) = cand;
         accept = 1;
     else
         accept =0;
     end
  
     end
   
 else
     gamdraw(j,i) = mvnrnd(b1,A1)';
     
 end 
 
 
 
  % Draw Psi given gammas and Sigma;
 % Construct Estar
  quasi = data(3:capT ,i)-gamdraw(j,i)*facvec(:,1);

 estar = quasi(3:rows(quasi));
 Estar = [quasi(2:rows(quasi)-1) quasi(1:rows(quasi)-2)] ;
 
  % POsterior Mean (c1) and Posterio Variance (B1)
 c1 = inv(inv(B0)+sigmadraw(j-1,i)*Estar'*Estar)*(inv(B0)*p0+sigmadraw(j-1,i)*Estar'*estar);
 B1 = inv(inv(B0)+sigmadraw(j-1,i)*Estar'*Estar);
 
 
 accept =0;
 while (accept == 0);
    cand = mvnrnd(c1,B1);
    coef = [-flipud(cand')' 1];            %check stationarity 
    root = roots(coef);
    rootmod = abs(root);
    if (min(rootmod)>=1.001)
        accept =1;
         psidraw(j,i,:) = cand;
    else
       accept=0;
    end
 end
  
 Ystar = yest(3:rows(yest),i)-psidraw(j,i,1)*yest(2:rows(yest)-1,i)-psidraw(j,i,2)*yest(1:rows(yest)-2,i);
 Xstar = [facvec(3:rows(yest),1)-psidraw(j,i,1)*facvec(2:rows(yest)-1,1)-psidraw(j,i,2)*facvec(1:rows(yest)-2,1)];
 
 nu1  = nu0 + rows(Ystar);
 delta1 = delta0 + (Ystar - Xstar*gamdraw(j,i)')'*(Ystar - Xstar*gamdraw(j,i)');
 
 c = chi2rnd(nu1);
 t2 = c/delta1;
 sigmadraw(j,i) = 1/t2;
 
 end % end of nvars 
 
 if (j>100);
 zmat = corrcoef(facdraw(:,100),facdraw(:,j-1));
 if (zmat(1,2)<-0.3) ;
 j=j-1;
 else
 end
 zmat(1,2)
 else
 end

 j

end


% get factor (median) and error bands(10% and 90%);
facest = zeros(rows(facdraw),3);


for i=1:rows(facdraw)

f2 = sort(facdraw(i,:),2);

facest(i,:) = [f2(Ndraws/10) f2(Ndraws/2) f2(Ndraws-Ndraws/10)];  
    
end

plot(facest);

elseif indx_model == 2;
    display('still working on it ...!!!!');
else
    disp('Error. Please select a model from the menu');
end;






