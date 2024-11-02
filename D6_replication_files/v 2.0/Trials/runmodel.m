% Federal Reserve Bank of Atlanta Dynamic Factor model.
% Version 2.0
% Copyright (2009) Pedro Silos and Diego Vilán.
%

% --------------Housekeeping: ----------------------
close all;
clear all;
clc;


%--- Menu for selecting a model.
indx_model = menu('Select a D6 model:', 'I - Version 1.0', 'II - Version 2.0');

if indx_model == 1;
    
load ysim.txt; 
 
nvars = cols(ysim);
capT = rows(ysim);
faclag = 2; % # of lags in the factor autoregresssion.
errlag = 2; % # of lags in the errors autoregression

for i=1:nvars
ysim(:,i) = (ysim(:,i)-mean(ysim(:,i)))./std(ysim(:,i));
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
ystar(:,i) = ysim(3:rows(ysim),i) - psidraw(j-1,i,1)*ysim(2:rows(ysim)-1,i)-psidraw(j-1,i,2)*ysim(1:rows(ysim)-2,i);
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

 yest = ysim(errlag+1:rows(ysim),:);
 
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
  quasi = ysim(3:capT ,i)-gamdraw(j,i)*facvec(:,1);

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


elseif indx_model == 2;

% Chow-Lin interpolation for six state series from quarter to monthly data.


% Note that both data files datam and dataq should have a particular state
% ordering. i.e.: states should be order in the following fashion: GA, FL,
% AL, TN, LA, MS. Each state should be a new column. Each date is a new
% row.
% The quarter data is on personal income per state.


% Create C matrix:
c=zeros(116,348);
j=zeros(116)+3;
j(1,1)=0;
k=1;
for i=1:116
    c(i,1 + j(i,1)*(i-1): 3 + j(i,1)*(i-1)) = 1;
end

% Create individual quarter data from monthly observables:
load datam.txt;
%Unemployment:
UQGA = c*datam(:,1);
UQFL = c*datam(:,4);
UQAL = c*datam(:,7);
UQTN = c*datam(:,10);
UQLA = c*datam(:,13);
UQMS = c*datam(:,16);
% Employment:
EQGA = c*datam(:,2);
EQFL = c*datam(:,5);
EQAL = c*datam(:,8);
EQTN = c*datam(:,11);
EQLA = c*datam(:,14);
EQMS = c*datam(:,17);
% Average hours worked per week:
HQGA = c*datam(:,3);
HQFL = c*datam(:,6);
HQAL = c*datam(:,9);
HQTN = c*datam(:,12);
HQLA = c*datam(:,15);
HQMS = c*datam(:,18);

% Create XQ from individual quarter data series:
XQGA = [UQGA,EQGA,HQGA];
XQFL = [UQFL,EQFL,HQFL];
XQAL = [UQAL,EQAL,HQAL];
XQTN = [UQTN,EQTN,HQTN];
XQLA = [UQLA,EQLA,HQLA];
XQMS = [UQMS,EQMS,HQMS];

% Calculate OLS estimate:
load dataq.txt;
betahatGA = (XQGA'*XQGA)^-1*(XQGA'* dataq(:,1));
betahatFL = (XQFL'*XQFL)^-1*(XQFL'* dataq(:,2));
betahatAL = (XQAL'*XQAL)^-1*(XQAL'* dataq(:,3));
betahatTN = (XQTN'*XQTN)^-1*(XQTN'* dataq(:,4));
betahatLA = (XQLA'*XQLA)^-1*(XQLA'* dataq(:,5));
betahatMS = (XQMS'*XQMS)^-1*(XQMS'* dataq(:,6));

% Calculate OLS residuals:

% Obtain predicted values:
yhatGA = XQGA*betahatGA;
yhatFL = XQFL*betahatFL;
yhatAL = XQAL*betahatAL;
yhatTN = XQTN*betahatTN;
yhatLA = XQLA*betahatLA;
yhatMS = XQMS*betahatMS;

% Obtain residuals:
resGA = dataq(:,1)- yhatGA;
resFL = dataq(:,2)- yhatFL;
resAL = dataq(:,3)- yhatAL;
resTN = dataq(:,4)- yhatTN;
resLA = dataq(:,5)- yhatLA;
resMS = dataq(:,6)- yhatMS;

% Obtain residual's first order autocorrelation coefficient:
corrGA=corr(resGA(2:length(resGA)),resGA(1:length(resGA)-1));
corrFL=corr(resFL(2:length(resFL)),resFL(1:length(resFL)-1));
corrAL=corr(resAL(2:length(resAL)),resAL(1:length(resAL)-1));
corrTN=corr(resTN(2:length(resTN)),resTN(1:length(resTN)-1));
corrLA=corr(resLA(2:length(resLA)),resLA(1:length(resLA)-1));
corrMS=corr(resMS(2:length(resMS)),resMS(1:length(resMS)-1));

% Calculate "ahat" per state:
global cr
cr=corrGA;
ahatGA=fsolve(@D,1);
cr=corrFL;
ahatFL=fsolve(@D,1);
cr=corrAL;
ahatAL=fsolve(@D,1);
cr=corrTN;
ahatTN=fsolve(@D,1);
cr=corrLA;
ahatLA=fsolve(@D,1);
cr=corrMS;
ahatMS=fsolve(@D,1);

% Calculate estimate of V, Vhat per state:

vhatGA=zeros(348,348);
for i=1:348
for j=1:348
vhatGA(i,j)=ahatGA.^abs(i-j);
end
end

vhatFL=zeros(348,348);
for i=1:348
for j=1:348
vhatFL(i,j)=ahatFL.^abs(i-j);
end
end

vhatAL=zeros(348,348);
for i=1:348
for j=1:348
vhatAL(i,j)=ahatAL.^abs(i-j);
end
end

vhatTN=zeros(348,348);
for i=1:348
for j=1:348
vhatTN(i,j)=ahatTN.^abs(i-j);
end
end

vhatLA=zeros(348,348);
for i=1:348
for j=1:348
vhatLA(i,j)=ahatLA.^abs(i-j);
end
end

vhatMS=zeros(348,348);
for i=1:348
for j=1:348
vhatMS(i,j)=ahatMS.^abs(i-j);
end
end


% Calculate GLS estimator:
betahatGLSGA = (XQGA'*((c*vhatGA*c')^-1)*XQGA)^-1*XQGA'*(c*vhatGA*c')^-1* dataq(:,1);
betahatGLSFL = (XQFL'*((c*vhatFL*c')^-1)*XQFL)^-1*XQFL'*(c*vhatFL*c')^-1* dataq(:,2);
betahatGLSAL = (XQAL'*((c*vhatAL*c')^-1)*XQAL)^-1*XQAL'*(c*vhatAL*c')^-1* dataq(:,3);
betahatGLSTN = (XQTN'*((c*vhatTN*c')^-1)*XQTN)^-1*XQTN'*(c*vhatTN*c')^-1* dataq(:,4);
betahatGLSLA = (XQLA'*((c*vhatLA*c')^-1)*XQLA)^-1*XQLA'*(c*vhatLA*c')^-1* dataq(:,5);
betahatGLSMS = (XQMS'*((c*vhatMS*c')^-1)*XQMS)^-1*XQMS'*(c*vhatMS*c')^-1* dataq(:,6);


% Calculate GLS residuals:

% Obtain GLS predicted values:
yhatGLSGA = XQGA*betahatGLSGA;
yhatGLSFL = XQFL*betahatGLSFL;
yhatGLSAL = XQAL*betahatGLSAL;
yhatGLSTN = XQTN*betahatGLSTN;
yhatGLSLA = XQLA*betahatGLSLA;
yhatGLSMS = XQMS*betahatGLSMS;

% Obtain GLS residuals:
resGLSGA = dataq(:,1)- yhatGLSGA;
resGLSFL = dataq(:,2)- yhatGLSFL;
resGLSAL = dataq(:,3)- yhatGLSAL;
resGLSTN = dataq(:,4)- yhatGLSTN;
resGLSLA = dataq(:,5)- yhatGLSLA;
resGLSMS = dataq(:,6)- yhatGLSMS;

% Aggregate monthly, observable data per state:
GAM = [datam(:,1:3)];
FLM = [datam(:,4:6)];
ALM = [datam(:,7:9)];
TNM = [datam(:,10:12)];
LAM = [datam(:,13:15)];
MSM = [datam(:,16:18)];


% Chow-lin linear, unbiased estimate of the unobserved montly series per state:
yhatMGA = GAM * betahatGLSGA + vhatGA*c'*(c*vhatGA*c')^-1*resGLSGA;
yhatMFL = FLM * betahatGLSFL + vhatFL*c'*(c*vhatFL*c')^-1*resGLSFL;
yhatMAL = ALM * betahatGLSAL + vhatAL*c'*(c*vhatAL*c')^-1*resGLSAL;
yhatMTN = TNM * betahatGLSTN + vhatTN*c'*(c*vhatTN*c')^-1*resGLSTN;
yhatMLA = LAM * betahatGLSLA + vhatLA*c'*(c*vhatLA*c')^-1*resGLSLA;
yhatMMS = MSM * betahatGLSMS + vhatMS*c'*(c*vhatMS*c')^-1*resGLSMS;

chowlin = [yhatMGA,yhatMFL, yhatMAL, yhatMTN, yhatMLA, yhatMMS];

% Calculate growth rates for Chow-lin interpolation:
for i=1:length(chowlin)-12;
    j=1:6;
    chowlinyoy(i,j)=[chowlin(i+12,j)./chowlin(i,j)-1];
end

load ysim.txt; 

% Rename ysim to include Chow-lin's estimates:
ysim = [chowlinyoy,ysim];

nvars = cols(ysim);
capT = rows(ysim);
faclag = 2; % # of lags in the factor autoregresssion.
errlag = 2; % # of lags in the errors autoregression

for i=1:nvars
ysim(:,i) = (ysim(:,i)-mean(ysim(:,i)))./std(ysim(:,i));
end

% Set up number of draws:
Ndraws=5000;

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
ystar(:,i) = ysim(3:rows(ysim),i) - psidraw(j-1,i,1)*ysim(2:rows(ysim)-1,i)-psidraw(j-1,i,2)*ysim(1:rows(ysim)-2,i);
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

 yest = ysim(errlag+1:rows(ysim),:);
 
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
  quasi = ysim(3:capT ,i)-gamdraw(j,i)*facvec(:,1);

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

else
    disp('Error. Please select a model from the menu');
end;






