function  [Zsave,Psave]= kfilter(y,F,H,Q,R,X,A);

% this procedure uses filter from Hamilton's text */
% this procedure allows for a constant term or other exogenous regressors */
% Kalman Filter for State Space Model of Form:
%   This version switches to the steady state state forecast error
%    after Ptgtm1 converges

%   Z(t+1)=F*Z(t)+v(t+1)        (state equation)
%   y(t)=A'*x(t)+H'*Z(t)+w(t)   (observation equation)

%   with E(v(t)*v(t)')=Q
%   and  E(w(t)*w(t)')=R  */


capT=cols(y);   %  number of observations */
rowF=rows(F);   %  dimension of state vector */
sstate=0;

% store estimates of state mean and variance from filter */
Psave=zeros(rowF,rowF*capT);
Zsave=zeros(rowF,capT);

% start recursion at unconditional mean and variance*/
Ztgtm1=zeros(rowF,1);
Ptgtm1=reshape(inv(eye(rowF^2)-(kron(F,F)))*vec(Q),rowF,rowF);
ptgttest=zeros(rows(Ptgtm1),cols(Ptgtm1));
ptgt=Ptgtm1;
% Begin Kalman Filter Loop 
i=1;
while i<=capT;  % consider i equivalent to t in Hamilton @
   
   % get mean and variance of state at t given info at t 
   
   if sstate==0;
      ihphr=pinv(H'*Ptgtm1*H+R);
      yresid=y(:,i)-A'*X(:,i)-H'*Ztgtm1;
      Ztgt=Ztgtm1+Ptgtm1*H*ihphr*yresid;
      ptgt=Ptgtm1-Ptgtm1*H*ihphr*H'*Ptgtm1;
      if (sum(sum(abs(ptgttest-ptgt)))/sum(diag(ptgt)))<= 0.00000000001; 
		        sstate=1;
                ptgtss=ptgt;
                Ptgtm1ss=Ptgtm1;
                ihphrss=ihphr;
      end;
    % forecast of state used in next period */
     Ztgtm1=F*Ztgtm1+F*Ptgtm1*H*ihphr*yresid;
     Ptgtm1=F*ptgt*F'+Q; 
    else; 
      yresid=y(:,i)-A'*X(:,i)-H'*Ztgtm1;
      Ztgt=Ztgtm1+Ptgtm1ss*H*ihphrss*yresid;
    % forecast of state used in next period */
     Ztgtm1=F*Ztgtm1+F*Ptgtm1ss*H*ihphrss*yresid;
    end

   % Store Stuff   
   Zsave(:,i)=Ztgt;   
   Psave(1:rowF,((i-1)*rowF)+1:((i-1)*rowF)+rowF)=ptgt;
   ptgttest=ptgt;
   
   i=i+1;
end;

Zsave=Zsave';


 