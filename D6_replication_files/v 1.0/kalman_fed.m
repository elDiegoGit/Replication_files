function  [Zsave,Psave]= kalman_fed(y,F,H,Q,R,X,A);
% F is transition equation matrix (coefficient).  Mu is the
% intercept term.  H is the coefficient of the state in the measurement equation
% and 

capT=cols(y);   %  number of observations */
rowF=rows(F);   %  dimension of state vector */
 

% store estimates of state mean and variance from filter */
Psave=zeros(rowF,rowF*capT);
Zsave=zeros(rowF,capT);

% start recursion at unconditional mean and variance*/
p_cf= zeros(rowF,1);
p_vr=reshape(inv(eye(rowF^2)-(kron(F,F)))*vec(Q),rowF,rowF);
% Begin Kalman Filter Loop 
i=1;
while i<=capT;  % consider i equivalent to t in Hamilton @
   
   % get mean and variance of state at t given info at t

      pst_cf = F*p_cf ;
      pst_vr = F*p_vr*F' + Q;
      f_cast = y(:,i) - H*pst_cf;
      ss = H*pst_vr*H' +R;
      
      K_gn = pst_vr*H'*inv(ss);
      p_cf = pst_cf + K_gn*f_cast;
      p_vr = (eye(3) - K_gn*H)*pst_vr; 
 
   % Store Stuff   
   Zsave(:,i)=p_cf;   
   Psave(1:rowF,((i-1)*rowF)+1:((i-1)*rowF)+rowF)=p_vr;
 
   
   i=i+1;
end;

Zsave=Zsave';