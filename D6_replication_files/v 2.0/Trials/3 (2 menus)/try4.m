clc;

indx_model = menu('Select a D6 model:', 'I - Version 1.0', 'II - Version 2.0');

if indx_model == 1;
    
disp('viva la pepa');

elseif indx_model == 2;

    if length(y) == length(x)/3;
        
        disp('hacer chow lin');
    
%     elseif length(y) < length(x)/3;
%         
%         disp('hacer proyecciones');
    else

        disp('Error. Please select a model from the menu');
end;
end;
