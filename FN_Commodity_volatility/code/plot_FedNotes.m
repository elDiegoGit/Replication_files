% ------------------------------------------------------------------------
% Notes: This code plots the stochastic volatility process for the IMF
% commodity EXPORT price indices for various countries.
%
% This is the script that was used to generate the Fed Notes graph.
% ------------------------------------------------------------------------

%% Housekeeping
clear all; 
close all;
clc; 


%% Paths:
cd /Users/dv/Library/CloudStorage/Dropbox/Research/3_short_projects/1_sv_tot/
addpath('/Users/dv/Library/CloudStorage/Dropbox/Research/3_short_projects/1_sv_tot/data/')
addpath('/Users/dv/Library/CloudStorage/Dropbox/Research/3_short_projects/1_sv_tot/')


%% Make figure:

% Read the data
filename = 'ToT_SV_allcountries_FedNotes.xlsx';
[num, txt, raw] = xlsread(filename);
datesm   = 1900+(80:1/12:124+6/12)'; % Monthly dates (set depending on data avail)


% txt: first row with country names
country_names = txt(1, :);
% num: actual data (assuming starts at row 2)
data = num;

% Specify the list of 9 countries to plot (use exact names as in the file)
countries_to_plot = {'Chile', 'Costa Rica', 'El Salvador', 'Malaysia', 'Philippines', 'Nepal', 'South Africa', 'Tunisia', 'Morocco'};

% Create figure
figure;

% Loop over the 9 countries
for i = 1:9
    country = countries_to_plot{i};
    
    % Find the index of the country column
    idx = find(strcmp(country_names, country));
    
    if isempty(idx)
        warning(['Country ', country, ' not found in the file.']);
        continue;
    end
    
    % Extract data for that country
    y = data(:, idx);
    
    % Plot in the appropriate subplot
    subplot(3,3,i);
 %   plot(datesm,y, '-o');
     plot(datesm,y, 'LineWidth',2);
    set(gca,'Xlim',[1980 2024+7/12])
    grid on;
    title(country);
%    xlabel('Time');
    ylabel('%');
end

%% Export Figure:
exportfig(gcf, 'FedNotes', ...
    'filetype', 'pdf', ...
    'orientation', 'p', ...
    'resolution', 600, ...
    'fillpage', true);








