% Code to graph the recession shading in a Matlab plot for D6 2.0:
% Copyright (2009) Diego Vilán.

% Define recession dates:
rec_dates=[
    1973+10/12 1975+2/12
    1980 1980+6/12
    1981+6/12 1982+10/12
    1990+6/12 1991+2/12
    2001+2/12 2001+10/12
    2007+12/12 2008+12/12];

% Define axis data:
axis([1981 2010 -5.5 6]);
AxisDat=axis;

% Plot recession shades:
hold on;
for RecessionNumber=1:length(rec_dates);
    rectangle
    %rectangle('Position', [rec_dates(i,1) 0 (rec_dates(i,2)-rec_dates(i,1)) max(x(:, varnumber))], 'FaceColor','y');
    rectangle('Position', [rec_dates(RecessionNumber,1) AxisDat(3) (rec_dates(RecessionNumber,2)-rec_dates(RecessionNumber,1)) (AxisDat(4)-AxisDat(3))], 'FaceColor','y');
end

% Plot data:
dates = 1981:1/12:[2008.91666666667;];
dates(1:2)=[];
plot(dates, zeros(length(dates),1), 'k:')
hold on
plot(dates, facest(1:length(dates),2), 'r', 'LineWidth', 2);
hold off
xlabel('Year');
ylabel('D6 Index');

