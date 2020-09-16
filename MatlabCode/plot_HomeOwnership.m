%*********************************
%PROGRAM 
%**********************************
close all;
%% Read data
cd ../Data/
data        = xlsread('CensusBureau_HomeOwnershipRate.xlsx','Summary','M2:P7');
age         = data(:,1);
cd ../MatlabCode/

ho_preboom_data  = data(:,2)/100;  
ho_boom_data     = data(:,3)/100;  
ho_bust_data     = data(:,4)/100;  

ho_preboom_model  = [mean(agepathEown{1}(1:7,100));mean(agepathEown{1}(8:12,100));mean(agepathEown{1}(13:17,100));mean(agepathEown{1}(18:22,100));mean(agepathEown{1}(23:30,100))];      
ho_boom_model     = [mean(agepathEown{1}(1:7,104));mean(agepathEown{1}(8:12,104));mean(agepathEown{1}(13:17,104));mean(agepathEown{1}(18:22,104));mean(agepathEown{1}(23:30,104))];
ho_bust_model     = [mean(agepathEown{1}(1:7,107));mean(agepathEown{1}(8:12,107));mean(agepathEown{1}(13:17,107));mean(agepathEown{1}(18:22,107));mean(agepathEown{1}(23:30,107))];



dloghomean_boom_data = 0; 
dloghomean_boom_model = 0; 
dloghomean_bust_data = 0; 
dloghomean_bust_model = 0; 

set(0,'DefaultLineLineWidth',4)
set(0,'DefaultTextFontSize', 16)

%% Log change
dlogho_boom_data = log(ho_boom_data)-log(ho_preboom_data) - dloghomean_boom_data;
dlogho_bust_data = log(ho_bust_data)-log(ho_boom_data) - dloghomean_bust_data;

dlogho_boom_model = log(ho_boom_model)-log(ho_preboom_model) - dloghomean_boom_model;
dlogho_bust_model = log(ho_bust_model)-log(ho_boom_model) - dloghomean_bust_model;



%% Plot
figure(1);
subplot(1,2,1);
p1=plot(age,dlogho_boom_data,'k:');
hold on;
plot(age,dlogho_boom_model,'Color',[0, 0.4470, 0.7410]);
grid on;
xlim([30,70]);
ylim([-0.02,0.14]);
xlabel('Age', 'fontsize', 14);
ylabel('Log-change', 'fontsize', 14);
title('Boom', 'fontsize', 14);
h1_legend = legend('Data','Model');
set(h1_legend,'FontSize',14);

subplot(1,2,2);
p1 = plot(age,dlogho_bust_data,'k:');
hold on;
plot(age,dlogho_bust_model,'Color',[0, 0.4470, 0.7410]);
grid on;
xlim([30,70]);
ylim([-0.14,0.02]);
YTick = [-0.15; -0.1; -0.05; 0; 0.05];
xlabel('Age', 'fontsize', 14);
ylabel('Log-change', 'fontsize', 14);
title('Bust', 'fontsize', 14);
h2_legend = legend('Data','Model');
set(h2_legend,'FontSize',14);

cd ../Figures
!mkdir Figure4
cd Figure4
print(1,'-dpdf','dhomeown_byage.pdf','-fillpage')
print(1,'-depsc2','dhomeown_byage.eps','-loose')    
cd(CodeDir);

