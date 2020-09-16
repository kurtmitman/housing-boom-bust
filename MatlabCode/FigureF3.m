%%
clc
clear all; close all;
global InputBaseDir SaveDir expname ltvD ownD refiD relphD ndcD foreD rPratioD hstartD trangeq CodeDir BenchC BenchPh BaseC BasePh BaseLev BaseRefi BaseFore BaseOwn BasePr
CodeDir = pwd;
cd ../Data
InputBaseDir='~/Dropbox/Housing-Boom-Bust/';
DataDir = pwd;
cd ../
cd Figures/AdditionalFigures
SaveDir = pwd;
cd(CodeDir)


%%
%Load XLS data
cd(DataDir);
data     = xlsread('data_shocks.xls','dataplot');
ltvD      = data(:,1)/data(1,1);  %LTV 
ownD      = data(:,3)/data(1,3);  %home ownership rate
hstartD   = data(:,4)/data(1,4);  %housing starts
refiD     = data(:,5)/data(6,5);  %refinancing
relphD    = exp(data(:,6))./exp(data(1,6));  %house prices detrended
ndcD      = exp(data(:,7))./exp(data(1,7));  %nondurableC detrended
foreD     = data(:,8)*8./data(:,3)*100/.7; %foreclosure rate
rPratioD  = data(:,10)./data(1,10);
trangeq   = linspace(1997,2015,size(ndcD,1));
cd(CodeDir);

%%
set(0,'DefaultLineLineWidth',5)
set(0,'DefaultTextFontSize', 16)

savefigs = 0;
FirstFigNo = 1;

Jtot=30; %Ages
Jwork=22;
tlen=999; %Time periods

indices{1}=[1]; %Number of regions

DoMRS = 0; %1 if want cross region plots, 0 if want Agg across region plots
DoModify = 1; %1 if doing modification
DoLifecycle = 0;



Prefix = '';
figname = 'NoRentalBelief';
figname2 = 'NoRentalBelief';



if(length(indices{1})==2)
    type{1} = [Prefix figname '/Results/' Prefix figname '_alpha_0.3']; 
    type{2} = [Prefix figname '/Results/' Prefix figname '_alpha_0.8'];
elseif(length(indices{1})==5)
    type{1} = [Prefix figname '/Results/' Prefix figname '_alpha_0.3'];  
    type{2} = [Prefix figname '/Results/' Prefix figname '_alpha_0.45'];  
    type{3} = [Prefix figname '/Results/' Prefix figname '_alpha_0.6'];
    type{4} = [Prefix figname '/Results/' Prefix figname '_alpha_0.7'];  
    type{5} = [Prefix figname '/Results/' Prefix figname '_alpha_0.8'];
elseif(length(indices{1})==4)
%     type{1} = [Prefix figname '/Results/' Prefix figname '_alpha_0.3'];  
%     type{2} = [Prefix figname '/Results/' Prefix figname '_alpha_0.45'];  
%     type{3} = [Prefix figname '/Results/' Prefix figname '_alpha_0.6'];
%     type{4} = [Prefix figname '/Results/' Prefix figname '_alpha_0.7'];  
    type{1} = [Prefix figname '/Results/' Prefix figname '_alpha_0.45'];  
    type{2} = [Prefix figname '/Results/' Prefix figname '_alpha_0.6'];
    type{3} = [Prefix figname '/Results/' Prefix figname '_alpha_0.7'];  
    type{4} = [Prefix figname '/Results/' Prefix figname '_alpha_0.8'];
elseif(length(indices{1})==1)
    type{1} = [Prefix figname '/Results/' Prefix figname '_alpha_0.6']; 
%     type2{1} = [Prefix figname2 '/Results/' Prefix figname2 '_alpha_0.6']; 
end

NoRent=0;

totalexperiments=length(indices);
%%
%Generate matrices for IV
dC=zeros(length(indices{1}),9);
dHNW=zeros(length(indices{1}),9);
dPH=zeros(length(indices{1}),9);

expc=1;
DoExu='';
ImportStudyOutputData2; %Import the data

index=1;weights=1;
%%
close all;
figname=figname2;

range_irf(1,1:11)=360:370; expname='Base';name{expc}=expname;%10:40; Inc
ProcessStudyOutputData2;
if(DoMRS==1)
    GraphStudyOutputDataMRS
else
    BaseC=[conI{index}]*weights;
    BasePh=[PhI{index}]*weights;
    BaseLev=[levI{index}]*weights;
    BaseFore=[foreI{index}]*weights;
    BaseHC=[conI2{index}]*weights;
    BaseRefi=[refiI{index}]*weights;
    BaseOwn=[ownI{index}]*weights;
    BasePr=[PrPhI{index}]*weights;    
end
expc=expc+1;
% keyboard

% figname='LandBelief';
range_irf(1,1:11)=100:110; expname ='Allshocks1';name{expc}=expname;
name{expc}=expname;
ProcessStudyOutputData4;
if(DoMRS==1)
    GraphStudyOutputDataMRS
else
    GraphStudyOutputDataAgg
    BenchC=[conID{index}]*weights;
    BenchPh=[PhID{index}]*weights;
    BenchLev=[levID{index}]*weights;
    BenchFore=[foreI{index}]*weights;
    BenchHC=[conID2{index}]*weights;
    BenchRefi=[refiID{index}]*weights;
    BenchOwn=[ownID{index}]*weights;
    BenchPr=[PrPhID{index}]*weights;
%         BenchHC=[hconID{index}]*weights;


end

%%
close all;
SinglePanelFigure
plot1=plot(trange,BenchOwn,trangeq,ownD,'--k');xlim([trange(1) trange(end)]);grid on;title('Home Ownership','FontSize',24);xlabel('Year'); ylim([0.95 1.15]);
legend1=legend('No Rental Belief','Data','Location','SouthWest');
set(legend1,'FontSize',24);

cd(SaveDir); 
        figure1.PaperPositionMode = 'auto';
fig_pos = figure1.PaperPosition;
figure1.PaperSize = [fig_pos(3) fig_pos(4)];

print(figure1,'-dpdf',[figname '_HomeOwnershipIRF.pdf'])
print(figure1,'-depsc2',[figname '_HomeOwnershipIRF.eps'],'-loose')
cd(CodeDir);


%%    
close all

SinglePanelFigure
title('Rent-Price Ratio','FontSize',24)
plot1=plot(trange,BenchPr,trangeq,rPratioD,'--k');xlim([trange(1) trange(end)]);grid on;xlabel('Year');ylim([0.65 1.25]);

    cd(SaveDir); 
            figure1.PaperPositionMode = 'auto';
    fig_pos = figure1.PaperPosition;
    figure1.PaperSize = [fig_pos(3) fig_pos(4)];


    print(figure1,'-dpdf',[figname '_PrPHIRF.pdf'])
    print(figure1,'-depsc2',[figname '_PrPHIRF.eps'],'-loose')
    cd(CodeDir);


