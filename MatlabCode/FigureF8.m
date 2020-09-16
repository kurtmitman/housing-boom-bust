%%
clc
clear all; close all;
global InputBaseDir SaveDir expname ltvD ownD refiD relphD ndcD foreD rPratioD hstartD trangeq CodeDir BenchC BenchPh BaseC BasePh BaseLev BaseRefi BaseFore BaseOwn BasePr
CodeDir = pwd;
cd ../Data
InputBaseDir='~/Dropbox/Housing-Boom-Bust/';
DataDir = pwd;
cd ../
cd Figures/AdditionalFigures/
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

savefigs = 1;
FirstFigNo = 1;

Jtot=30; %Ages
Jwork=22;
tlen=999; %Time periods

indices{1}=[1]; %Number of regions

DoMRS = 0; %1 if want cross region plots, 0 if want Agg across region plots
DoModify = 0; %1 if doing modification
DoLifecycle = 0;


%Generate matrices for IV
dC=zeros(length(indices{1}),9);
dHNW=zeros(length(indices{1}),9);
dPH=zeros(length(indices{1}),9);

expc=1;
DoExu='';


Prefix = '';
figname = 'FullSegmentation';
figname2 = 'NoOverlap';


% figname = '10ver_nhr2_redo';
% figname2 = 'Overlap1_NHR2';
type{1} = [Prefix figname '/Results/' Prefix figname '_alpha_0.6']; 
NoRent=0;
totalexperiments=length(indices);

%%
ImportStudyOutputData2; %Import the data
% keyboard
index=1;weights=1;
%%
close all;
figname=figname2;

range_irf(1,1:11)=360:370; expname='Base';name{expc}=expname;%10:40; Inc
ProcessStudyOutputData2;
BaseC=[conI{index}]*weights;
BasePh=[PhI{index}]*weights;
BaseLev=[levI{index}]*weights;
BaseFore=[foreI{index}]*weights;
BaseHC=[conI2{index}]*weights;
BaseRefi=[refiI{index}]*weights;
BaseOwn=[ownI{index}]*weights;
BasePr=[PrPhI{index}]*weights;    

savefigs=0;
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

range_irf(1,1:11)=240:250; expname='FinDeregOnly';name{expc}=expname;%10:40; Inc
ProcessStudyOutputData4;
BenchC=[conID{index}]*weights;
BenchPh2=[PhID{index}]*weights;
BenchLev=[levID{index}]*weights;
BenchFore=[foreI{index}]*weights;
BenchHC=[conID2{index}]*weights;
BenchRefi=[refiID{index}]*weights;
BenchOwn2=[ownID{index}]*weights;
BenchPr2=[PrPhID{index}]*weights;


savefigs=1;
    close all

    set(0,'DefaultLineLineWidth',4)
    set(0,'DefaultTextFontSize',12)

    trange=1997:2:2017;

    % Create figure
    figure1 = figure;
    orient('landscape')
    % Create axes
    axes1 = axes('Parent',figure1,'FontSize',12);
    box(axes1,'on');
    grid(axes1,'on');
    hold(axes1,'on');

    subplot2=subplot(2,3,1,'FontSize',12);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');plot(trange,BenchPr,trangeq,rPratioD,'--k');xlim([trange(1) trange(end)]);title('Rent Price Ratio','FontSize',14);grid on;%xlabel('Year');
    subplot2=subplot(2,3,2,'FontSize',12);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');plot(trange,BenchPh,trangeq,relphD,'--k');xlim([trange(1) trange(end)]);title('House Price','FontSize',14);grid on;%xlabel('Year');
    subplot2=subplot(2,3,3,'FontSize',12);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');plot(trange,BenchOwn,trangeq,ownD,'--k');xlim([trange(1) trange(end)]);title('Home Ownership','FontSize',14);grid on;%xlabel('Year');
    subplot2=subplot(2,3,4,'FontSize',12);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');plot(trange,BenchPr2,trangeq,rPratioD,'--k');xlim([trange(1) trange(end)]);%title('Rent Price Ratio','FontSize',14);grid on;xlabel('Year');
    subplot2=subplot(2,3,5,'FontSize',12);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');plot(trange,BenchPh2,trangeq,relphD,'--k');xlim([trange(1) trange(end)]);%title('House Price','FontSize',14);grid on;xlabel('Year');
    subplot2=subplot(2,3,6,'FontSize',12);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');plot(trange,BenchOwn2,trangeq,ownD,'--k');xlim([trange(1) trange(end)]);%title('Home Ownership','FontSize',14);grid on;xlabel('Year');    
        
        
        legend1=legend('Model','Data','Location','Best');
        set(legend1,'FontSize',8);

            cd(SaveDir); 
                figure1.PaperPositionMode = 'auto';
        fig_pos = figure1.PaperPosition;
        figure1.PaperSize = [fig_pos(3) fig_pos(4)];

            print(figure1,'-dpdf',[figname2 '_6Panel.pdf'],'-fillpage')
            print(figure1,'-depsc2',[figname2 '_6Panel.eps'],'-loose')
            cd(CodeDir); 










