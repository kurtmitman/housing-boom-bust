%%
clc
clear all; close all;
global InputBaseDir SaveDir expname ltvD ownD refiD relphD ndcD foreD rPratioD hstartD trangeq CodeDir BenchC BenchPh BaseC BasePh BaseLev BaseRefi BaseFore BaseOwn BasePr
CodeDir = pwd;
cd ../Data
InputBaseDir='~/Dropbox/Housing-Boom-Bust/';
DataDir = pwd;
cd ../
cd Figures
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

indices{1}=[1 2 3]; %Number of regions

DoMRS = 0; %1 if want cross region plots, 0 if want Agg across region plots
DoModify = 0; %1 if doing modification
DoLifecycle = 0;

figname2 = 'BeliefSense';


figname = 'Benchmark';
type{1} = [figname '/Results/' figname '_alpha_0.6']; 
figname = 'NoBank';
type{2} = [figname '/Results/' figname '_alpha_0.6']; 
figname = 'OnlyBank';
type{3} = [figname '/Results/' figname '_alpha_0.6']; 


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
%%
close all;
figname=figname2;

% figname='LandBelief';
range_irf(1,1:11)=100:110; expname ='Allshocks1';name{expc}=expname;
name{expc}=expname;
ProcessStudyOutputData2;
if(DoMRS==1)
    GraphStudyOutputDataMRS
else

GraphStudyOutputDataAgg
    BenchC=[conID{index}];
    BenchPh=[PhID{index}];

    
        BenchLev=[levID{index}];
        BenchFore=[foreI{index}];
        BenchHC=[hconID{index}];
        BenchH=[houseID{index}];
        BenchRefi=[refiID{index}];
        BenchOwn=[ownID{index}];
        BenchPr=[PrPhID{index}];

end

%%

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
        hold(subplot2,'on');plot1=plot(trange,BenchPh,trangeq,relphD,'--k');xlim([trange(1) trange(end)]);title('House Price','FontSize',14);grid on;xlabel('Year');
    MakePrettyLinesSub6

        subplot2=subplot(2,3,2,'FontSize',12);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');plot1=plot(trange,BenchC,trangeq,ndcD,'--k');xlim([trange(1) trange(end)]);title('Consumption','FontSize',14);grid on;xlabel('Year');
     MakePrettyLinesSub6

        subplot2=subplot(2,3,3,'FontSize',12);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');plot1=plot(trange,BenchPr,trangeq,rPratioD,'--k');xlim([trange(1) trange(end)]);title('Rent Price Ratio','FontSize',14);grid on;xlabel('Year');
    MakePrettyLinesSub6

    subplot2=subplot(2,3,4,'FontSize',12);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');plot1=plot(trange,BenchOwn,trangeq,ownD,'--k');xlim([trange(1) trange(end)]);title('Home Ownership','FontSize',14);grid on;xlabel('Year');ylim([0.95 1.1]);
       MakePrettyLinesSub6

        subplot2=subplot(2,3,5,'FontSize',12);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');plot1=plot(trange,BenchLev,trangeq,ltvD,'--k');xlim([trange(1) trange(end)]);title('Leverage','FontSize',14);grid on;xlabel('Year');
        MakePrettyLinesSub6

        subplot2=subplot(2,3,6,'FontSize',12);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');plot1=plot(trange,BenchFore,trangeq,foreD,'--k');xlim([trange(1) trange(end)]);title('Foreclosure','FontSize',14);grid on;xlabel('Year');
        
            MakePrettyLinesSub6
            legend1=legend('Bench','No Bank','Only Bank','Data','Location','SouthWest');
        set(legend1,'FontSize',8);

        

                if(savefigs ==1)
            cd(SaveDir); 
!mkdir Figure7
cd Figure7
            figure1.PaperPositionMode = 'auto';
        fig_pos = figure1.PaperPosition;
        figure1.PaperSize = [fig_pos(3) fig_pos(4)];

            print(figure1,'-dpdf',['BeliefSense.pdf'],'-fillpage')
                        print(figure1,'-depsc2',['BeliefSense.eps'],'-loose')

            cd(CodeDir); 
                end



