%%
% clc
clear all; close all;
global InputBaseDir SaveDir expname ltvD ownD refiD relphD ndcD foreD rPratioD hstartD trangeq CodeDir BenchC BenchPh BaseC BasePh BaseLev BaseRefi BaseFore BaseOwn BasePr
CodeDir = pwd;
cd ../Data
InputBaseDir='~/Dropbox/Housing-Boom-Bust/';
DataDir = pwd;
cd ../
!mkdir Figures
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

savefigs = 1;
FirstFigNo = 1;

Jtot=30; %Ages
Jwork=22;
tlen=999; %Time periods

indices{1}=[1]; %Number of regions

DoMRS = 0; %1 if want cross region plots, 0 if want Agg across region plots
DoModify = 0; %1 if doing modification
DoLifecycle = 0;




Prefix = '';
figname = 'Benchmark';
figname2 = 'Benchmark';


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
    type{1} = [Prefix figname '/Results/' Prefix figname '_alpha_0.45'];  
    type{2} = [Prefix figname '/Results/' Prefix figname '_alpha_0.6'];
    type{3} = [Prefix figname '/Results/' Prefix figname '_alpha_0.7'];  
    type{4} = [Prefix figname '/Results/' Prefix figname '_alpha_0.8'];
elseif(length(indices{1})==1)
    type{1} = [Prefix figname '/Results/' Prefix figname '_alpha_0.6']; 
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
plot_HomeOwnership
cd(CodeDir)
Figure1
cd(CodeDir)
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


% figname='LandBelief';
range_irf(1,1:11)=100:110; expname ='Allshocks1';name{expc}=expname;
name{expc}=expname;
ProcessStudyOutputData4;
if(DoMRS==1)
    GraphStudyOutputDataMRS
else
%     GraphStudyOutputDataAgg
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

expc=expc+1;
range_irf(1,1:11)=160:170; expname='Modify95';name{expc}=expname;
ProcessStudyOutputData4;
if(DoMRS==1)
    GraphStudyOutputDataMRS
else
%     GraphStudyOutputDataAgg
    if(DoModify==1)
        BenchC1=[conID{index}]*weights;
        BenchLev1=[levID{index}]*weights;
        BenchFore1=[foreI{index}]*weights;
        BenchPh1=[PhID{index}]*weights;
        BenchHC1=[conID2{index}]*weights;
%         BenchHC1=[hconID{index}]*weights;
        BenchOwn1=[ownID{index}]*weights;
    end
end
expc=expc+1;

range_irf(1,1:11)=200:210; expname='Modify90';name{expc}=expname;
ProcessStudyOutputData4;
if(DoMRS==1)
    GraphStudyOutputDataMRS
else
%     GraphStudyOutputDataAgg
    if(DoModify==1)
        BenchC2=[conID{index}]*weights;
        BenchLev2=[levID{index}]*weights;
        BenchFore2=[foreI{index}]*weights;
        BenchPh2=[PhID{index}]*weights;
        BenchHC2=[conID2{index}]*weights;
%         BenchHC2=[hconID{index}]*weights;
        BenchOwn2=[ownID{index}]*weights;

    end
end
expc=expc+1;

range_irf(1,1:11)=420:430; expname='FinDeregBelief';name{expc}=expname;%10:40; FD
ProcessStudyOutputData4;
if(DoMRS==1)
    GraphStudyOutputDataMRS
else
%     GraphStudyOutputDataAgg
    FinBeliefC=[conID{index}]*weights;
    FinBeliefPh=[PhID{index}]*weights;
end
expc=expc+1;




range_irf(1,1:11)=290:300; expname='IncOnly';name{expc}=expname;%10:40; FD
ProcessStudyOutputData4;
if(DoMRS==1)
    GraphStudyOutputDataMRS
else
%     GraphStudyOutputDataAgg
    IncC=[conID{index}]*weights;
    IncPh=[PhID{index}]*weights;
    IncPr=[PrPhID{index}]*weights;
        IncLev=[levID{index}]*weights;
        IncFore=[foreI{index}]*weights;
        IncHC=[conID2{index}]*weights;
        IncRefi=[refiID{index}]*weights;
        IncOwn=[ownID{index}]*weights;


end
expc=expc+1;

range_irf(1,1:11)=240:250; expname='FinDeregOnly';name{expc}=expname;%10:40; Inc
ProcessStudyOutputData4;
if(DoMRS==1)
    GraphStudyOutputDataMRS
else
%     GraphStudyOutputDataAgg
    FinDeregC=[conID{index}]*weights;
    FinDeregPh=[PhID{index}]*weights;
            FinDeregLev=[levID{index}]*weights;
        FinDeregFore=[foreI{index}]*weights;
        FinDeregHC=[conID2{index}]*weights;
        FinDeregRefi=[refiID{index}]*weights;
        FinDeregOwn=[ownID{index}]*weights;
        FinDeregPr=[PrPhID{index}]*weights;

end
expc=expc+1;

range_irf(1,1:11)=330:340; expname='FundamentalOnly';name{expc}=expname;%10:40; Rf
ProcessStudyOutputData4;
if(DoMRS==1)
    GraphStudyOutputDataMRS
else
%     GraphStudyOutputDataAgg
    FundamentalC=[conID{index}]*weights;
    FundamentalPh=[PhID{index}]*weights;

end
expc=expc+1;

range_irf(1,1:11)=390:400; expname='BeliefOnly';name{expc}=expname;%10:40; FD
ProcessStudyOutputData4;
if(DoMRS==1)
    GraphStudyOutputDataMRS
else
%     GraphStudyOutputDataAgg
    BeliefC=[conID{index}]*weights;
    BeliefPh=[PhID{index}]*weights;
    BeliefLev=[levID{index}]*weights;
    BeliefFore=[foreI{index}]*weights;
    BeliefHC=[conID2{index}]*weights;
    BeliefRefi=[refiID{index}]*weights;
    BeliefOwn=[ownID{index}]*weights;
    BeliefPr=[PrPhID{index}]*weights;


end
expc=expc+1;

range_irf(1,1:11)=440:450; expname='DemandOnly';name{expc}=expname;%10:40; FD
ProcessStudyOutputData4;
if(DoMRS==1)
    GraphStudyOutputDataMRS
else
%     GraphStudyOutputDataAgg
    DemandC=[conID{index}]*weights;
    DemandPh=[PhID{index}]*weights;

end









%%

savefigs=1;
close all;
SinglePanelFigure
plot1=plot(trange,BenchOwn,trange,BeliefOwn,'-.r',trange,IncOwn,trange,FinDeregOwn,trangeq,ownD,'--k');xlim([trange(1) trange(end)]);grid on;title('Home Ownership','FontSize',24);xlabel('Year'); ylim([0.95 1.1]);
MakePrettyLines
legend1=legend('Bench','Belief','Income','Credit','Data','Location','NorthEast');
set(legend1,'FontSize',24);

if(savefigs ==1)
    cd(SaveDir);
    !mkdir Figure4
    cd Figure4
            figure1.PaperPositionMode = 'auto';
    fig_pos = figure1.PaperPosition;
    figure1.PaperSize = [fig_pos(3) fig_pos(4)];

    print(figure1,'-dpdf',[figname '_HomeOwnershipIRF_Counterfactual_AllOnly.pdf'])
    print(figure1,'-depsc2',[figname '_HomeOwnershipIRF_Counterfactual_AllOnly.eps'],'-loose')
    savefig([figname '_HomeOwnershipIRF_Counterfactual_AllOnly'])
    cd(CodeDir);
end   


 %%
    
    
close all;
SinglePanelFigure
plot1=plot(trange,BenchLev,trange,BeliefLev,'-.r',trange,IncLev,trange,FinDeregLev,trangeq,ltvD,'--k');xlim([trange(1) trange(end)]);title('Leverage','FontSize',24);grid on;xlabel('Year');ylim([0.8 1.8]);
MakePrettyLines
legend1=legend('Benchmark','Belief','Income','Credit','Data','Location','NorthWest');
set(legend1,'FontSize',24);


if(savefigs ==1)
    cd(SaveDir); 
    !mkdir Figure5
    cd Figure5

    figure1.PaperPositionMode = 'auto';
    fig_pos = figure1.PaperPosition;
    figure1.PaperSize = [fig_pos(3) fig_pos(4)];

    print(figure1,'-dpdf',[figname '_LevIRF_Counterfactual_AllOnly.pdf'])
    print(figure1,'-depsc2',[figname '_LevIRF_Counterfactual_AllOnly.eps'],'-loose')
    savefig([figname '_LevIRF_Counterfactual_AllOnly'])
    cd(CodeDir);
end  
%%
close all;
SinglePanelFigure
plot1=plot(trange,BenchFore,trange,BeliefFore,'-.r',trange,IncFore,trange,FinDeregFore,trangeq,foreD,'--k');xlim([trange(1) trange(end)]);title('Foreclosure rate','FontSize',24);grid on;xlabel('Year');ylim([0 0.04]);
MakePrettyLines



    if(savefigs ==1)
        cd(SaveDir); 
        cd Figure5
                figure1.PaperPositionMode = 'auto';
        fig_pos = figure1.PaperPosition;
        figure1.PaperSize = [fig_pos(3) fig_pos(4)];

        print(figure1,'-dpdf',[figname '_ForeIRF_Counterfactual_AllOnly.pdf'])
        print(figure1,'-depsc2',[figname '_ForeIRF_Counterfactual_AllOnly.eps'],'-loose')
        savefig([figname '_ForeIRF_Counterfactual_AllOnly'])
        cd(CodeDir);
    end  
    
    
    
%%    
close all

    SinglePanelFigure
    title('Rent-Price Ratio','FontSize',24)
    plot1=plot(trange,BenchPr,trange,BeliefPr,'-.r',trange,IncPr,trange,FinDeregPr,trangeq,rPratioD,'--k');xlim([trange(1) trange(end)]);grid on;xlabel('Year');ylim([0.65 1.151]);
    MakePrettyLines
    %     legend1=legend('Benchmark','Belief Only','Income Only','Credit Only','Location','SouthEast');
    legend1=legend('Benchmark','Belief','Income','Credit','Data','Location','SouthEast');

    set(legend1,'FontSize',18);
    if(savefigs ==1)
        cd(SaveDir); 
        !mkdir Figure3
        cd Figure3
                figure1.PaperPositionMode = 'auto';
        fig_pos = figure1.PaperPosition;
        figure1.PaperSize = [fig_pos(3) fig_pos(4)];

        
        print(figure1,'-dpdf',[figname '_IRF3_Counterfactual_BeliefOnlyIncOnly.pdf'])
        print(figure1,'-depsc2',[figname '_IRF3_Counterfactual_BeliefOnlyIncOnly.eps'],'-loose')
        savefig([figname '_IRF3_Counterfactual_BeliefOnlyIncOnly'])
        cd(CodeDir);
    end  
%%

    SinglePanelFigure
    plot1=plot(trange,BenchPh,trange,BeliefPh,'-.r',trange,IncPh,trange,FinDeregPh,trangeq,relphD,'--k');xlim([trange(1) trange(end)]);title('House Price','FontSize',24);grid on;xlabel('Year');ylim([0.8 1.375]);
    MakePrettyLines
    legend1=legend('Benchmark','Belief','Income','Credit','Data','Location','NorthEast');
%     legend1=legend('Bench','Belief','Inc','Credit','Data','Location','SouthWest');

    set(legend1,'FontSize',18);

        if(savefigs==1)
        cd(SaveDir); 
        cd Figure3
        figure1.PaperPositionMode = 'auto';
        fig_pos = figure1.PaperPosition;
        figure1.PaperSize = [fig_pos(3) fig_pos(4)];

        print(figure1,'-dpdf',[figname '_IRF_Counterfactual_AllOnlyPh.pdf'])
        print(figure1,'-depsc2',[figname '_IRF_Counterfactual_AllOnlyPh.eps'],'-loose')
        savefig([figname '_IRF_Counterfactual_AllOnlyPh'])
        cd(CodeDir);
        end    
    

%%        
 close all


    trange=1997:2:2017;


    SinglePanelFigure
    plot1=plot(trange,BenchC,trange,BeliefC,'-.r',trange,IncC,trange,FinDeregC,trangeq,ndcD,'--k');xlim([trange(1) trange(end)]);title('Consumption','FontSize',24);grid on;xlabel('Year');ylim([0.95 1.1]);
    MakePrettyLines
    legend1=legend('Bench','Belief','Income','Credit','Data','Location','NorthEast');
set(legend1,'FontSize',24);

    if(savefigs==1)
        cd(SaveDir);
        !mkdir Figure6
        cd Figure6
        figure1.PaperPositionMode = 'auto';
        fig_pos = figure1.PaperPosition;
        figure1.PaperSize = [fig_pos(3) fig_pos(4)];

        print(figure1,'-dpdf',[figname '_IRF_Counterfactual_AllOnlyC.pdf'])
        print(figure1,'-depsc2',[figname '_IRF_Counterfactual_AllOnlyC.eps'],'-loose')
        savefig([figname '_IRF_Counterfactual_AllOnlyC'])
        cd(CodeDir);
    end       
        
%%

                    close all
    SinglePanelFigure
        plot1=plot(trange,BenchPh,trange,DemandPh,'-.r',trangeq,relphD,'--k');xlim([trange(1) trange(end)]);title('House Price','FontSize',24);grid on;xlabel('Year');ylim([0.8 1.375]); %[0.8 1.375]
    legend1=legend('Benchmark','Preferences Only','Data','Location','SouthWest');
    set(legend1,'FontSize',18);

    if(savefigs==1)
        cd(SaveDir); 
        !mkdir Figure9
        cd Figure9
        figure1.PaperPositionMode = 'auto';
        fig_pos = figure1.PaperPosition;
        figure1.PaperSize = [fig_pos(3) fig_pos(4)];

        print(figure1,'-dpdf',[figname '_IRF_Counterfactual_DemandOnlyPh.pdf'])
        print(figure1,'-depsc2',[figname '_IRF_Counterfactual_DemandOnlyPh.eps'],'-loose')
        savefig([figname '_IRF_Counterfactual_DemandOnlyPh'])
        cd(CodeDir);
    end
    
%%    
    close all;
    SinglePanelFigure
    plot1=plot(trange,BenchC,trange,DemandC,'-.r',trangeq,ndcD,'--k');xlim([trange(1) trange(end)]);title('Consumption','FontSize',24);grid on;xlabel('Year');ylim([0.9 1.1]);
%     set(plot1(3),'LineStyle',':','Color',[0 0.498039215803146 0]);
%     set(plot1(4),    'Color',[0.301960796117783 0.745098054409027 0.933333337306976]);
    
%         legend1=legend('Benchmark','Preferences Only','Data','Location','North');
%     set(legend1,'FontSize',18);

    if(savefigs==1)
        cd(SaveDir); 
        cd Figure9
        figure1.PaperPositionMode = 'auto';
        fig_pos = figure1.PaperPosition;
        figure1.PaperSize = [fig_pos(3) fig_pos(4)];

        print(figure1,'-dpdf',[figname '_IRF_Counterfactual_DemandOnlyC.pdf'])
        print(figure1,'-depsc2',[figname '_IRF_Counterfactual_DemandOnlyC.eps'],'-loose')
        savefig([figname '_IRF_Counterfactual_DemandOnlyC'])
        cd(CodeDir);
    end
        
    
    
%%

MakeLifecycleGraphs

%%
Figure8

%%
cd(CodeDir)
