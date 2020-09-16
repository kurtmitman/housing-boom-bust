global InputBaseDir SaveDir expname ltvD ownD refiD relphD ndcD foreD hstartD rPratioD trangeq
for i=1:1
    clear index;
    index = indices{i};
    set(0,'DefaultLineLineWidth',4)
    set(0,'DefaultTextFontSize', 18)
    
    trange=1:20; %1:20;


close all

trange=0:2:20;
trange=1997:2:2017;


% Create figure
figure1 = figure;
orient('landscape')
% Create axes
axes1 = axes('Parent',figure1,'FontSize',16);
box(axes1,'on');
grid(axes1,'on');
hold(axes1,'on');
%Generate the weights to average across regions, each region gets equal
%weight
weights=ones(length(index),1)/length(index);

trough=8; peak=6;
for i=1:length(indices{1})
    HHNWI(:,i)=(log(PhID{index(i)}(:))-log(PhID{index(i)}(peak)))*PhI{index(i)}(peak)*houseI{index(i)}(peak)./(hnwI{index(i)}(peak)+assI{index(i)}(peak))-(log(PhID{index(i)}(1))-log(PhID{index(i)}(peak)))*PhI{index(i)}(peak)*houseI{index(i)}(peak)./(hnwI{index(i)}(peak)+assI{index(i)}(peak))+1;
end

switch expname
    case {'RfBelief','IncOnly', 'FundamentalOnly','FinDeregBelief','IncBelief'}
        subplot2=subplot(2,3,4,'FontSize',16);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');plot(trange,[levID{index}]*weights);xlim([trange(1) trange(end)]);title('Leverage','FontSize',18);grid on;xlabel('Years');ylim([0.8 1.8]);
        subplot2=subplot(2,3,5,'FontSize',16);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');plot(trange,[refiID{index}]*weights);xlim([trange(1) trange(end)]);title('Refinancing','FontSize',18);grid on;xlabel('Years');ylim([0 4]);
        subplot2=subplot(2,3,6,'FontSize',16);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');plot(trange,[foreI{index}]*weights);xlim([trange(1) trange(end)]);title('Foreclosure rate','FontSize',18);grid on;xlabel('Years');ylim([0 0.025]);
%         subplot(2,2,4);plot(trange,[locID{index}]*weights);xlim([trange(1) trange(end)]);title('HELOCs');grid on;xlabel('Years');ylim([0 2]);
%         subplot(2,3,4);plot(trange,[locposID{index}]*weights);xlim([trange(1) trange(end)]);title('HELOCs','FontSize',14);grid on;xlabel('Years');ylim([0 3]);
%          subplot(2,3,5);plot(trange,[PrPhID{index}]*weights);xlim([trange(1) trange(end)]);title('Rent-Price ratio','FontSize',14);grid on;xlabel('Years');ylim([0.65 1.151]);
         subplot2=subplot(2,3,3,'FontSize',16);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');plot(trange,[ownID{index}]*weights);xlim([trange(1) trange(end)]);title('Home Ownership','FontSize',18);grid on;xlabel('Years'); ylim([0.95 1.1]);
          subplot1=subplot(2,3,1,'FontSize',16);
        box(subplot1,'on');
        grid(subplot1,'on');
        hold(subplot1,'on');
        plot(trange,[PhID{index}]*weights,trangeq,relphD,'--k');xlim([trange(1) trange(end)]);title('House Price','FontSize',18);grid on;xlabel('Years'); ylim([0.8 1.375]);
        
%         legend1=legend('Model','Data','Location','SouthWest');
%         set(legend1,'FontSize',16);
        subplot2=subplot(2,3,2,'FontSize',16);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');
        plot(trange,[conID{index}]*weights,trangeq,ndcD,'--k');xlim([trange(1) trange(end)]);title('Consumption','FontSize',18);grid on;xlabel('Year');ylim([0.95 1.1]);
    subplot2=subplot(2,3,5,'FontSize',16);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');plot(trange,[PrPhID{index}]*weights,trangeq,rPratioD,'--k');xlim([trange(1) trange(end)]);title('Rent/Price Ratio','FontSize',18);grid on;xlabel('Years'); 


    case {'Allshocks1', 'Allshocks2', 'Allshocks3','Allshocks4','BeliefOnly','DemandOnly','Modify95','RfOnly','FinDeregOnly'}
         subplot1=subplot(2,3,1,'FontSize',16);
        box(subplot1,'on');
        grid(subplot1,'on');
        hold(subplot1,'on');
        plot(trange,[PhID{index}]*weights,trangeq,relphD,'--k');xlim([trange(1) trange(end)]);title('House Price','FontSize',18);grid on;xlabel('Years'); ylim([0.8 1.375]);
        
%         legend1=legend('Model','Data','Location','SouthWest');
%         set(legend1,'FontSize',16);
        subplot2=subplot(2,3,2,'FontSize',16);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');
        plot(trange,[conID{index}]*weights,trangeq,ndcD,'--k');xlim([trange(1) trange(end)]);title('Consumption','FontSize',18);grid on;xlabel('Year');ylim([0.95 1.1]);
    
        subplot2=subplot(2,3,4,'FontSize',16);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');plot(trange,[levID{index}]*weights,trangeq,ltvD,'--k');xlim([trange(1) trange(end)]);title('Leverage','FontSize',18);grid on;xlabel('Years');ylim([0.8 1.8]);
%         subplot2=subplot(3,3,5,'FontSize',16);
%         box(subplot2,'on');
%         grid(subplot2,'on');
%         hold(subplot2,'on');plot(trange,[refiID{index}]*weights,trangeq,refiD,'--k');xlim([trange(1) trange(end)]);title('Refinancing','FontSize',18);grid on;xlabel('Years');ylim([0 4]);
        subplot2=subplot(2,3,6,'FontSize',16);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');plot(trange,[foreI{index}]*weights,trangeq,foreD,'--k');xlim([trange(1) trange(end)]);title('Foreclosure rate','FontSize',18);grid on;xlabel('Years');ylim([0 0.025]);
        legend1=legend('Model','Data','Location','Best');
        set(legend1,'FontSize',16);
%         subplot(2,2,4);plot(trange,[locID{index}]*weights);xlim([trange(1) trange(end)]);title('HELOCs');grid on;xlabel('Years');ylim([0 2]);
%         subplot(2,3,4);plot(trange,[locposID{index}]*weights);xlim([trange(1) trange(end)]);title('HELOCs','FontSize',14);grid on;xlabel('Years');ylim([0 3]);
%         subplot(2,3,5);plot(trange,[PrPhID{index}]*weights,trangeq,rPratioD,'--k');xlim([trange(1) trange(end)]);title('Rent-Price ratio','FontSize',14);grid on;xlabel('Years');ylim([0.65 1.151]);
%         subplot(2,2,4);plot(trange,[ownID{index}]*weights,trangeq,ownD,'--k');xlim([trange(1) trange(end)]);title('Home Ownership','FontSize',14);grid on;xlabel('Years'); ylim([0.95 1.1]);
     subplot2=subplot(2,3,3,'FontSize',16);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');plot(trange,[ownID{index}]*weights,trangeq,ownD,'--k');xlim([trange(1) trange(end)]);title('Home Ownership','FontSize',18);grid on;xlabel('Years'); ylim([0.95 1.1]);
    subplot2=subplot(2,3,5,'FontSize',16);
        box(subplot2,'on');
        grid(subplot2,'on');
        hold(subplot2,'on');plot(trange,[PrPhID{index}]*weights,trangeq,rPratioD,'--k');xlim([trange(1) trange(end)]);title('Rent/Price Ratio','FontSize',18);grid on;xlabel('Years'); 

        
        
        %subplot(2,3,5);plot(trange,[HinvID{index}]*weights,trangeq,hstartD,'--k');xlim([trange(1) trange(end)]);title('Housing Investment');grid on;xlabel('Years');ylim([0.4 1.6]);
end

switch expname
    case {'Allshocks1', 'Allshocks2', 'Allshocks3','Allshocks4','Modify95','RfOnly','FinDeregOnly'}
    if(savefigs ==1)
        cd(SaveDir); 
                        figure1.PaperPositionMode = 'auto';
        fig_pos = figure1.PaperPosition;
        figure1.PaperSize = [fig_pos(3) fig_pos(4)];

        print(figure1,'-dpdf',[figname '_FullIRF_' expname DoExu 'Agg.pdf'])
%         print(figure1,'-depsc2',[figname '_IRF2_' expname DoExu 'Agg.eps'],'-loose')
        cd(CodeDir); 
    end
end
% concon=[conID{index}]*weights;
% phph=[PhID{index}]*weights;
% trough=1; peak=6;
% boomelas=(log(concon(trough))-log(concon(peak)))/(log(phph(trough))-log(phph(peak)))
% trough=8; peak=6;
% bustelas=(log(concon(trough))-log(concon(peak)))/(log(phph(trough))-log(phph(peak)))


end

