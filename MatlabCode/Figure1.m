close all
BaseDir = pwd;
cd ../Figures
!mkdir Figure1
cd Figure1

set(0,'DefaultLineLineWidth',8)
set(0,'DefaultTextFontSize', 32)

figname='Benchmark';
trange = 1997:2:2017;
figure1 = figure;
    orient('landscape')
    % Create axes
    axes1 = axes('Parent',figure1,'FontSize',32);
    box(axes1,'on');
    grid(axes1,'on');
    hold(axes1,'on');
    Zgrid(1)=0.965;
    Zgrid(2:6)=1.035;
    Zgrid(7:11)=0.965;
    Rgrid(1)=0.0606;
    Rgrid(2:6)=0.0202;
    Rgrid(7:11)=0.0202;
    Kgrid(1)=0.01;
    Kgrid(2:6)=0.0;
    Kgrid(7:11)=0.01;
    Lgrid(1:2)=0.95;
    Lgrid(3:6)=1.1;
    Lgrid(7:11)=0.95;
    Egrid(1:3)=0.01;
    Egrid(4:6)=0.8;
    Egrid(7:11)=0.01;
    
    %%
    trange = 1997:2:2017;

    figure1 = figure;
    orient('landscape')
    % Create axes
    axes1 = axes('Parent',figure1,'FontSize',32);
        box(axes1,'on');
    grid(axes1,'on');
    hold(axes1,'on');

    plot(trange,Zgrid);xlim([trange(1) trange(end)]);title('Productivity, \Theta','FontSize',32);grid on;xlabel('Year');
     
    print(figure1,'-dpdf',[figname '_Shocks1.pdf'],'-fillpage')
    print(figure1,'-depsc2',[figname '_Shocks1.eps'],'-loose')
    savefig([figname '_Shocks1'])
  

   

%%

figure1 = figure;
    orient('landscape')
    % Create axes
    axes1 = axes('Parent',figure1,'FontSize',32);
        box(axes1,'on');
    grid(axes1,'on');
    hold(axes1,'on');

    plot(trange,Lgrid);xlim([trange(1) trange(end)]);title('Credit Conditions - Max LTV, \lambda_m','FontSize',32);grid on;xlabel('Year');


    print(figure1,'-dpdf',[figname '_Shocks2.pdf'])
    print(figure1,'-depsc2',[figname '_Shocks2.eps'],'-loose')
    savefig([figname '_Shocks2'])
 
    
%%
    trange = 1997:2:2017;
figure1 = figure;
    orient('landscape')
    % Create axes
    axes1 = axes('Parent',figure1,'FontSize',32);

        box(axes1,'on');
    grid(axes1,'on');
    hold(axes1,'on');

        plot(trange,Egrid);title('Probability of \phi_H','FontSize',32);xlim([trange(1) trange(end)]);grid on;xlabel('Year');ylim([0 1])

 
    print(figure1,'-dpdf',[figname '_Shocks3.pdf'],'-fillpage')
    print(figure1,'-depsc2',[figname '_Shocks3.eps'],'-loose')
    savefig([figname '_Shocks3'])
    
%%    
    cd(CodeDir)
    TransMats
    cd(SaveDir)
    !mkdir AdditionalFigures
    cd AdditionalFigures
    
        trange = 1997:2:2017;
figure1 = figure;
    orient('landscape')
    % Create axes
    axes1 = axes('Parent',figure1,'FontSize',32);

        box(axes1,'on');
    grid(axes1,'on');
    hold(axes1,'on');

        plot(trange,expgrowth);title('Expected Price Growth','FontSize',32);xlim([trange(1) trange(end)]);grid on;xlabel('Year');

 
    print(figure1,'-dpdf',[figname '_EPh.pdf'],'-fillpage')
    print(figure1,'-depsc2',[figname '_EPh.eps'],'-loose')
    
    
   cd(BaseDir);
    