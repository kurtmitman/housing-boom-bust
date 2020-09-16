qm3=importdata([InputBaseDir type{1} '/' 'qm3' DoExu '.txt']);

%%

mgridraw=[           1  0.000000000000000E+000
           2  1.250000000000000E-002
           3  2.500000000000000E-002
           4  3.750000000000000E-002
           5  5.000000000000000E-002
           6  6.250000000000000E-002
           7  7.500000000000000E-002
           8  8.749999999999999E-002
           9  0.100000000000000
          10  0.112500000000000
          11  0.125000000000000
          12  0.137500000000000
          13  0.150000000000000
          14  0.164705882352941
          15  0.179411764705882
          16  0.194117647058824
          17  0.208823529411765
          18  0.223529411764706
          19  0.238235294117647
          20  0.252941176470588
          21  0.267647058823529
          22  0.282352941176471
          23  0.297058823529412
          24  0.311764705882353
          25  0.326470588235294
          26  0.341176470588235
          27  0.355882352941176
          28  0.370588235294118
          29  0.385294117647059
          30  0.400000000000000
          31  0.414705882352941
          32  0.429411764705882
          33  0.444117647058823
          34  0.458823529411765
          35  0.473529411764706
          36  0.488235294117647
          37  0.502941176470588
          38  0.517647058823529
          39  0.532352941176470
          40  0.547058823529412
          41  0.561764705882353
          42  0.576470588235294
          43  0.591176470588235
          44  0.605882352941176
          45  0.620588235294117
          46  0.635294117647059
          47  0.650000000000000
          48  0.664705882352941
          49  0.679411764705882
          50  0.694117647058823
          51  0.708823529411765
          52  0.723529411764706
          53  0.738235294117647
          54  0.752941176470588
          55  0.767647058823529
          56  0.782352941176470
          57  0.797058823529412
          58  0.811764705882353
          59  0.826470588235294
          60  0.841176470588235
          61  0.855882352941176
          62  0.870588235294117
          63  0.885294117647059
          64  0.900000000000000
];
mgridraw = mgridraw(:,2);
mgrid=mgridraw(1:3:64);

phgrid=linspace(0.3,0.9,13);

%%
phval=7;
agg1=12;
agg2=3;
inc1=3;
inc2=5;
agg3=11;
agg4=10;
agg5=6;
levgrid=mgrid/phgrid(phval);
qmp=qm3';
boomInt1=(1.076./qmp(:,(phval-1)*60+(agg2-1)*5+inc1)-1)/2;
boomInt2=(1.076./qmp(:,(phval-1)*60+(agg2-1)*5+inc2)-1)/2;

preInt1=(1.08./qmp(:,(phval-1)*60+(agg1-1)*5+inc1)-1)/2;
preInt2=(1.08./qmp(:,(phval-1)*60+(agg1-1)*5+inc2)-1)/2;

creditInt1=(1.076./qmp(:,(phval-1)*60+(agg3-1)*5+inc1)-1)/2;
creditInt2=(1.076./qmp(:,(phval-1)*60+(agg3-1)*5+inc2)-1)/2;

beliefInt1=(1.08./qmp(:,(phval-1)*60+(agg4-1)*5+inc1)-1)/2;
beliefInt2=(1.08./qmp(:,(phval-1)*60+(agg4-1)*5+inc2)-1)/2;

incInt1=(1.08./qmp(:,(phval-1)*60+(agg5-1)*5+inc1)-1)/2;
incInt2=(1.08./qmp(:,(phval-1)*60+(agg5-1)*5+inc2)-1)/2;

preInt2(16)=(1.08./(qmp(16,(phval-1)*60+(agg3-1)*5+inc2)*.98)-1)/2;
preInt1(16)=(1.08./(qmp(16,(phval-1)*60+(agg3-1)*5+inc2)*.98)-1)/2;
preInt2(15)=(1.08./(qmp(15,(phval-1)*60+(agg3-1)*5+inc2)*.98)-1)/2;
preInt1(15)=(1.08./(qmp(15,(phval-1)*60+(agg3-1)*5+inc2)*.98)-1)/2;
levgrid2=linspace(0.5,1.10,100);
levgrid3=linspace(0.5,0.95,100);


boomInt3=interp1(levgrid(1:16),boomInt2(1:16),levgrid2,'pchip');
preInt3=interp1(levgrid(1:15),preInt2(1:15),levgrid3,'pchip');
creditInt3=interp1(levgrid(1:16),creditInt2(1:16),levgrid2,'pchip');



%%


close all;
figure1=figure;
orient('landscape');
axes1 = axes('Parent',figure1,'FontSize',20);
box(axes1,'on');
grid(axes1,'on');
hold(axes1,'on');

plot(levgrid3,preInt3,levgrid2,creditInt3,':',levgrid2,boomInt3,'-.',levgrid2,0.03*ones(100,1),'--k');xlim([0.65 1.1]);xlabel('Leverage','FontSize',20);ylabel('Endogenous Borrowing Rate (1/q-1)','FontSize',20);grid on;
ylim([0.025 0.15])
legend('No shocks','Credit Only','All Shocks','Location','NorthWest')

cd(SaveDir); 
!mkdir Figure8
cd Figure8
print(figure1,'-dpdf',['MortRate.pdf'])
print(figure1,'-depsc2',['MortRate.eps'],'-loose')

