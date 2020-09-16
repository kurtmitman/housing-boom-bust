%%
ngpC=2;
transMatC(1,1)=0.99;
transMatC(ngpC,ngpC)=0.99;
transMatC(1,ngpC)=1.0-transMatC(1,1);
transMatC(ngpC,1)=1.0-transMatC(ngpC,ngpC);

ngpAY=2;
transMatAY(1,1)=0.90;
transMatAY(ngpAY,ngpAY)=0.90;
transMatAY(1,ngpAY)=1.0-transMatAY(1,1);
transMatAY(ngpAY,1)=1.0-transMatAY(ngpAY,ngpAY);

ngpHD=3;
transMatHD(1,1)=0.95;
transMatHD(ngpHD,ngpHD)=0.95;
transMatHD(ngpHD,ngpHD-1)=1.0-transMatHD(ngpHD,ngpHD)-0.01;
transMatHD(ngpHD,1)=0.01;
transMatHD(ngpHD-1,1)=0.85;
transMatHD(ngpHD-1,ngpHD)=0.025;
transMatHD(ngpHD-1,ngpHD-1)=0.125;
transMatHD(1,ngpHD-1)=1.0-transMatHD(1,1)-0.01;
transMatHD(1,ngpHD)=0.01;

iAgg=0;
for iAy=1:ngpAY
    for iHD=1:ngpHD
        for iC=1:ngpC
            iAgg=iAgg+1;
            iAggp=0;
            for iAyp=1:ngpAY
            for iHDp=1:ngpHD
            for iCp=1:ngpC
		            iAggp=iAggp+1;
                    transMatAgg(iAgg,iAggp)=transMatAY(iAy,iAyp)*transMatHD(iHD,iHDp)*transMatC(iC,iCp);
            end
            end
            end
        end
    end
end

expgrowth=zeros(11,1);
expPh=zeros(11,1);
for i=100:110
expgrowth(i-99)=((transMatAgg(pathiAgg(i),:)*exp(a0((pathiAgg(i)-1)*12+1:pathiAgg(i)*12)+a1((pathiAgg(i)-1)*12+1:pathiAgg(i)*12)*log(pathPh{1}(i))))/(pathPh{1}(i))-1)*2;
expPh(i-99)=(transMatAgg(pathiAgg(i),:)*exp(a0((pathiAgg(i)-1)*12+1:pathiAgg(i)*12)+a1((pathiAgg(i)-1)*12+1:pathiAgg(i)*12)*log(pathPh{1}(i))));
end


