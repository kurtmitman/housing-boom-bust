for jj =1:size(indices{1},2)
    j=indices{1}(jj);
    
    range1=1:Jtot;
    range2=1:tlen;
    con{j}=mean(agepathEcon{j}(range1,range2),1)';
    hcon{j}=mean(agepathEhcon{j}(range1,range2),1)';

    conown{j}=mean(agepathEconown{j}(range1,range2),1)';
    conrent{j}=mean(agepathEconrent{j}(range1,range2),1)';
    coninchigh{j}=mean(agepathEconinchigh{j}(range1,range2),1)';
    coninclow{j}=mean(agepathEconinclow{j}(range1,range2),1)';
    conhighltv{j}=mean(agepathEconhighltv{j}(range1,range2),1)';
    conlowltv{j}=mean(agepathEconlowltv{j}(range1,range2),1)';
    dh{j}=mean(agepathEdh{j}(range1,range2),1)';
    dp{j}=mean(agepathEdp{j}(range1,range2),1)';
    own{j}=mean(agepathEown{j}(range1,range2),1)';
    hnw{j}=mean(agepathEhnw{j}(range1,range2),1)';
    hnwshare{j}=mean(agepathEhnwshare{j}(range1,range2),1)';
    ass{j}=mean(agepathEass{j}(range1,range2),1)';
    lev{j}=mean(agepathElev{j}(range1,range2))'./own{j};
    inc{j}=mean(agepathEinc{j}(range1,range2),1)';
    buy{j}=mean(agepathEbuy{j}(range1,range2),1)';
    sell{j}=mean(agepathEsell{j}(range1,range2),1)'./own{j};
    refi{j}=mean(agepathErefi{j}(range1,range2),1)';
    mlp{j}=mean(agepathEmortpos{j}(range1,range2),1)';
    locpos{j}=mean(agepathElocpos{j}(range1,range2),1)'./own{j};
    fore{j}=mean(agepathEfore{j}(range1,range2),1)'./mlp{j};
    refi{j}=refi{j}+locpos{j};
    house{j}=mean(agepathEhouse{j}(range1,range2),1)';
    hcon{j}=(hcon{j}-house{j})+1.075*house{j};
%     house{j}=pathHt{j};
    loc{j}=mean(agepathEloc{j}(range1,range2),1)';
    mort{j}=mean(agepathEmort{j}(range1,range2),1);
    house{j}=mean(agepathEhouse{j}(range1,range2),1)';
%     own{j}=house{j}./own{j};
%     lev{j}=loc{j}'./own{j}+(mort{j}./(house{j}.*pathPh{j}(range2)'))';
%     lev{j}=((-loc{j}+mort{j}')./(house{j}.*pathPh{j}(range2)));

    range2=20:30;
%     range2=30:100;
%     range2=300:tlen-500;
    conL{j}=mean(agepathEcon{j}(range1,range2),2);
    hconL{j}=mean(agepathEhcon{j}(range1,range2),2);

    dhL{j}=mean(agepathEdh{j}(range1,range2),2);
    dpL{j}=mean(agepathEdp{j}(range1,range2),2);
    ownL{j}=mean(agepathEown{j}(range1,range2),2);
    ownL1{j}=mean(agepathEown{j}(range1,100),2);
    ownL2{j}=mean(agepathEown{j}(range1,105),2);
    ownL3{j}=mean(agepathEown{j}(range1,107),2);
    ownL4{j}=mean(agepathEown{j}(range1,102),2);
    hnwL{j}=mean(agepathEhnw{j}(range1,range2),2);
    hnwshareL{j}=mean(agepathEhnwshare{j}(range1,range2),2);
    assL{j}=mean(agepathEass{j}(range1,range2),2);
    levL{j}=mean(agepathElev{j}(range1,range2),2)./ownL{j};
    incL{j}=mean(agepathEinc{j}(range1,range2),2);
    penL{j}=mean(agepathEpen{j}(range1,range2),2);
    mlpL{j}=mean(agepathEmortpos{j}(range1,range2),2);
    buyL{j}=mean(agepathEbuy{j}(range1,range2),2);
    sellL{j}=mean(agepathEsell{j}(range1,range2),2)./ownL{j};
    refiL{j}=mean(agepathErefi{j}(range1,range2),2);
    foreL{j}=mean(agepathEfore{j}(range1,range2),2);
    houseL{j}=mean(agepathEhouse{j}(range1,range2),2);
    mortL{j}=mean(agepathEmort{j}(range1,range2),2);
    locL{j}=mean(agepathEloc{j}(range1,range2),2);
    housevalL{j}=mean(agepathEhouse{j}(range1,range2).*repmat(pathPh{j}(range2)',Jtot,1),2);

%     levL{j}=(locL{j}./ownL{j})+(mortL{j}./(housevalL{j}));
%     levL{j}=((mortL{j}-locL{j})./(housevalL{j}));
    levL{j}=((mortL{j})./(housevalL{j}));
    
    yL{j}=incL{j}+penL{j};
    range2=1:tlen;
    range1=2:11;
    con1{j}=mean(agepathEcon{j}(range1,range2),1)';
    conown1{j}=mean(agepathEconown{j}(range1,range2),1)';
    conrent1{j}=mean(agepathEconrent{j}(range1,range2),1)';
    own1{j}=mean(agepathEown{j}(range1,range2),1)';
    hnw1{j}=mean(agepathEhnw{j}(range1,range2),1)';
    ass1{j}=mean(agepathEass{j}(range1,range2),1)';
    lev1{j}=mean(agepathElev{j}(range1,range2),1)';
    inc1{j}=mean(agepathEinc{j}(range1,range2),1)';
    mort1{j}=mean(agepathEmort{j}(range1,range2),1)';
    house1{j}=mean(agepathEhouse{j}(range1,range2),1)';
    
    range1=6:16;
    conown2{j}=mean(agepathEconown{j}(range1,range2),1)';
    conrent2{j}=mean(agepathEconrent{j}(range1,range2),1)';

    con2{j}=mean(agepathEcon{j}(range1,range2),1)';
    own2{j}=mean(agepathEown{j}(range1,range2),1)';
    hnw2{j}=mean(agepathEhnw{j}(range1,range2),1)';
    ass2{j}=mean(agepathEass{j}(range1,range2),1)';
    lev2{j}=mean(agepathElev{j}(range1,range2),1)';
    inc2{j}=mean(agepathEinc{j}(range1,range2),1)';
    mort2{j}=mean(agepathEmort{j}(range1,range2),1)';
    house2{j}=mean(agepathEhouse{j}(range1,range2),1)';

    
    range1=23:Jtot;
    conown3{j}=mean(agepathEconown{j}(range1,range2),1)';
    conrent3{j}=mean(agepathEconrent{j}(range1,range2),1)';

    con3{j}=mean(agepathEcon{j}(range1,range2),1)';
    own3{j}=mean(agepathEown{j}(range1,range2),1)';
    hnw3{j}=mean(agepathEhnw{j}(range1,range2),1)';
    ass3{j}=mean(agepathEass{j}(range1,range2),1)';
    lev3{j}=mean(agepathElev{j}(range1,range2),1)';
    inc3{j}=mean(agepathEinc{j}(range1,range2),1)';
    mort3{j}=mean(agepathEmort{j}(range1,range2),1)';
    house3{j}=mean(agepathEhouse{j}(range1,range2),1)';

  
    conI{j}=con{j}(range_irf);
    hconI{j}=hcon{j}(range_irf);
    conownI{j}=conown{j}(range_irf);
    conrentI{j}=conrent{j}(range_irf);
    coninchighI{j}=coninchigh{j}(range_irf);
    coninclowI{j}=coninclow{j}(range_irf);
    conhighltvI{j}=conhighltv{j}(range_irf);
    conlowltvI{j}=conlowltv{j}(range_irf);
    dhI{j}=dh{j}(range_irf);
    dpI{j}=dp{j}(range_irf);
    ownI{j}=own{j}(range_irf);
    hnwI{j}=hnw{j}(range_irf);
    assI{j}=ass{j}(range_irf);
    incI{j}=inc{j}(range_irf);
    buyI{j}=buy{j}(range_irf);
    sellI{j}=sell{j}(range_irf);
    refiI{j}=refi{j}(range_irf);
    foreI{j}=fore{j}(range_irf);
    houseI{j}=house{j}(range_irf);
    mortI{j}=mort{j}(range_irf);
    levI{j}=lev{j}(range_irf);
    locI{j}=loc{j}(range_irf);
    locposI{j}=locpos{j}(range_irf);

      peak=6;
    PhI{j}=pathPh{j}(range_irf);
%     houseI{j}=houseI{j}./pathHt{j}(range_irf);
    MRSI{j}=PhI{j}*houseI{j}(peak)./(hnwI{j}(peak)+assI{j}(peak));
   
%      temp=own{j}(range_irf);
%     [xData, yData] = prepareCurveData( [], temp );
%     ft = fittype( 'smoothingspline' );
%     opts = fitoptions( 'Method', 'SmoothingSpline' );
%     opts.SmoothingParam = 0.768030683315926;
%     % Fit model to data.
%     [temp2, gof] = fit( xData, yData, ft, opts );
%     ownI{j}=temp2(1:20);
%     
%      temp=con{j}(range_irf);
%     [xData, yData] = prepareCurveData( [], temp );
%     ft = fittype( 'smoothingspline' );
%     opts = fitoptions( 'Method', 'SmoothingSpline' );
%     opts.SmoothingParam = 0.768030683315926;
%     % Fit model to data.
%     [temp2, gof] = fit( xData, yData, ft, opts );
%     conI{j}=temp2(1:20);
         MRSID{j}=MRSI{j}/MRSI{j}(1);

    conID{j}=conI{j}/conI{j}(1);
    hconID{j}=hconI{j}/hconI{j}(1);
    conownID{j}=conownI{j}/conownI{j}(1);
    conrentID{j}=conrentI{j}/conrentI{j}(1);
    coninchighID{j}=coninchighI{j}/coninchighI{j}(1);
    coninclowID{j}=coninclowI{j}/coninclowI{j}(1);
    conhighltvID{j}=conhighltvI{j}/conhighltvI{j}(1);
    conlowltvID{j}=conlowltvI{j}/conlowltvI{j}(1);
    dhID{j}=dhI{j}/conI{j}(1);
    dpID{j}=dpI{j}/conI{j}(1);
    ownID{j}=ownI{j}/ownI{j}(1);
    hnwID{j}=hnwI{j}/hnwI{j}(1);
    assID{j}=assI{j}/assI{j}(1);
    levID{j}=levI{j}/levI{j}(1);
    locID{j}=locI{j}/locI{j}(1);
    locposID{j}=locposI{j}/locposI{j}(1);
    incID{j}=incI{j}/incI{j}(1);
    buyID{j}=buyI{j}/buyI{j}(1);
    sellID{j}=sellI{j}/sellI{j}(1);
    refiID{j}=refiI{j}/refiI{j}(1);
    foreID{j}=foreI{j}/foreI{j}(1);
    mortID{j}=mortI{j}/mortI{j}(1);
    houseID{j}=houseI{j}/houseI{j}(1);

    
    conI1{j}=con1{j}(range_irf);
    conownI1{j}=conown1{j}(range_irf);
    conrentI1{j}=conrent1{j}(range_irf);
    ownI1{j}=own1{j}(range_irf);
    hnwI1{j}=hnw1{j}(range_irf);
    assI1{j}=ass1{j}(range_irf);
    levI1{j}=(mort1{j}./house1{j})';
    incI1{j}=inc1{j}(range_irf);

    conID1{j}=conI1{j}/conI1{j}(1);
    conownID1{j}=conownI1{j}/conownI1{j}(1);
    conrentID1{j}=conrentI1{j}/conrentI1{j}(1);
    ownID1{j}=ownI1{j}/ownI1{j}(1);
    hnwID1{j}=hnwI1{j}/hnwI1{j}(1);
    assID1{j}=assI1{j}/assI1{j}(1);
    levID1{j}=levI1{j}/levI1{j}(1);
    incID1{j}=incI1{j}/incI1{j}(1);
    
    conownI2{j}=conown2{j}(range_irf);
    conrentI2{j}=conrent2{j}(range_irf);
    
        conI2{j}=con2{j}(range_irf);
    ownI2{j}=own2{j}(range_irf);
    hnwI2{j}=hnw2{j}(range_irf);
    assI2{j}=ass2{j}(range_irf);
    levI2{j}=(mort2{j}./house2{j})';
    incI2{j}=inc2{j}(range_irf);

    conID2{j}=conI2{j}/conI2{j}(1);
        conownID2{j}=conownI2{j}/conownI2{j}(1);
    conrentID2{j}=conrentI2{j}/conrentI2{j}(1);

    ownID2{j}=ownI2{j}/ownI2{j}(1);
    hnwID2{j}=hnwI2{j}/hnwI2{j}(1);
    assID2{j}=assI2{j}/assI2{j}(1);
    levID2{j}=levI2{j}/levI2{j}(1);
    incID2{j}=incI2{j}/incI2{j}(1);
    
        conownI3{j}=conown3{j}(range_irf);
    conrentI3{j}=conrent3{j}(range_irf);

        conI3{j}=con3{j}(range_irf);
    ownI3{j}=own3{j}(range_irf);
    hnwI3{j}=hnw3{j}(range_irf);
    assI3{j}=ass3{j}(range_irf);
    levI3{j}=(mort3{j}./house3{j})';
    incI3{j}=inc3{j}(range_irf);

        conownID3{j}=conownI3{j}/conownI3{j}(1);
    conrentID3{j}=conrentI3{j}/conrentI3{j}(1);

    conID3{j}=conI3{j}/conI3{j}(1);
    ownID3{j}=ownI3{j}/ownI3{j}(1);
    hnwID3{j}=hnwI3{j}/hnwI3{j}(1);
    assID3{j}=assI3{j}/assI3{j}(1);
    levID3{j}=levI3{j}/levI3{j}(1);
    incID3{j}=incI3{j}/incI3{j}(1);
    
    Hinv{j}=pathHt{j}(2:end)-0.97*pathHt{j}(1:end-1);
    Hinv{j}(2:end-1)=Hinv{j}(1:end-2);

    PhID{j}=pathPh{j}(range_irf)/pathPh{j}(range_irf(1));
    PrID{j}=pathPr{j}(range_irf)/pathPr{j}(range_irf(1));
    PrPhID{j}=(pathPr{j}(range_irf)./pathPh{j}(range_irf))/(pathPr{j}(range_irf(1))/pathPh{j}(range_irf(1)));
    HinvID{j}=Hinv{j}(range_irf)/Hinv{j}(range_irf(1));
%     houseID{j}=ownID{j}
end