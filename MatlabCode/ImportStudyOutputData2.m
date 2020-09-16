
% for j =1:size(type,2)
for jj =1:size(indices{1},2)
    j=indices{1}(jj);

    temp=importdata([InputBaseDir type{j} '/' 'agepathEass' DoExu '.txt']);
    agepathEass{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp=importdata([InputBaseDir type{j} '/' 'agepathEbuy' DoExu '.txt']);
    agepathEbuy{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEcon' DoExu '.txt']);
    agepathEcon{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEfore' DoExu '.txt']);
    agepathEfore{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEforem' DoExu '.txt']);
    agepathEforem{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEhouse' DoExu '.txt']);
    agepathEhouse{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEinc' DoExu '.txt']);
    agepathEinc{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathElev' DoExu '.txt']);
    agepathElev{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathElocpos' DoExu '.txt']);
    agepathElocpos{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEloc' DoExu '.txt']);
    agepathEloc{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEmlpos' DoExu '.txt']);
    agepathEmlpos{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEmort' DoExu '.txt']);
    agepathEmort{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEmortpos' DoExu '.txt']);
    agepathEmortpos{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEown' DoExu '.txt']);
    agepathEown{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEpen' DoExu '.txt']);
    agepathEpen{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathErefi' DoExu '.txt']);
    agepathErefi{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEsell' DoExu '.txt']);
    agepathEsell{j}= reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEhnw' DoExu '.txt']);
    agepathEhnw{j}= reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEhnwshare' DoExu '.txt']);
    agepathEhnwshare{j}= reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEdh.txt']);
    agepathEdh{j}= reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEdp.txt']);
    agepathEdp{j}= reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEconown' DoExu '.txt']);
    agepathEconown{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEconrent' DoExu '.txt']);
    agepathEconrent{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEconinclow' DoExu '.txt']);
    agepathEconinclow{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEconinchigh' DoExu '.txt']);
    agepathEconinchigh{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEconlowltv' DoExu '.txt']);
    agepathEconlowltv{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    temp = importdata([InputBaseDir type{j} '/' 'agepathEconhighltv' DoExu '.txt']);
    agepathEconhighltv{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);
    
    temp = importdata([InputBaseDir type{j} '/' 'agepathEhcon.txt']);
    agepathEhcon{j} = reshape(temp(1:Jtot*tlen),Jtot,tlen);


    pathPh{j} = importdata([InputBaseDir type{j} '/' 'pathPh' DoExu '.txt']);
    pathPr{j} = importdata([InputBaseDir type{j} '/' 'pathPr' DoExu '.txt']);
    pathHt{j} = importdata([InputBaseDir type{j} '/' 'pathHt' DoExu '.txt']);
    agepathEhnw{j} = repmat(pathPh{j}(1:tlen)',Jtot,1).*agepathEhouse{j}-agepathEmort{j};
    Vlogcon{j} = importdata([InputBaseDir type{j} '/' 'Vlogcon.txt']);
    Vloghcon{j} = importdata([InputBaseDir type{j} '/' 'Vloghcon.txt']);
    Vloginc{j} = importdata([InputBaseDir type{j} '/' 'Vloginc.txt']);
    Goldmine{j} = importdata([InputBaseDir type{j} '/' 'Goldmine.txt']);
    
    
end

%%

a0=importdata([InputBaseDir type{j} '/' 'a0' DoExu '.txt']);
a1=importdata([InputBaseDir type{j} '/' 'a1' DoExu '.txt']);
pathiAgg=importdata([InputBaseDir type{j} '/' 'iAggpath' DoExu '.txt']);
%%

forecastp2(99)=pathPh{j}(99);
for i=100:tlen-1
    forecastp(i)=exp(a0((pathiAgg(i-1)-1)*max(pathiAgg)+pathiAgg(i))+a1((pathiAgg(i-1)-1)*max(pathiAgg)+pathiAgg(i))*log(pathPh{1}(i-1)));
    forecastp2(i)=exp(a0((pathiAgg(i-1)-1)*max(pathiAgg)+pathiAgg(i))+a1((pathiAgg(i-1)-1)*max(pathiAgg)+pathiAgg(i))*log(forecastp2(i-1)));

end
