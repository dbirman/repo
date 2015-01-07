% Set up variable
pps = []; ones = [];
mus = 1:.1:18.5;
for i = 1:length(mus)
    clear PperT SperT
    mu = mus(i);
    
    C = .3/18.5;
    S = mu/18.5;
    NS = (18.5-(mu+.3))/18.5;
    ones(i) = C+S+NS;
    U = -mu;
    
    PperT = C.*U+S.*mu;
    SperT = C.*(mu-.15)+S.*mu+NS.*mu./2;
    
    pps(i) = PperT./SperT;
    
end
figure(1)
plot(pps);
a = axis();
axis([0 18.5 a(3) a(4)])