%%

g1 = X(:,1);
g2 = X(:,2);
% g3 = X(:,3);

d1 = X(:,3);

%% D1 = FORAGERS, D2 = ENERGY

[p,t,stats] = anovan(d1,{g1 g2});

%%

multcompare(stats,'dimension',[1 2])