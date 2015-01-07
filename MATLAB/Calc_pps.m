c = pps{1}*1000;
p1 = pps{2}*1000;
p2 = pps{3}*1000;
%%
figure
subplot(131)
plot(c,'*')
axis([0 10 -.1 1])
subplot(132)
plot(p1,'*')
axis([0 10 -.1 1])
subplot(133)
plot(p2,'*')
axis([0 10 -.1 1])

%%
[h,p,ci,stats] = ttest(c);
%%
[h,p,ci,stats] = ttest(p1);
%%
[h,p,ci,stats] = ttest(p2);