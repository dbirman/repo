%% Variable List from Netlogo:

% runcount : 
% variablenames_1
% variablepacket_1
% datapacket_energy_1
% datapacket_count_1

%% Re-organize variables for ANOVAN

V = []; %% variables, concatenated (1 ROW PER GROUP)
D = zeros(groups,runcount); %% data packets, concatenated (1 ROW PER GROUP)
for r = 1:runcount
    strcat(runcount, '-variablepacket');
    V = [V;eval(strcat('variablepacket_',r))];
    for g = 1:groups
        D(g,r) = mean(eval(strcat('datagroup_',g)));
    end
end

%% Prepare group variables
G = {};

for g = 1:groups
    G{end+1} = {D(g,:)};
end

%% run ANOVAS

