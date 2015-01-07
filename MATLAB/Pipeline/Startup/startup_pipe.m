%% Startup_Pipeline
% Sets up the background globals used to track the pipeline program.

global PIPE;
PIPE.path = 'C:\Users\Dan\Documents\MATLAB\Pipeline';

PIPE.cuser = getenv('USERNAME');

%% Load User Data

try
    PIPE = load(fullfile(PIPE.path,'Data',[PIPE.cuser '_data.mat']));
catch e
    sprintf('User data for %s does not exist. Generated new PIPE.',PIPE.cuser);
    save(fullfile(PIPE.path,'Data',[PIPE.cuser '_data.mat']),'PIPE');
end

%% End