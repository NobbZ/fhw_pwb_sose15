
%% -type board()   :: gameboard:t().
%% -type move()    :: {non_neg_integer(), non_neg_integer()}.
%% -type score()   :: non_neg_integer().
%% -type history() :: {move(), score()}.
%% -type job()     :: read_board
%%                  | {progress, board(), move(), history()}.

-type click() :: {non_neg_integer(), non_neg_integer()}.

-record(job, {potential  = 0                   :: non_neg_integer(),
              board      = matrix:new(1, 1, 0) :: matrix:t(),
              click      = {0, 0}              :: click(),
              history    = []                  :: [click()],
              lastscore  = 0                   :: non_neg_integer(),
              whitespace = 0                   :: non_neg_integer()}).
