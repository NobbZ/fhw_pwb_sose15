
-type board()   :: gameboard:t().
-type move()    :: {non_neg_integer(), non_neg_integer()}.
-type score()   :: non_neg_integer().
-type history() :: {move(), score()}.
-type job()     :: read_board
                 | {progress, board(), move(), history()}.
