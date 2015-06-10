-module (vector).

-export ([]).
-export_types ([t/0, t/1]).

-type t()  :: t(any()).
-type t(A) :: [A].