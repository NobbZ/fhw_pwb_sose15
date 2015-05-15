-module (vector).

-export ([]).
-export_type ([t/0, t/1]).

-type t()  :: t(any()).
-type t(A) :: [A].