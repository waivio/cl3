
module spec Cl3 where

import GHC.Float

assume abs :: x:{v : _ } -> y: {v : _}

assume pi :: x:{c: _}
assume exp :: x:{v: _ } -> y:{v: _}
assume log :: x:{v: _ | v > 0} -> y:{v: _}
assume sqrt :: x:{v: _ | v >= 0} -> y:{v: _}
assume (**) :: x:{v: _} -> y:{v: _} -> z:{v: _}
assume logBase :: x:{v: _} -> y:{v: _} -> z:{v: _}
assume sin :: x:{v: _} -> y:{v: _}
assume cos :: x:{v: _} -> y:{v: _}
assume tan :: x:{v: _} -> y:{v: _}
assume asin :: x:{v : _ | v <= 1 && v >= -1} -> y: {v : _}
assume acos :: x:{v: _ | v <= 1 && v >= -1} -> y:{v: _}
assume atan :: x:{v: _} -> y:{v: _}
assume sinh :: x:{v: _} -> y:{v: _}
assume cosh :: x:{v: _} -> y:{v: _}
assume tanh :: x:{v: _} -> y:{v: _}
assume asinh :: x:{v: _} -> y:{v: _}
assume acosh :: x:{v: _ | v <=1} -> y:{v: _}
assume atanh :: x:{v: _ | v <= 1 && v >= -1} -> y:{v: _}
