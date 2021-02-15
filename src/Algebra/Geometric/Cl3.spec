
module spec Cl3 where

import GHC.Float

assume abs :: x:{v : _ } -> y: {v : gteqZero }

assume pi :: x:{v: _ | v = pi}
assume exp :: x:{v: _ } -> y:{v: _ | v > 0}
assume log :: x:{v: _ | v > 0} -> y:{v: _}
assume sqrt :: x:{v: _ | v >= 0} -> y:{v: _ | v >= 0}
assume (**) :: x:{v: _} -> y:{v: _} -> z:{v: _}
assume logBase :: x:{v: _} -> y:{v: _} -> z:{v: _}
assume sin :: x:{v: _} -> y:{v: -1 <= v <= 1}
assume cos :: x:{v: _} -> y:{v: -1 <= v <= 1}
assume tan :: x:{v: _ | v/pi + 1/2 with v not an Int} -> y:{v: _}
assume asin :: x:{v : _ | -1 <= v <= 1} -> y: {v : -pi/2 <= v <= pi/2}
assume acos :: x:{v: _ | -1 <= v <= 1} -> y:{v: 0 <= v <= pi}
assume atan :: x:{v: _} -> y:{v: _ | -pi/2 <= v <= pi/2}
assume sinh :: x:{v: _} -> y:{v: _}
assume cosh :: x:{v: _} -> y:{v: _}
assume tanh :: x:{v: _} -> y:{v: _}
assume asinh :: x:{v: _} -> y:{v: _}
assume acosh :: x:{v: _} -> y:{v: _}
assume atanh :: x:{v: _} -> y:{v: _}
