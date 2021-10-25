open Core

let type_directed_equal_int    = Int.equal
let type_directed_equal_string = String.equal

module M = struct
  type rec_6 =
    { f1 : string
    ; f2 : string
    ; f3 : string
    ; g1 : int
    ; g2 : int
    ; g3 : int
    }
  [@@deriving equal, type_directed_equal]

  type rec_16 =
    { f1 : string
    ; f2 : string
    ; f3 : string
    ; f4 : string
    ; f5 : string
    ; f6 : string
    ; f7 : string
    ; f8 : string
    ; g1 : int
    ; g2 : int
    ; g3 : int
    ; g4 : int
    ; g5 : int
    ; g6 : int
    ; g7 : int
    ; g8 : int
    }
  [@@deriving equal, type_directed_equal]

  type rec_30 =
    { f1  : string
    ; f2  : string
    ; f3  : string
    ; f4  : string
    ; f5  : string
    ; f6  : string
    ; f7  : string
    ; f8  : string
    ; f9  : string
    ; f10 : string
    ; f11 : string
    ; f12 : string
    ; f13 : string
    ; f14 : string
    ; f15 : string
    ; g1  : int
    ; g2  : int
    ; g3  : int
    ; g4  : int
    ; g5  : int
    ; g6  : int
    ; g7  : int
    ; g8  : int
    ; g9  : int
    ; g10 : int
    ; g11 : int
    ; g12 : int
    ; g13 : int
    ; g14 : int
    ; g15 : int
    }
  [@@deriving equal, type_directed_equal]

  type rec_60 =
    { f1  : string
    ; f2  : string
    ; f3  : string
    ; f4  : string
    ; f5  : string
    ; f6  : string
    ; f7  : string
    ; f8  : string
    ; f9  : string
    ; f10 : string
    ; f11 : string
    ; f12 : string
    ; f13 : string
    ; f14 : string
    ; f15 : string
    ; f16 : string
    ; f17 : string
    ; f18 : string
    ; f19 : string
    ; f20 : string
    ; f21 : string
    ; f22 : string
    ; f23 : string
    ; f24 : string
    ; f25 : string
    ; f26 : string
    ; f27 : string
    ; f28 : string
    ; f29 : string
    ; f30 : string
    ; g1  : int
    ; g2  : int
    ; g3  : int
    ; g4  : int
    ; g5  : int
    ; g6  : int
    ; g7  : int
    ; g8  : int
    ; g9  : int
    ; g10 : int
    ; g11 : int
    ; g12 : int
    ; g13 : int
    ; g14 : int
    ; g15 : int
    ; g16 : int
    ; g17 : int
    ; g18 : int
    ; g19 : int
    ; g20 : int
    ; g21 : int
    ; g22 : int
    ; g23 : int
    ; g24 : int
    ; g25 : int
    ; g26 : int
    ; g27 : int
    ; g28 : int
    ; g29 : int
    ; g30 : int
    }
  [@@deriving equal, type_directed_equal]
end

let rec_6 = { M.f1 = "hello"; f2 = "hello"; f3 = "hello"; g1 = 10; g2 = 10; g3 = 10 }

let rec_16 =
  { M.f1 = "hello"
  ; f2   = "hello"
  ; f3   = "hello"
  ; f4   = "hello"
  ; f5   = "hello"
  ; f6   = "hello"
  ; f7   = "hello"
  ; f8   = "hello"
  ; g1   = 10
  ; g2   = 10
  ; g3   = 10
  ; g4   = 10
  ; g5   = 10
  ; g6   = 10
  ; g7   = 10
  ; g8   = 10
  }
;;

let rec_30 =
  { M.f1 = "hello"
  ; f2   = "hello"
  ; f3   = "hello"
  ; f4   = "hello"
  ; f5   = "hello"
  ; f6   = "hello"
  ; f7   = "hello"
  ; f8   = "hello"
  ; f9   = "hello"
  ; f10  = "hello"
  ; f11  = "hello"
  ; f12  = "hello"
  ; f13  = "hello"
  ; f14  = "hello"
  ; f15  = "hello"
  ; g1   = 10
  ; g2   = 10
  ; g3   = 10
  ; g4   = 10
  ; g5   = 10
  ; g6   = 10
  ; g7   = 10
  ; g8   = 10
  ; g9   = 10
  ; g10  = 10
  ; g11  = 10
  ; g12  = 10
  ; g13  = 10
  ; g14  = 10
  ; g15  = 10
  }
;;

let rec_60 =
  { M.f1 = "hello"
  ; f2   = "hello"
  ; f3   = "hello"
  ; f4   = "hello"
  ; f5   = "hello"
  ; f6   = "hello"
  ; f7   = "hello"
  ; f8   = "hello"
  ; f9   = "hello"
  ; f10  = "hello"
  ; f11  = "hello"
  ; f12  = "hello"
  ; f13  = "hello"
  ; f14  = "hello"
  ; f15  = "hello"
  ; f16  = "hello"
  ; f17  = "hello"
  ; f18  = "hello"
  ; f19  = "hello"
  ; f20  = "hello"
  ; f21  = "hello"
  ; f22  = "hello"
  ; f23  = "hello"
  ; f24  = "hello"
  ; f25  = "hello"
  ; f26  = "hello"
  ; f27  = "hello"
  ; f28  = "hello"
  ; f29  = "hello"
  ; f30  = "hello"
  ; g1   = 10
  ; g2   = 10
  ; g3   = 10
  ; g4   = 10
  ; g5   = 10
  ; g6   = 10
  ; g7   = 10
  ; g8   = 10
  ; g9   = 10
  ; g10  = 10
  ; g11  = 10
  ; g12  = 10
  ; g13  = 10
  ; g14  = 10
  ; g15  = 10
  ; g16  = 10
  ; g17  = 10
  ; g18  = 10
  ; g19  = 10
  ; g20  = 10
  ; g21  = 10
  ; g22  = 10
  ; g23  = 10
  ; g24  = 10
  ; g25  = 10
  ; g26  = 10
  ; g27  = 10
  ; g28  = 10
  ; g29  = 10
  ; g30  = 10
  }
;;

let%bench "equal_equal_6" = M.equal_rec_6 rec_6 { rec_6 with g3 = 100 }

let%bench "equal_type_directed_6" =
  M.type_directed_equal_rec_6 rec_6 { rec_6 with g3 = 100 }
;;

let%bench "equal_equal_16" = M.equal_rec_16 rec_16 { rec_16 with g8 = 100 }

let%bench "equal_type_directed_16" =
  M.type_directed_equal_rec_16 rec_16 { rec_16 with g8 = 100 }
;;

let%bench "equal_equal_30" = M.equal_rec_30 rec_30 { rec_30 with g15 = 100 }

let%bench "equal_type_directed_30" =
  M.type_directed_equal_rec_30 rec_30 { rec_30 with g15 = 100 }
;;

let%bench "equal_equal_60" = M.equal_rec_60 rec_60 { rec_60 with g30 = 100 }

let%bench "equal_type_directed_60" =
  M.type_directed_equal_rec_60 rec_60 { rec_60 with g30 = 100 }
;;
