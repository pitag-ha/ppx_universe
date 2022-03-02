open! Import

include Identifiable.Extend
    (Base.Nativeint)
    (struct
      type t = nativeint [@@deriving bin_io]
    end)

include Base.Nativeint

type t = nativeint [@@deriving typerep]

module Hex = struct
  include Hex

  type nonrec t = t [@@deriving typerep, bin_io]
end

let quickcheck_generator = Base_quickcheck.Generator.nativeint
let quickcheck_observer = Base_quickcheck.Observer.nativeint
let quickcheck_shrinker = Base_quickcheck.Shrinker.nativeint
let gen_incl = Base_quickcheck.Generator.nativeint_inclusive
let gen_uniform_incl = Base_quickcheck.Generator.nativeint_uniform_inclusive
let gen_log_incl = Base_quickcheck.Generator.nativeint_log_inclusive
let gen_log_uniform_incl = Base_quickcheck.Generator.nativeint_log_uniform_inclusive
