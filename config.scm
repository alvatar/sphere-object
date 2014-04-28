(sphere: "object")
(dependencies:
 (prototype
  (include (core: base-macros)
           (core: assert-macros))
  (load (core: assert)))
 (record
  (include (core: assert-macros))
  (load (core: assert)))
 (type-class-macros
  (include (core: match-macros))))
