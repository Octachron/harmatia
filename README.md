Harmatia is a for-fun example of compiler plugins for Ocaml.
Harmatia uplifts Ocaml error messages to the world of unicode:

* Digraph are replaced by their unicode equivalent
* Short type names are transliterated to the greek alphabet
* The characters `<` and `>` are reserved to relation orders and are replaced 
  by angle brackets `⟨` and `⟩` when they were used as brackets.
* `<hv>` boxes are replaced with `<hov>` boxes

|       Before      |    After    |
|-------------------|-------------|
|         ->        |      ⟶     |
|       [&#124;1&#124;]     |     ⟦1⟧    |
|        'a         |      α       |
|        ...        |      …       |
|         ..        |      ‥       |
| < f:'a.'a -> 'a > | ⟨f:α.α ⟶ α⟩ |

## How to use harmatia

Unfortunately, compiler plugins currently do no work with toplevel.

Otherwise, first install the harmatia opam package 

* clone this repository
* `opam pin add harmatia path_to_the_cloned path`

Once installed use the `-plugin` option of either `ocamlc` or `ocamlopt`

* `ocamlc -plugin $(ocamlfind query harmatia)/harmatia`

