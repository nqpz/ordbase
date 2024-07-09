# sto-handler

Handles STO data.

Run:

```sh
stack build
stack exec -- sto-handler generateMorphologyProlog > morphology.pl
stack exec -- sto-handler generateSyntaxProlog > syntax.pl
swipl --stand_alone=true -o sto -c morphology.pl syntax.pl
./sto
```

Things to try out (TODO: check if all of these still work):

```prolog
att(definiteness, definite, skib_1, S), att(grammatical_number, plural, skib_1, S), att(case, genitive_case, skib_1, S).
S = "skibenes".

findall(Id, att(_, _, Id, "hest"), L), sort(L, L1).

word_ids_and_types("hoppe", Ids).

att(case, genitive_case, _, Word).

noun_group(D, ["statsstøtte", "på", P, "til", T]).
noun_group(D, ["husstandens", "forelæggelse", "af", A, "for", F]).
noun_group(D, [G, "forelæggelse", "af", A, "for", F]).
noun_group(D, L).

invalid: noun_group(D, [G, "forelæggelsen", "af", A, "for", F]).

adjective_group(Degree, GramNum, GramGen, ["huset"|S]).
adjective_group(Degree, GramNum, GramGen, L).
```

## Unfinished output kinds

```sh
grep -Eo '"FIXME[^"]*"' morphology.pl syntax.pl | sort | uniq
```
